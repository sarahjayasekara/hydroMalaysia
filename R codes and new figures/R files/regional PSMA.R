# ============================================================
# REGIONAL PRECIPITATION–SOIL-MOISTURE ASYNCHRONY IN MALAYSIA
# Period: 1982–2011
#
# Panel A:
# Best antecedent precipitation lag (0–6 months)
# based on deseasonalized monthly anomalies.
#
# Panel B:
# Same-month correlation between the 12-month climatological
# seasonal cycles of precipitation and total soil moisture.
# ============================================================


# ------------------------------------------------------------
# 0. INSTALL PACKAGES — RUN ONCE IF NEEDED
# ------------------------------------------------------------

# install.packages("tidyverse")
install.packages("lubridate")
install.packages("patchwork")
install.packages("viridis")
install.packages("sf")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")


# ------------------------------------------------------------
# 1. LOAD PACKAGES
# ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)
library(viridis)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# ------------------------------------------------------------
# 2. FILE PATH
# ------------------------------------------------------------

file_path <- paste0(
  "C:/Users/MSI/OneDrive/Desktop/MARC/",
  "R codes and plots/datasets/",
  "malaysia_hydrology_monthly_1982_2011.csv"
)


# ------------------------------------------------------------
# 3. READ DATA
# ------------------------------------------------------------

hydro <- read_csv(
  file_path,
  show_col_types = FALSE
)


# ------------------------------------------------------------
# 4. CHECK REQUIRED COLUMNS
# ------------------------------------------------------------

required_cols <- c(
  "grid_id",
  "longitude",
  "latitude",
  "year",
  "month",
  "precipitation_mm",
  "soil_moisture_total_mm"
)

missing_cols <- setdiff(
  required_cols,
  names(hydro)
)

if (length(missing_cols) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}


# ------------------------------------------------------------
# 5. CLEAN DATA
# ------------------------------------------------------------

hydro_clean <- hydro %>%
  transmute(
    grid_id = as.character(grid_id),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    year = as.integer(year),
    month = as.integer(month),
    precipitation = as.numeric(precipitation_mm),
    soil_moisture = as.numeric(soil_moisture_total_mm)
  ) %>%
  filter(
    year >= 1982,
    year <= 2011,
    month >= 1,
    month <= 12,
    !is.na(grid_id),
    !is.na(longitude),
    !is.na(latitude)
  ) %>%
  mutate(
    date_month = make_date(
      year = year,
      month = month,
      day = 1
    )
  )


# ------------------------------------------------------------
# 6. CHECK FOR DUPLICATE GRID CELL × MONTH OBSERVATIONS
# ------------------------------------------------------------

duplicates <- hydro_clean %>%
  count(
    grid_id,
    date_month
  ) %>%
  filter(n > 1)

if (nrow(duplicates) > 0) {
  
  warning(
    "Duplicate grid_id/month observations were detected. ",
    "They will be averaged before analysis."
  )
  
  hydro_clean <- hydro_clean %>%
    group_by(
      grid_id,
      longitude,
      latitude,
      date_month
    ) %>%
    summarise(
      precipitation = mean(
        precipitation,
        na.rm = TRUE
      ),
      soil_moisture = mean(
        soil_moisture,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    mutate(
      year = year(date_month),
      month = month(date_month)
    )
}


# ------------------------------------------------------------
# 7. STORE GRID CELL COORDINATES
# ------------------------------------------------------------

grid_coordinates <- hydro_clean %>%
  distinct(
    grid_id,
    longitude,
    latitude
  )


# ------------------------------------------------------------
# 8. CREATE COMPLETE MONTHLY SEQUENCE
# ------------------------------------------------------------
#
# This is IMPORTANT.
#
# Every grid cell should have exactly:
#
# 30 years × 12 months = 360 rows
#
# Missing months are explicitly inserted as NA.
#
# Therefore:
#
# lag(x, 1)
#
# ALWAYS means exactly one calendar month earlier.
# ------------------------------------------------------------

all_months <- seq(
  from = as.Date("1982-01-01"),
  to   = as.Date("2011-12-01"),
  by   = "month"
)


hydro_complete <- grid_coordinates %>%
  
  select(
    grid_id,
    longitude,
    latitude
  ) %>%
  
  crossing(
    date_month = all_months
  ) %>%
  
  left_join(
    hydro_clean %>%
      select(
        grid_id,
        date_month,
        precipitation,
        soil_moisture
      ),
    by = c(
      "grid_id",
      "date_month"
    )
  ) %>%
  
  mutate(
    year = year(date_month),
    month = month(date_month)
  ) %>%
  
  arrange(
    grid_id,
    date_month
  )


# ------------------------------------------------------------
# 9. CHECK COMPLETENESS
# ------------------------------------------------------------

sequence_check <- hydro_complete %>%
  count(grid_id)

if (!all(sequence_check$n == 360)) {
  stop(
    "Monthly completion failed: not every grid cell has 360 rows."
  )
}

cat(
  "\nMonthly sequence successfully completed.",
  "\nEach grid cell contains 360 calendar months.\n"
)


# ============================================================
# PART A
# DESEASONALIZED PRECIPITATION / SOIL-MOISTURE ANOMALIES
# ============================================================


# ------------------------------------------------------------
# 10. CALCULATE MONTHLY CLIMATOLOGICAL MEANS
# ------------------------------------------------------------

climatology <- hydro_complete %>%
  
  group_by(
    grid_id,
    longitude,
    latitude,
    month
  ) %>%
  
  summarise(
    
    precip_clim = ifelse(
      all(is.na(precipitation)),
      NA_real_,
      mean(
        precipitation,
        na.rm = TRUE
      )
    ),
    
    soil_clim = ifelse(
      all(is.na(soil_moisture)),
      NA_real_,
      mean(
        soil_moisture,
        na.rm = TRUE
      )
    ),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 11. CALCULATE MONTHLY ANOMALIES
# ------------------------------------------------------------

anomaly_data <- hydro_complete %>%
  
  left_join(
    climatology %>%
      select(
        grid_id,
        month,
        precip_clim,
        soil_clim
      ),
    by = c(
      "grid_id",
      "month"
    )
  ) %>%
  
  mutate(
    
    precip_anomaly =
      precipitation - precip_clim,
    
    soil_anomaly =
      soil_moisture - soil_clim
    
  ) %>%
  
  arrange(
    grid_id,
    date_month
  )


# ------------------------------------------------------------
# 12. CREATE PRECIPITATION LAGS 0–6 MONTHS
# ------------------------------------------------------------

lagged_data <- anomaly_data %>%
  
  group_by(grid_id) %>%
  
  arrange(
    date_month,
    .by_group = TRUE
  ) %>%
  
  mutate(
    
    lag_0 = precip_anomaly,
    
    lag_1 = lag(
      precip_anomaly,
      n = 1
    ),
    
    lag_2 = lag(
      precip_anomaly,
      n = 2
    ),
    
    lag_3 = lag(
      precip_anomaly,
      n = 3
    ),
    
    lag_4 = lag(
      precip_anomaly,
      n = 4
    ),
    
    lag_5 = lag(
      precip_anomaly,
      n = 5
    ),
    
    lag_6 = lag(
      precip_anomaly,
      n = 6
    )
    
  ) %>%
  
  ungroup()


# ------------------------------------------------------------
# 13. CONVERT LAGS TO LONG FORMAT
# ------------------------------------------------------------

lag_long <- lagged_data %>%
  
  select(
    grid_id,
    longitude,
    latitude,
    date_month,
    soil_anomaly,
    starts_with("lag_")
  ) %>%
  
  pivot_longer(
    
    cols = starts_with("lag_"),
    
    names_to = "lag",
    
    values_to = "precip_lag_anomaly"
    
  ) %>%
  
  mutate(
    
    lag = as.integer(
      str_remove(
        lag,
        "lag_"
      )
    )
    
  )


# ------------------------------------------------------------
# 14. SAFE CORRELATION FUNCTION
# ------------------------------------------------------------

safe_cor <- function(x, y, min_pairs = 24) {
  
  ok <- complete.cases(x, y)
  
  n <- sum(ok)
  
  if (n < min_pairs) {
    return(NA_real_)
  }
  
  x_ok <- x[ok]
  y_ok <- y[ok]
  
  # Pearson correlation is undefined
  # if either variable has zero variance.
  if (
    sd(x_ok) == 0 ||
    sd(y_ok) == 0
  ) {
    return(NA_real_)
  }
  
  cor(
    x_ok,
    y_ok,
    method = "pearson"
  )
}


# ------------------------------------------------------------
# 15. CALCULATE CORRELATION FOR EACH LAG
# ------------------------------------------------------------

lag_correlations <- lag_long %>%
  
  group_by(
    grid_id,
    longitude,
    latitude,
    lag
  ) %>%
  
  summarise(
    
    n_pairs = sum(
      complete.cases(
        precip_lag_anomaly,
        soil_anomaly
      )
    ),
    
    correlation = safe_cor(
      precip_lag_anomaly,
      soil_anomaly,
      min_pairs = 24
    ),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 16. SELECT BEST POSITIVE LAG
# ------------------------------------------------------------
#
# IMPORTANT:
#
# We select the LARGEST POSITIVE correlation.
#
# We are NOT selecting:
#
# max(abs(correlation))
#
# because that would treat strong negative correlations
# as the "best" precipitation response.
# ------------------------------------------------------------

best_lag <- lag_correlations %>%
  
  filter(
    !is.na(correlation),
    correlation > 0
  ) %>%
  
  group_by(
    grid_id,
    longitude,
    latitude
  ) %>%
  
  arrange(
    desc(correlation),
    lag
  ) %>%
  
  slice(1) %>%
  
  ungroup() %>%
  
  transmute(
    
    grid_id,
    longitude,
    latitude,
    
    best_lag = lag,
    
    best_correlation = correlation,
    
    n_pairs
  )


# ------------------------------------------------------------
# 17. ADD CELLS WITH NO VALID POSITIVE CORRELATION
# ------------------------------------------------------------

best_lag <- grid_coordinates %>%
  
  left_join(
    best_lag,
    by = c(
      "grid_id",
      "longitude",
      "latitude"
    )
  )


# ============================================================
# PART B
# SAME-MONTH SEASONAL CORRELATION
# ============================================================


# ------------------------------------------------------------
# 18. CALCULATE SEASONAL CORRELATION
# ------------------------------------------------------------
#
# Here each grid cell contributes up to 12 pairs:
#
# Jan precipitation climatology <-> Jan soil climatology
# Feb precipitation climatology <-> Feb soil climatology
# ...
# Dec precipitation climatology <-> Dec soil climatology
# ------------------------------------------------------------

seasonal_correlation <- climatology %>%
  
  group_by(
    grid_id,
    longitude,
    latitude
  ) %>%
  
  summarise(
    
    n_months = sum(
      complete.cases(
        precip_clim,
        soil_clim
      )
    ),
    
    seasonal_correlation = {
      
      ok <- complete.cases(
        precip_clim,
        soil_clim
      )
      
      # Require at least 8 of the 12 calendar months
      if (sum(ok) < 8) {
        
        NA_real_
        
      } else if (
        sd(precip_clim[ok]) == 0 ||
        sd(soil_clim[ok]) == 0
      ) {
        
        NA_real_
        
      } else {
        
        cor(
          precip_clim[ok],
          soil_clim[ok],
          method = "pearson"
        )
        
      }
    },
    
    .groups = "drop"
  )


# ============================================================
# GEOGRAPHIC INFORMATION
# ============================================================


# ------------------------------------------------------------
# 19. DETERMINE GRID SPACING
# ------------------------------------------------------------

lon_values <- sort(
  unique(grid_coordinates$longitude)
)

lat_values <- sort(
  unique(grid_coordinates$latitude)
)


lon_diff <- diff(lon_values)
lat_diff <- diff(lat_values)


lon_step <- min(
  lon_diff[lon_diff > 0],
  na.rm = TRUE
)

lat_step <- min(
  lat_diff[lat_diff > 0],
  na.rm = TRUE
)


# Fallback
if (!is.finite(lon_step)) {
  lon_step <- 0.5
}

if (!is.finite(lat_step)) {
  lat_step <- 0.5
}


cat(
  "\nDetected grid spacing:",
  "\nLongitude:", lon_step,
  "\nLatitude:", lat_step,
  "\n"
)


# ------------------------------------------------------------
# 20. GRID CELL WIDTH / HEIGHT
# ------------------------------------------------------------
#
# 95% of spacing leaves a tiny visual gap.
# ------------------------------------------------------------

cell_width <- lon_step * 0.95
cell_height <- lat_step * 0.95


# ------------------------------------------------------------
# 21. CREATE RECTANGLE COORDINATES
# ------------------------------------------------------------

best_lag_plot <- best_lag %>%
  
  mutate(
    
    xmin = longitude - cell_width / 2,
    xmax = longitude + cell_width / 2,
    
    ymin = latitude - cell_height / 2,
    ymax = latitude + cell_height / 2,
    
    lag_label = ifelse(
      is.na(best_lag),
      "",
      as.character(best_lag)
    )
    
  )


seasonal_plot <- seasonal_correlation %>%
  
  mutate(
    
    xmin = longitude - cell_width / 2,
    xmax = longitude + cell_width / 2,
    
    ymin = latitude - cell_height / 2,
    ymax = latitude + cell_height / 2,
    
    correlation_label = ifelse(
      is.na(seasonal_correlation),
      "",
      sprintf(
        "%.2f",
        seasonal_correlation
      )
    )
    
  )


# ------------------------------------------------------------
# 22. GET MALAYSIA COUNTRY OUTLINE
# ------------------------------------------------------------

malaysia <- ne_countries(
  scale = "medium",
  country = "Malaysia",
  returnclass = "sf"
)

malaysia <- st_transform(
  malaysia,
  crs = 4326
)


# ------------------------------------------------------------
# 23. COMMON MAP EXTENT
# ------------------------------------------------------------

malaysia_bbox <- st_bbox(malaysia)


x_min <- min(
  grid_coordinates$longitude - cell_width / 2,
  malaysia_bbox["xmin"],
  na.rm = TRUE
)

x_max <- max(
  grid_coordinates$longitude + cell_width / 2,
  malaysia_bbox["xmax"],
  na.rm = TRUE
)

y_min <- min(
  grid_coordinates$latitude - cell_height / 2,
  malaysia_bbox["ymin"],
  na.rm = TRUE
)

y_max <- max(
  grid_coordinates$latitude + cell_height / 2,
  malaysia_bbox["ymax"],
  na.rm = TRUE
)


# Small margin
x_pad <- 0.25
y_pad <- 0.25


# ============================================================
# PLOTTING
# ============================================================


# ------------------------------------------------------------
# 24. COMMON MAP THEME
# ------------------------------------------------------------

map_theme <- theme_minimal(
  base_size = 11
) +
  
  theme(
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    panel.grid.major = element_line(
      colour = "grey93",
      linewidth = 0.25
    ),
    
    panel.grid.minor = element_blank(),
    
    axis.title = element_text(
      size = 11
    ),
    
    axis.text = element_text(
      size = 9,
      colour = "black"
    ),
    
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5,
      margin = margin(
        b = 8
      )
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      size = 10
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    legend.key.width = unit(
      1.3,
      "cm"
    )
    
  )


# ------------------------------------------------------------
# 25. PANEL A
# BEST ANTECEDENT PRECIPITATION LAG
# ------------------------------------------------------------

panel_A <- ggplot() +
  
  # Malaysia outline behind grid
  geom_sf(
    data = malaysia,
    fill = "grey98",
    colour = "grey45",
    linewidth = 0.35,
    inherit.aes = FALSE
  ) +
  
  # Grid cells
  geom_rect(
    data = best_lag_plot,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = factor(
        best_lag,
        levels = 0:6
      )
    ),
    colour = "grey20",
    linewidth = 0.25
  ) +
  
  # Lag number
  geom_text(
    data = best_lag_plot %>%
      filter(!is.na(best_lag)),
    aes(
      x = longitude,
      y = latitude,
      label = lag_label
    ),
    size = 2.4,
    colour = "black"
  ) +
  
  scale_fill_viridis_d(
    option = "viridis",
    direction = 1,
    drop = FALSE,
    na.value = "grey90",
    name = "Antecedent precipitation lag (months)",
    guide = guide_legend(
      nrow = 1,
      title.position = "bottom",
      label.position = "bottom"
    )
  ) +
  
  coord_sf(
    xlim = c(
      x_min - x_pad,
      x_max + x_pad
    ),
    ylim = c(
      y_min - y_pad,
      y_max + y_pad
    ),
    expand = FALSE,
    datum = NA
  ) +
  
  labs(
    title =
      "Best antecedent precipitation lag\nfor monthly soil-moisture anomalies",
    x = "Longitude (°E)",
    y = "Latitude (°N)"
  ) +
  
  map_theme


# ------------------------------------------------------------
# 26. PANEL B
# SEASONAL CORRELATION
# ------------------------------------------------------------

panel_B <- ggplot() +
  
  # Malaysia outline
  geom_sf(
    data = malaysia,
    fill = "grey98",
    colour = "grey45",
    linewidth = 0.35,
    inherit.aes = FALSE
  ) +
  
  # Grid cells
  geom_rect(
    data = seasonal_plot,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = seasonal_correlation
    ),
    colour = "grey20",
    linewidth = 0.25
  ) +
  
  # Correlation labels
  geom_text(
    data = seasonal_plot %>%
      filter(
        !is.na(seasonal_correlation)
      ),
    aes(
      x = longitude,
      y = latitude,
      label = correlation_label
    ),
    size = 1.9,
    colour = "black"
  ) +
  
  scale_fill_gradient2(
    low = "#3B5CCC",
    mid = "white",
    high = "#D73027",
    midpoint = 0,
    limits = c(-1, 1),
    breaks = c(
      -1,
      -0.75,
      -0.50,
      -0.25,
      0,
      0.25,
      0.50,
      0.75,
      1
    ),
    name = "Pearson correlation",
    na.value = "grey90",
    guide = guide_colourbar(
      title.position = "bottom",
      barwidth = unit(10, "cm"),
      barheight = unit(0.45, "cm")
    )
  ) +
  
  coord_sf(
    xlim = c(
      x_min - x_pad,
      x_max + x_pad
    ),
    ylim = c(
      y_min - y_pad,
      y_max + y_pad
    ),
    expand = FALSE,
    datum = NA
  ) +
  
  labs(
    title =
      "Same-month seasonal correlation\nbetween precipitation and soil moisture",
    x = "Longitude (°E)",
    y = "Latitude (°N)"
  ) +
  
  map_theme


# ============================================================
# SUMMARY STATISTICS
# ============================================================


# ------------------------------------------------------------
# 27. NUMBER OF GRID CELLS
# ------------------------------------------------------------

n_grid_cells <- nrow(
  grid_coordinates
)

n_valid_lag <- sum(
  !is.na(best_lag$best_lag)
)

n_excluded_lag <- sum(
  is.na(best_lag$best_lag)
)

n_valid_seasonal <- sum(
  !is.na(
    seasonal_correlation$seasonal_correlation
  )
)

n_excluded_seasonal <- sum(
  is.na(
    seasonal_correlation$seasonal_correlation
  )
)


cat(
  "\n============================================\n",
  "ANALYSIS SUMMARY\n",
  "============================================\n"
)

cat(
  "\nTotal grid cells:",
  n_grid_cells,
  "\n"
)

cat(
  "Grid cells with valid best lag:",
  n_valid_lag,
  "\n"
)

cat(
  "Grid cells excluded from lag analysis:",
  n_excluded_lag,
  "\n"
)

cat(
  "Grid cells with valid seasonal correlation:",
  n_valid_seasonal,
  "\n"
)

cat(
  "Grid cells excluded from seasonal analysis:",
  n_excluded_seasonal,
  "\n"
)


# ------------------------------------------------------------
# 28. DISTRIBUTION OF BEST LAGS
# ------------------------------------------------------------

lag_distribution <- best_lag %>%
  
  filter(
    !is.na(best_lag)
  ) %>%
  
  count(
    best_lag,
    name = "number_of_grid_cells"
  ) %>%
  
  complete(
    best_lag = 0:6,
    fill = list(
      number_of_grid_cells = 0
    )
  ) %>%
  
  arrange(best_lag)


cat(
  "\nDistribution of best antecedent lags:\n"
)

print(
  lag_distribution
)


# ------------------------------------------------------------
# 29. SEASONAL CORRELATION SUMMARY
# ------------------------------------------------------------

seasonal_summary <- seasonal_correlation %>%
  
  summarise(
    
    mean_correlation = mean(
      seasonal_correlation,
      na.rm = TRUE
    ),
    
    minimum_correlation = min(
      seasonal_correlation,
      na.rm = TRUE
    ),
    
    maximum_correlation = max(
      seasonal_correlation,
      na.rm = TRUE
    )
    
  )


cat(
  "\nSeasonal correlation summary:\n"
)

print(
  seasonal_summary
)


# ------------------------------------------------------------
# 30. OPTIONAL:
# BEST-LAG CORRELATION SUMMARY
# ------------------------------------------------------------

best_lag_correlation_summary <- best_lag %>%
  
  summarise(
    
    mean_best_correlation = mean(
      best_correlation,
      na.rm = TRUE
    ),
    
    min_best_correlation = min(
      best_correlation,
      na.rm = TRUE
    ),
    
    max_best_correlation = max(
      best_correlation,
      na.rm = TRUE
    )
    
  )


cat(
  "\nBest-lag correlation summary:\n"
)

print(
  best_lag_correlation_summary
)


# ============================================================
# COMBINE FIGURE
# ============================================================


# ------------------------------------------------------------
# 31. COMBINE PANELS
# ------------------------------------------------------------

combined_plot <- panel_A + panel_B +
  
  plot_layout(
    ncol = 2,
    guides = "keep"
  ) +
  
  plot_annotation(
    
    title =
      "Regional precipitation–soil-moisture asynchrony in Malaysia",
    
    theme = theme(
      
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0.5,
        margin = margin(
          b = 15
        )
      )
      
    )
  )


# ------------------------------------------------------------
# 32. DISPLAY FINAL FIGURE
# ------------------------------------------------------------

print(
  combined_plot
)


# ============================================================
# SAVE FIGURE
# ============================================================


# ------------------------------------------------------------
# 33. OUTPUT PATH
# ------------------------------------------------------------

output_file <- paste0(
  "C:/Users/MSI/OneDrive/Desktop/MARC/",
  "R codes and plots/",
  "Regional_precipitation_soil_moisture_asynchrony.png"
)


# ------------------------------------------------------------
# 34. SAVE HIGH-RESOLUTION PNG
# ------------------------------------------------------------

ggsave(
  
  filename = output_file,
  
  plot = combined_plot,
  
  width = 18,
  
  height = 10,
  
  units = "in",
  
  dpi = 400,
  
  bg = "white"
  
)


cat(
  "\n============================================\n",
  "Plot successfully saved to:\n",
  output_file,
  "\n============================================\n"
)