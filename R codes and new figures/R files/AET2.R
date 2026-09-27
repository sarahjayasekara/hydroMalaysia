
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)


# ------------------------------------------------------------
# 2. FILE PATH
# ------------------------------------------------------------

file_path <- paste0(
  "C:/Users/MSI/OneDrive/Desktop/MARC/",
  "R codes and plots/datasets/",
  "malaysia_hydrology_monthly_1982_2011.csv"
)


# ------------------------------------------------------------
# 3. LOAD DATA
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
  "actual_evapotranspiration_mm"
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
    grid_id = grid_id,
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    year = as.integer(year),
    month = as.integer(month),
    AET = as.numeric(actual_evapotranspiration_mm)
  ) %>%
  filter(
    year >= 1982,
    year <= 2011,
    month >= 1,
    month <= 12,
    !is.na(longitude),
    !is.na(latitude),
    !is.na(AET)
  )


# ------------------------------------------------------------
# 6. MONTHLY CLIMATOLOGY FOR EACH GRID CELL
# ------------------------------------------------------------

aet_clim <- hydro_clean %>%
  group_by(
    grid_id,
    longitude,
    latitude,
    month
  ) %>%
  summarise(
    
    # Mean AET
    mean_AET = mean(
      AET,
      na.rm = TRUE
    ),
    
    # 25th percentile
    Q1 = quantile(
      AET,
      probs = 0.25,
      na.rm = TRUE,
      names = FALSE
    ),
    
    # 75th percentile
    Q3 = quantile(
      AET,
      probs = 0.75,
      na.rm = TRUE,
      names = FALSE
    ),
    
    # Number of years available
    n_years = sum(!is.na(AET)),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 7. GET MALAYSIA MAP
# ------------------------------------------------------------

malaysia <- ne_countries(
  scale = "medium",
  country = "Malaysia",
  returnclass = "sf"
)

# Make sure it uses geographic longitude/latitude coordinates
malaysia <- st_transform(
  malaysia,
  crs = 4326
)


# ------------------------------------------------------------
# 8. DETERMINE HYDROLOGICAL GRID SPACING
# ------------------------------------------------------------

lon_values <- sort(
  unique(aet_clim$longitude)
)

lat_values <- sort(
  unique(aet_clim$latitude)
)


# Difference between neighbouring coordinates
lon_diffs <- diff(lon_values)
lat_diffs <- diff(lat_values)


# Use smallest meaningful spacing.
# This is safer than median() when Malaysia contains
# large geographic gaps between Peninsular and East Malaysia.
lon_step <- min(
  lon_diffs[lon_diffs > 0],
  na.rm = TRUE
)

lat_step <- min(
  lat_diffs[lat_diffs > 0],
  na.rm = TRUE
)


# Fallback
if (!is.finite(lon_step)) {
  lon_step <- 0.5
}

if (!is.finite(lat_step)) {
  lat_step <- 0.5
}


# Leave some space between mini-plots
panel_width <- lon_step * 0.82
panel_height <- lat_step * 0.82


# ------------------------------------------------------------
# 9. DEFINE COMMON AET SCALE
# ------------------------------------------------------------
#
# IMPORTANT:
# Every mini-panel uses the SAME AET scale.
#
# This means the height of the curves is directly comparable
# between different locations.
# ------------------------------------------------------------

aet_min <- min(
  aet_clim$Q1,
  na.rm = TRUE
)

aet_max <- max(
  aet_clim$Q3,
  na.rm = TRUE
)


if (aet_max == aet_min) {
  aet_max <- aet_min + 1
}


# ------------------------------------------------------------
# 10. DEFINE EACH MINI-PANEL
# ------------------------------------------------------------

grid_cells <- aet_clim %>%
  distinct(
    grid_id,
    longitude,
    latitude
  ) %>%
  mutate(
    
    xmin = longitude - panel_width / 2,
    xmax = longitude + panel_width / 2,
    
    ymin = latitude - panel_height / 2,
    ymax = latitude + panel_height / 2
    
  )


# ------------------------------------------------------------
# 11. TRANSFORM MONTH + AET INTO MAP COORDINATES
# ------------------------------------------------------------
#
# Month:
#
# Jan -------------------------- Dec
#
# is mapped horizontally inside each geographic cell.
#
# AET:
#
# low
#  |
#  |
# high
#
# is mapped vertically inside each geographic cell.
# ------------------------------------------------------------

plot_data <- aet_clim %>%
  
  left_join(
    grid_cells %>%
      select(
        grid_id,
        xmin,
        xmax,
        ymin,
        ymax
      ),
    by = "grid_id"
  ) %>%
  
  mutate(
    
    # -----------------------------------------
    # Month position
    # -----------------------------------------
    
    plot_x =
      xmin +
      ((month - 1) / 11) *
      (xmax - xmin),
    
    
    # -----------------------------------------
    # Mean AET position
    # -----------------------------------------
    
    mean_y =
      ymin +
      rescale(
        mean_AET,
        to = c(0, panel_height),
        from = c(aet_min, aet_max)
      ),
    
    
    # -----------------------------------------
    # Q1 position
    # -----------------------------------------
    
    q1_y =
      ymin +
      rescale(
        Q1,
        to = c(0, panel_height),
        from = c(aet_min, aet_max)
      ),
    
    
    # -----------------------------------------
    # Q3 position
    # -----------------------------------------
    
    q3_y =
      ymin +
      rescale(
        Q3,
        to = c(0, panel_height),
        from = c(aet_min, aet_max)
      )
  )


# ------------------------------------------------------------
# 12. INTERNAL MONTH GRID LINES
# ------------------------------------------------------------

month_grid <- crossing(
  
  grid_cells,
  
  month = c(
    1, 3, 5, 7, 9, 11
  )
  
) %>%
  
  mutate(
    
    x =
      xmin +
      ((month - 1) / 11) *
      (xmax - xmin)
    
  )


# ------------------------------------------------------------
# 13. INTERNAL AET GRID LINES
# ------------------------------------------------------------

aet_breaks <- pretty(
  c(
    aet_min,
    aet_max
  ),
  n = 4
)


horizontal_grid <- crossing(
  
  grid_cells,
  
  aet_value = aet_breaks
  
) %>%
  
  mutate(
    
    y =
      ymin +
      rescale(
        aet_value,
        to = c(0, panel_height),
        from = c(aet_min, aet_max)
      )
    
  )


# ------------------------------------------------------------
# 14. DETERMINE MAP EXTENT
# ------------------------------------------------------------

map_bbox <- st_bbox(malaysia)


# Include BOTH:
#
# 1. Malaysia boundary
# 2. hydrological grid cells

x_min <- min(
  map_bbox["xmin"],
  grid_cells$xmin,
  na.rm = TRUE
)

x_max <- max(
  map_bbox["xmax"],
  grid_cells$xmax,
  na.rm = TRUE
)

y_min <- min(
  map_bbox["ymin"],
  grid_cells$ymin,
  na.rm = TRUE
)

y_max <- max(
  map_bbox["ymax"],
  grid_cells$ymax,
  na.rm = TRUE
)


# Small margin around Malaysia
x_padding <- 0.4
y_padding <- 0.4


# ============================================================
# 15. CREATE FINAL PLOT — FIXED VERSION
# ============================================================

p <- ggplot() +
  
  # ----------------------------------------------------------
# MALAYSIA MAP BACKGROUND
# ----------------------------------------------------------
geom_sf(
  data = malaysia,
  fill = "grey98",
  colour = "grey35",
  linewidth = 0.6,
  inherit.aes = FALSE
) +
  
  # ----------------------------------------------------------
# INTERNAL HORIZONTAL GRID LINES
# ----------------------------------------------------------
geom_segment(
  data = horizontal_grid,
  aes(
    x = xmin,
    xend = xmax,
    y = y,
    yend = y
  ),
  colour = "grey88",
  linewidth = 0.15
) +
  
  # ----------------------------------------------------------
# INTERNAL MONTH GRID LINES
# ----------------------------------------------------------
geom_segment(
  data = month_grid,
  aes(
    x = x,
    xend = x,
    y = ymin,
    yend = ymax
  ),
  colour = "grey88",
  linewidth = 0.15
) +
  
  # ----------------------------------------------------------
# LIGHT BLUE AREA BELOW MEAN AET
# ----------------------------------------------------------
geom_ribbon(
  data = plot_data,
  aes(
    x = plot_x,
    ymin = ymin,
    ymax = mean_y,
    group = grid_id
  ),
  fill = "lightblue",
  alpha = 0.65,
  colour = NA
) +
  
  # ----------------------------------------------------------
# GREY INTERANNUAL IQR
# ----------------------------------------------------------
geom_ribbon(
  data = plot_data,
  aes(
    x = plot_x,
    ymin = q1_y,
    ymax = q3_y,
    group = grid_id
  ),
  fill = "grey65",
  alpha = 0.45,
  colour = NA
) +
  
  # ----------------------------------------------------------
# BLACK MEAN AET LINE
# ----------------------------------------------------------
geom_line(
  data = plot_data,
  aes(
    x = plot_x,
    y = mean_y,
    group = grid_id
  ),
  colour = "black",
  linewidth = 0.42,
  lineend = "round"
) +
  
  # ----------------------------------------------------------
# MINI-PLOT BORDERS
# ----------------------------------------------------------
geom_rect(
  data = grid_cells,
  aes(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax
  ),
  fill = NA,
  colour = "grey45",
  linewidth = 0.22
) +
  
  # ----------------------------------------------------------
# GEOGRAPHIC COORDINATES
#
# IMPORTANT:
# Do NOT add scale_x_continuous() or scale_y_continuous()
# below this.
# ----------------------------------------------------------
coord_sf(
  xlim = c(
    x_min - x_padding,
    x_max + x_padding
  ),
  ylim = c(
    y_min - y_padding,
    y_max + y_padding
  ),
  expand = FALSE,
  datum = st_crs(4326)
) +
  
  # ----------------------------------------------------------
# LABELS
# ----------------------------------------------------------
labs(
  title =
    "Regional monthly climatology of Actual evapotranspiration, 1982–2011",
  
  x = "Longitude",
  
  y = "Latitude",
  
  caption =
    "Light blue area: mean AET magnitude | Grey band: interannual IQR"
) +
  
  # ----------------------------------------------------------
# THEME
# ----------------------------------------------------------
theme_minimal(base_size = 10) +
  
  theme(
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    # Keep geographic grid subtle
    panel.grid.major = element_line(
      colour = "grey92",
      linewidth = 0.25
    ),
    
    panel.grid.minor = element_blank(),
    
    # Title
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    
    # Axis titles
    axis.title = element_text(
      size = 11,
      face = "bold"
    ),
    
    # Longitude / latitude labels
    axis.text = element_text(
      size = 7,
      colour = "black"
    ),
    
    axis.ticks = element_line(
      linewidth = 0.25,
      colour = "grey40"
    ),
    
    # Caption
    plot.caption = element_text(
      size = 8,
      hjust = 1,
      colour = "grey30",
      margin = margin(t = 10)
    ),
    
    plot.margin = margin(
      t = 15,
      r = 20,
      b = 15,
      l = 20
    )
  )


# ============================================================
# 16. DISPLAY
# ============================================================

print(p)


# ============================================================
# 17. SAVE HIGH-RESOLUTION IMAGE
# ============================================================

output_file <- paste0(
  "C:/Users/MSI/OneDrive/Desktop/MARC/",
  "R codes and plots/",
  "Regional_monthly_climatology_AET_1982_2011_map.png"
)

ggsave(
  filename = output_file,
  plot = p,
  width = 20,
  height = 12,
  units = "in",
  dpi = 400,
  bg = "white"
)

cat(
  "\nPlot successfully saved to:\n",
  output_file,
  "\n"
)