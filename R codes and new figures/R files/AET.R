# ============================================================
# Regional monthly climatology of Actual Evapotranspiration
# Malaysia, 1982–2011
#
# Each geographic grid cell contains:
#   Black line  = mean monthly AET
#   Grey ribbon = interannual IQR (Q1–Q3)
# ============================================================

library(tidyverse)
library(scales)

# ------------------------------------------------------------
# 1. FILE PATH
# ------------------------------------------------------------

file_path <- paste0(
  "C:/Users/MSI/OneDrive/Desktop/MARC/",
  "R codes and plots/datasets/",
  "malaysia_hydrology_monthly_1982_2011.csv"
)

# ------------------------------------------------------------
# 2. LOAD DATA
# ------------------------------------------------------------

hydro <- read_csv(
  file_path,
  show_col_types = FALSE
)

# Check required columns
required_cols <- c(
  "grid_id",
  "longitude",
  "latitude",
  "year",
  "month",
  "actual_evapotranspiration_mm"
)

missing_cols <- setdiff(required_cols, names(hydro))

if (length(missing_cols) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 3. CLEAN DATA
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
# 4. CALCULATE MONTHLY CLIMATOLOGY FOR EACH GRID CELL
# ------------------------------------------------------------

aet_clim <- hydro_clean %>%
  group_by(
    grid_id,
    longitude,
    latitude,
    month
  ) %>%
  summarise(
    mean_AET = mean(AET, na.rm = TRUE),
    
    Q1 = quantile(
      AET,
      probs = 0.25,
      na.rm = TRUE,
      names = FALSE
    ),
    
    Q3 = quantile(
      AET,
      probs = 0.75,
      na.rm = TRUE,
      names = FALSE
    ),
    
    n_years = sum(!is.na(AET)),
    
    .groups = "drop"
  )

# ------------------------------------------------------------
# 5. DETERMINE GRID SPACING
# ------------------------------------------------------------
#
# We use the actual longitude/latitude coordinates rather than
# facets. Each climatology is scaled to fit inside its
# geographic grid cell.
# ------------------------------------------------------------

lon_values <- sort(unique(aet_clim$longitude))
lat_values <- sort(unique(aet_clim$latitude))

# Typical spacing between neighbouring grid centres
lon_step <- median(diff(lon_values), na.rm = TRUE)
lat_step <- median(diff(lat_values), na.rm = TRUE)

# Fallback in case there is only one unique coordinate
if (!is.finite(lon_step)) lon_step <- 0.5
if (!is.finite(lat_step)) lat_step <- 0.5

# Leave a little whitespace between mini-plots
panel_width  <- lon_step * 0.82
panel_height <- lat_step * 0.82


# ------------------------------------------------------------
# 6. COMMON AET SCALE
# ------------------------------------------------------------
#
# Using a common y-scale means the height of curves is
# comparable between geographic grid cells.
# ------------------------------------------------------------

aet_min <- min(aet_clim$Q1, na.rm = TRUE)
aet_max <- max(aet_clim$Q3, na.rm = TRUE)

# Small protection against zero range
if (aet_max == aet_min) {
  aet_max <- aet_min + 1
}


# ------------------------------------------------------------
# 7. TRANSFORM MONTH + AET INTO MAP COORDINATES
# ------------------------------------------------------------
#
# Month 1 -> left side of cell
# Month 12 -> right side of cell
#
# AET values -> vertically scaled inside the geographic cell
# ------------------------------------------------------------

plot_data <- aet_clim %>%
  mutate(
    
    # Horizontal position of each month inside grid cell
    plot_x =
      longitude -
      panel_width / 2 +
      ((month - 1) / 11) * panel_width,
    
    # Mean AET vertical position
    mean_y =
      latitude -
      panel_height / 2 +
      rescale(
        mean_AET,
        to = c(0, panel_height),
        from = c(aet_min, aet_max)
      ),
    
    # Q1 vertical position
    q1_y =
      latitude -
      panel_height / 2 +
      rescale(
        Q1,
        to = c(0, panel_height),
        from = c(aet_min, aet_max)
      ),
    
    # Q3 vertical position
    q3_y =
      latitude -
      panel_height / 2 +
      rescale(
        Q3,
        to = c(0, panel_height),
        from = c(aet_min, aet_max)
      )
  )


# ------------------------------------------------------------
# 8. CREATE PANEL BORDERS
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
# 9. CREATE LIGHT INTERNAL GRID LINES
# ------------------------------------------------------------

# Vertical internal lines representing months
month_grid <- expand_grid(
  grid_cells,
  month = c(1, 3, 5, 7, 9, 11)
) %>%
  mutate(
    x =
      longitude -
      panel_width / 2 +
      ((month - 1) / 11) * panel_width
  )


# Horizontal internal lines
aet_breaks <- pretty(
  c(aet_min, aet_max),
  n = 4
)

horizontal_grid <- expand_grid(
  grid_cells,
  aet_value = aet_breaks
) %>%
  mutate(
    y =
      latitude -
      panel_height / 2 +
      rescale(
        aet_value,
        to = c(0, panel_height),
        from = c(aet_min, aet_max)
      )
  )


# ------------------------------------------------------------
# 10. CREATE PLOT
# ------------------------------------------------------------

p <- ggplot() +
  
  # ----------------------------------------------------------
# Light horizontal grid lines inside each mini-panel
# ----------------------------------------------------------

geom_segment(
  data = horizontal_grid,
  aes(
    x = xmin,
    xend = xmax,
    y = y,
    yend = y
  ),
  colour = "grey90",
  linewidth = 0.18
) +
  
  # ----------------------------------------------------------
# Light vertical month grid lines
# ----------------------------------------------------------

geom_segment(
  data = month_grid,
  aes(
    x = x,
    xend = x,
    y = ymin,
    yend = ymax
  ),
  colour = "grey90",
  linewidth = 0.18
) +
  
  # ----------------------------------------------------------
# IQR ribbon
# ----------------------------------------------------------

geom_ribbon(
  data = plot_data,
  aes(
    x = plot_x,
    ymin = q1_y,
    ymax = q3_y,
    group = grid_id
  ),
  fill = "grey75",
  alpha = 0.55,
  colour = NA
) +
  
  # ----------------------------------------------------------
# Mean climatology
# ----------------------------------------------------------

geom_line(
  data = plot_data,
  aes(
    x = plot_x,
    y = mean_y,
    group = grid_id
  ),
  colour = "black",
  linewidth = 0.35,
  lineend = "round"
) +
  
  # ----------------------------------------------------------
# Thin panel borders
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
  colour = "grey55",
  linewidth = 0.22
) +
  
  # ----------------------------------------------------------
# Geographic coordinate system
# ----------------------------------------------------------

coord_fixed(
  ratio = 1,
  clip = "off"
) +
  
  # ----------------------------------------------------------
# Longitude / latitude labels
# ----------------------------------------------------------

scale_x_continuous(
  breaks = lon_values,
  labels = function(x) sprintf("%.2f", x),
  expand = expansion(mult = c(0.015, 0.015))
) +
  
  scale_y_continuous(
    breaks = lat_values,
    labels = function(x) sprintf("%.2f", x),
    expand = expansion(mult = c(0.015, 0.015))
  ) +
  
  # ----------------------------------------------------------
# Titles
# ----------------------------------------------------------

labs(
  title =
    "Regional monthly climatology of Actual evapotranspiration, 1982–2011",
  
  x = "Longitude / Calendar month",
  
  y = expression(
    paste(
      "Latitude / AET (mm month"^{-1}, ")"
    )
  ),
  
  caption =
    "Shaded bands: interannual IQR"
) +
  
  # ----------------------------------------------------------
# Theme
# ----------------------------------------------------------

theme_minimal(base_size = 10) +
  
  theme(
    
    # White background
    plot.background =
      element_rect(
        fill = "white",
        colour = NA
      ),
    
    panel.background =
      element_rect(
        fill = "white",
        colour = NA
      ),
    
    # Remove ggplot's global grid
    panel.grid =
      element_blank(),
    
    # Title
    plot.title =
      element_text(
        size = 16,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 12)
      ),
    
    # Axis labels
    axis.title =
      element_text(
        size = 11,
        face = "bold"
      ),
    
    # Longitude / latitude labels
    axis.text =
      element_text(
        size = 5,
        colour = "black"
      ),
    
    axis.ticks =
      element_line(
        linewidth = 0.25,
        colour = "grey40"
      ),
    
    # Caption on bottom right
    plot.caption =
      element_text(
        size = 8,
        hjust = 1,
        colour = "grey30",
        margin = margin(t = 8)
      ),
    
    # Margins
    plot.margin =
      margin(
        t = 15,
        r = 20,
        b = 15,
        l = 20
      )
  )


# ------------------------------------------------------------
# 11. DISPLAY PLOT
# ------------------------------------------------------------

print(p)


# ------------------------------------------------------------
# 12. SAVE HIGH-RESOLUTION PNG
# ------------------------------------------------------------

output_file <- paste0(
  "C:/Users/MSI/OneDrive/Desktop/MARC/",
  "R codes and plots/",
  "Regional_monthly_climatology_AET_1982_2011.png"
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
  "\nPlot saved to:\n",
  output_file,
  "\n"
)