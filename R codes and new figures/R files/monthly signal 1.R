# ================================================================
# MONTHLY PRECIPITATION CLIMATOLOGY ACROSS MALAYSIA
# 1982–2011
#
# 12 spatial panels:
# Jan Feb Mar Apr
# May Jun Jul Aug
# Sep Oct Nov Dec
#
# All months use ONE COMMON precipitation colour scale.
# ================================================================


# ================================================================
# 0. INSTALL PACKAGES — RUN ONCE IF NEEDED
# ================================================================

# install.packages("tidyverse")
# install.packages("sf")
# install.packages("viridis")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")


# ================================================================
# 1. LOAD PACKAGES
# ================================================================

library(tidyverse)
library(ggplot2)
library(sf)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)


# ================================================================
# 2. FILE PATHS
# ================================================================

input_file <- paste0(
  "C:/Users/MSI/OneDrive/Desktop/MARC/",
  "R codes and plots/datasets/",
  "malaysia_hydrology_monthly_1982_2011.csv"
)

output_file <- paste0(
  "C:/Users/MSI/OneDrive/Desktop/MARC/",
  "R codes and plots/",
  "Malaysia_monthly_precipitation_climatology.png"
)


# ================================================================
# 3. READ DATA
# ================================================================

hydro <- read_csv(
  input_file,
  show_col_types = FALSE
)


# ================================================================
# 4. VALIDATE REQUIRED COLUMNS
# ================================================================

required_cols <- c(
  "grid_id",
  "longitude",
  "latitude",
  "year",
  "month",
  "precipitation_mm"
)

missing_cols <- setdiff(
  required_cols,
  names(hydro)
)

if (length(missing_cols) > 0) {
  
  stop(
    paste0(
      "The following required columns are missing: ",
      paste(missing_cols, collapse = ", ")
    )
  )
}


# ================================================================
# 5. CLEAN DATA
# ================================================================

hydro_clean <- hydro %>%
  
  transmute(
    
    grid_id = as.character(grid_id),
    
    longitude = as.numeric(longitude),
    
    latitude = as.numeric(latitude),
    
    year = as.integer(year),
    
    month = as.integer(month),
    
    precipitation_mm =
      as.numeric(precipitation_mm)
    
  ) %>%
  
  filter(
    
    year >= 1982,
    year <= 2011,
    
    month >= 1,
    month <= 12,
    
    !is.na(grid_id),
    !is.na(longitude),
    !is.na(latitude)
    
  )


# ================================================================
# 6. BASIC DATA CHECKS
# ================================================================

total_grid_cells <- hydro_clean %>%
  
  distinct(
    grid_id,
    longitude,
    latitude
  ) %>%
  
  nrow()


if (total_grid_cells == 0) {
  
  stop(
    "No valid grid cells were found in the dataset."
  )
}


# ================================================================
# 7. CALCULATE 1982–2011 MONTHLY PRECIPITATION CLIMATOLOGY
# ================================================================
#
# For each grid cell and calendar month:
#
# P_clim(i,m) = mean precipitation across years
#
# ================================================================

precip_climatology <- hydro_clean %>%
  
  group_by(
    grid_id,
    longitude,
    latitude,
    month
  ) %>%
  
  summarise(
    
    precipitation_clim =
      
      if (
        all(is.na(precipitation_mm))
      ) {
        
        NA_real_
        
      } else {
        
        mean(
          precipitation_mm,
          na.rm = TRUE
        )
        
      },
    
    n_valid_years =
      sum(
        !is.na(precipitation_mm)
      ),
    
    .groups = "drop"
  )


# ================================================================
# 8. CHECK COMPLETENESS OF EACH GRID CELL
# ================================================================

cell_completeness <- precip_climatology %>%
  
  group_by(
    grid_id,
    longitude,
    latitude
  ) %>%
  
  summarise(
    
    valid_months =
      sum(
        !is.na(precipitation_clim)
      ),
    
    .groups = "drop"
  )


complete_cells <- cell_completeness %>%
  
  filter(
    valid_months == 12
  )


incomplete_cells <- cell_completeness %>%
  
  filter(
    valid_months < 12
  )


n_complete_cells <- nrow(
  complete_cells
)

n_incomplete_cells <- nrow(
  incomplete_cells
)


# ================================================================
# 9. REPORT INCOMPLETE CELLS
# ================================================================

if (n_incomplete_cells > 0) {
  
  cat(
    "\nGrid cells with incomplete monthly climatology:\n"
  )
  
  print(
    incomplete_cells
  )
}


# ================================================================
# 10. CREATE COMPLETE GRID × MONTH STRUCTURE
# ================================================================
#
# This makes missing grid-cell/month combinations explicit.
# We preserve them as NA instead of silently removing them.
#
# ================================================================

grid_locations <- hydro_clean %>%
  
  distinct(
    grid_id,
    longitude,
    latitude
  )


expected_grid_months <- grid_locations %>%
  
  crossing(
    month = 1:12
  )


precip_complete_structure <- expected_grid_months %>%
  
  left_join(
    
    precip_climatology,
    
    by = c(
      "grid_id",
      "longitude",
      "latitude",
      "month"
    )
    
  )


# ================================================================
# 11. CREATE ORDERED MONTH LABELS
# ================================================================

precip_complete_structure <- precip_complete_structure %>%
  
  mutate(
    
    month_name = factor(
      
      month,
      
      levels = 1:12,
      
      labels = month.abb
      
    )
    
  )


# ================================================================
# 12. DETECT LONGITUDE GRID SPACING
# ================================================================

unique_lon <- sort(
  unique(
    grid_locations$longitude
  )
)


lon_diff <- diff(
  unique_lon
)


lon_diff <- lon_diff[
  is.finite(lon_diff) &
    lon_diff > 0
]


if (length(lon_diff) == 0) {
  
  stop(
    "Unable to determine longitude grid spacing."
  )
}


# Median is more robust than minimum if coordinates
# contain floating-point irregularities.

lon_spacing <- median(
  lon_diff
)


# ================================================================
# 13. DETECT LATITUDE GRID SPACING
# ================================================================

unique_lat <- sort(
  unique(
    grid_locations$latitude
  )
)


lat_diff <- diff(
  unique_lat
)


lat_diff <- lat_diff[
  is.finite(lat_diff) &
    lat_diff > 0
]


if (length(lat_diff) == 0) {
  
  stop(
    "Unable to determine latitude grid spacing."
  )
}


lat_spacing <- median(
  lat_diff
)


# ================================================================
# 14. CHECK DETECTED SPACING
# ================================================================

cat(
  "\nDetected longitude spacing:",
  lon_spacing,
  "degrees\n"
)

cat(
  "Detected latitude spacing:",
  lat_spacing,
  "degrees\n"
)


# ================================================================
# 15. CONSTRUCT GRID-CELL BOUNDARIES
# ================================================================
#
# Coordinates are assumed to represent grid-cell centres.
#
# xmin = lon - dx/2
# xmax = lon + dx/2
#
# ymin = lat - dy/2
# ymax = lat + dy/2
#
# ================================================================

plot_data <- precip_complete_structure %>%
  
  mutate(
    
    xmin =
      longitude -
      lon_spacing / 2,
    
    xmax =
      longitude +
      lon_spacing / 2,
    
    ymin =
      latitude -
      lat_spacing / 2,
    
    ymax =
      latitude +
      lat_spacing / 2
    
  )


# ================================================================
# 16. COMMON PRECIPITATION SCALE
# ================================================================
#
# IMPORTANT:
#
# These limits are calculated ONCE using all 12 months.
#
# Therefore:
#
# the same colour = the same precipitation magnitude
#
# in Jan, Feb, Mar ... Dec.
#
# ================================================================

global_min <- min(
  plot_data$precipitation_clim,
  na.rm = TRUE
)

global_max <- max(
  plot_data$precipitation_clim,
  na.rm = TRUE
)

global_mean <- mean(
  plot_data$precipitation_clim,
  na.rm = TRUE
)


if (
  !is.finite(global_min) ||
  !is.finite(global_max)
) {
  
  stop(
    "Unable to determine precipitation colour limits."
  )
}


if (global_min == global_max) {
  
  stop(
    "All climatological precipitation values are identical."
  )
}


# ================================================================
# 17. MONTHLY SPATIAL SUMMARY
# ================================================================

monthly_summary <- plot_data %>%
  
  group_by(
    month,
    month_name
  ) %>%
  
  summarise(
    
    spatial_mean_mm =
      mean(
        precipitation_clim,
        na.rm = TRUE
      ),
    
    spatial_min_mm =
      min(
        precipitation_clim,
        na.rm = TRUE
      ),
    
    spatial_max_mm =
      max(
        precipitation_clim,
        na.rm = TRUE
      ),
    
    valid_grid_cells =
      sum(
        !is.na(precipitation_clim)
      ),
    
    .groups = "drop"
  )


# ================================================================
# 18. COUNT CLIMATOLOGY VALUES
# ================================================================

n_grid_month_values <- nrow(
  plot_data
)


n_missing_climatology <- sum(
  is.na(
    plot_data$precipitation_clim
  )
)


# ================================================================
# 19. GET MALAYSIA BOUNDARY
# ================================================================

malaysia <- ne_countries(
  
  scale = "medium",
  
  country = "Malaysia",
  
  returnclass = "sf"
  
)


# Ensure WGS84

malaysia <- st_transform(
  malaysia,
  4326
)


# ================================================================
# 20. GET GEOGRAPHIC EXTENT
# ================================================================

map_bbox <- st_bbox(
  malaysia
)


grid_xmin <- min(
  plot_data$xmin,
  na.rm = TRUE
)

grid_xmax <- max(
  plot_data$xmax,
  na.rm = TRUE
)

grid_ymin <- min(
  plot_data$ymin,
  na.rm = TRUE
)

grid_ymax <- max(
  plot_data$ymax,
  na.rm = TRUE
)


# Combine raster + map extents

x_min <- min(
  grid_xmin,
  as.numeric(
    map_bbox["xmin"]
  )
)

x_max <- max(
  grid_xmax,
  as.numeric(
    map_bbox["xmax"]
  )
)

y_min <- min(
  grid_ymin,
  as.numeric(
    map_bbox["ymin"]
  )
)

y_max <- max(
  grid_ymax,
  as.numeric(
    map_bbox["ymax"]
  )
)


# ================================================================
# 21. SMALL GEOGRAPHIC MARGIN
# ================================================================

x_range <- x_max - x_min

y_range <- y_max - y_min


x_margin <- x_range * 0.025

y_margin <- y_range * 0.04


plot_xlim <- c(
  x_min - x_margin,
  x_max + x_margin
)

plot_ylim <- c(
  y_min - y_margin,
  y_max + y_margin
)


# ================================================================
# 22. PRINT DIAGNOSTICS
# ================================================================

cat(
  "\n",
  "============================================================\n",
  "MALAYSIA PRECIPITATION CLIMATOLOGY SUMMARY\n",
  "============================================================\n",
  sep = ""
)


cat(
  "Total unique grid cells: ",
  total_grid_cells,
  "\n",
  sep = ""
)


cat(
  "Grid cells with all 12 months: ",
  n_complete_cells,
  "\n",
  sep = ""
)


cat(
  "Incomplete grid cells: ",
  n_incomplete_cells,
  "\n",
  sep = ""
)


cat(
  "Detected longitude spacing: ",
  round(
    lon_spacing,
    4
  ),
  " degrees\n",
  sep = ""
)


cat(
  "Detected latitude spacing: ",
  round(
    lat_spacing,
    4
  ),
  " degrees\n",
  sep = ""
)


cat(
  "Expected grid-cell × month combinations: ",
  n_grid_month_values,
  "\n",
  sep = ""
)


cat(
  "Missing climatology values: ",
  n_missing_climatology,
  "\n",
  sep = ""
)


cat(
  "Global minimum climatological precipitation: ",
  round(
    global_min,
    2
  ),
  " mm/month\n",
  sep = ""
)


cat(
  "Global maximum climatological precipitation: ",
  round(
    global_max,
    2
  ),
  " mm/month\n",
  sep = ""
)


cat(
  "Global mean climatological precipitation: ",
  round(
    global_mean,
    2
  ),
  " mm/month\n",
  sep = ""
)


cat(
  "============================================================\n"
)


# ================================================================
# 23. PRINT MONTHLY SUMMARY TABLE
# ================================================================

cat(
  "\nMonthly spatial precipitation summary:\n\n"
)


print(
  
  monthly_summary %>%
    
    select(
      month,
      month_name,
      spatial_mean_mm,
      spatial_min_mm,
      spatial_max_mm,
      valid_grid_cells
    )
  
)


# ================================================================
# 24. CREATE THE 12-PANEL FIGURE
# ================================================================

p <- ggplot() +
  
  
  # --------------------------------------------------------------
# PRECIPITATION GRID CELLS
#
# geom_rect() is used instead of points so that the original
# gridded spatial structure is preserved.
# --------------------------------------------------------------

geom_rect(
  
  data = plot_data,
  
  aes(
    
    xmin = xmin,
    xmax = xmax,
    
    ymin = ymin,
    ymax = ymax,
    
    fill = precipitation_clim
    
  ),
  
  # Very subtle cell boundary
  colour = alpha(
    "grey25",
    0.20
  ),
  
  linewidth = 0.08
  
) +
  
  
  # --------------------------------------------------------------
# MALAYSIA COASTLINE / NATIONAL BOUNDARY
# --------------------------------------------------------------

geom_sf(
  
  data = malaysia,
  
  fill = NA,
  
  colour = "grey10",
  
  linewidth = 0.35,
  
  linejoin = "round",
  
  inherit.aes = FALSE
  
) +
  
  
  # --------------------------------------------------------------
# 12 MONTHLY FACETS
# --------------------------------------------------------------

facet_wrap(
  
  ~ month_name,
  
  ncol = 4,
  
  drop = FALSE
  
) +
  
  
  # --------------------------------------------------------------
# COMMON PRECIPITATION COLOUR SCALE
#
# option = "viridis":
#
# dark purple
#      ↓
# blue
#      ↓
# teal
#      ↓
# green
#      ↓
# yellow
#
# limits are GLOBAL, not month-specific.
# --------------------------------------------------------------

scale_fill_viridis_c(
  
  option = "viridis",
  
  direction = 1,
  
  limits = c(
    global_min,
    global_max
  ),
  
  na.value = "white",
  
  name =
    "Precipitation\n(mm month\u207B\u00B9)",
  
  guide = guide_colorbar(
    
    title.position = "top",
    
    title.hjust = 0,
    
    barheight =
      grid::unit(
        45,
        "mm"
      ),
    
    barwidth =
      grid::unit(
        5,
        "mm"
      ),
    
    ticks = TRUE,
    
    frame.colour = "grey40",
    
    frame.linewidth = 0.3
    
  )
  
) +
  
  
  # --------------------------------------------------------------
# GEOGRAPHIC COORDINATES
#
# IMPORTANT:
# All facets use identical geographic limits.
#
# We avoid separate scale_x_continuous() and
# scale_y_continuous() calls to reduce the possibility of
# coord_sf() graticule-label conflicts.
# --------------------------------------------------------------

coord_sf(
  
  xlim = plot_xlim,
  
  ylim = plot_ylim,
  
  expand = FALSE,
  
  crs = st_crs(4326),
  
  datum = st_crs(4326)
  
) +
  
  
  # --------------------------------------------------------------
# TITLES AND AXES
# --------------------------------------------------------------

labs(
  
  title =
    "Monthly precipitation climatology across Malaysia",
  
  subtitle =
    "1982–2011 monthly climatology | Common color scale across all months",
  
  x =
    "Longitude (\u00B0E)",
  
  y =
    "Latitude (\u00B0N)",
  
  caption =
    paste0(
      "Each panel shows mean monthly precipitation for 1982–2011. ",
      "A common color scale is used across all 12 calendar months."
    )
  
) +
  
  
  # --------------------------------------------------------------
# PUBLICATION-STYLE THEME
# --------------------------------------------------------------

theme_minimal(
  base_size = 10
) +
  
  
  theme(
    
    # ------------------------------------------------------------
    # Figure background
    # ------------------------------------------------------------
    
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
    
    
    # ------------------------------------------------------------
    # Overall title
    # ------------------------------------------------------------
    
    plot.title =
      element_text(
        
        size = 18,
        
        face = "bold",
        
        hjust = 0,
        
        colour = "grey10",
        
        margin = margin(
          b = 4
        )
        
      ),
    
    
    # ------------------------------------------------------------
    # Subtitle
    # ------------------------------------------------------------
    
    plot.subtitle =
      element_text(
        
        size = 10,
        
        colour = "grey40",
        
        hjust = 0,
        
        margin = margin(
          b = 12
        )
        
      ),
    
    
    # ------------------------------------------------------------
    # Month labels
    # ------------------------------------------------------------
    
    strip.text =
      element_text(
        
        size = 10,
        
        face = "bold",
        
        colour = "grey10",
        
        margin = margin(
          t = 3,
          b = 4
        )
        
      ),
    
    
    strip.background =
      element_blank(),
    
    
    # ------------------------------------------------------------
    # Axis titles
    # ------------------------------------------------------------
    
    axis.title.x =
      element_text(
        
        size = 10,
        
        face = "bold",
        
        margin = margin(
          t = 7
        )
        
      ),
    
    
    axis.title.y =
      element_text(
        
        size = 10,
        
        face = "bold",
        
        margin = margin(
          r = 7
        )
        
      ),
    
    
    # ------------------------------------------------------------
    # Axis labels
    # ------------------------------------------------------------
    
    axis.text =
      element_text(
        
        size = 7.5,
        
        colour = "grey25"
        
      ),
    
    
    axis.ticks =
      element_line(
        
        colour = "grey45",
        
        linewidth = 0.25
        
      ),
    
    
    # ------------------------------------------------------------
    # Geographic grid
    # ------------------------------------------------------------
    
    panel.grid.major =
      element_line(
        
        colour = "grey88",
        
        linewidth = 0.25
        
      ),
    
    
    panel.grid.minor =
      element_blank(),
    
    
    # ------------------------------------------------------------
    # Facet spacing
    # ------------------------------------------------------------
    
    panel.spacing =
      grid::unit(
        0.65,
        "lines"
      ),
    
    
    # ------------------------------------------------------------
    # Legend
    # ------------------------------------------------------------
    
    legend.position =
      "right",
    
    
    legend.title =
      element_text(
        
        size = 9,
        
        face = "bold",
        
        lineheight = 0.95
        
      ),
    
    
    legend.text =
      element_text(
        size = 8
      ),
    
    
    # ------------------------------------------------------------
    # Caption
    # ------------------------------------------------------------
    
    plot.caption =
      element_text(
        
        size = 8,
        
        colour = "grey45",
        
        hjust = 0,
        
        margin = margin(
          t = 10
        )
        
      ),
    
    
    # ------------------------------------------------------------
    # Overall margins
    # ------------------------------------------------------------
    
    plot.margin =
      margin(
        t = 15,
        r = 18,
        b = 15,
        l = 15
      )
    
  )


# ================================================================
# 25. DISPLAY FIGURE
# ================================================================

print(p)


# ================================================================
# 26. SAVE HIGH-RESOLUTION PNG
# ================================================================

ggsave(
  
  filename = output_file,
  
  plot = p,
  
  width = 14,
  
  height = 12,
  
  units = "in",
  
  dpi = 400,
  
  bg = "white"
  
)


# ================================================================
# 27. CONFIRM OUTPUT
# ================================================================

cat(
  "\n",
  "============================================================\n",
  "FIGURE SAVED SUCCESSFULLY\n",
  "============================================================\n",
  output_file,
  "\n",
  "============================================================\n",
  sep = ""
)