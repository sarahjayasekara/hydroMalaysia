# Install required packages if not already present
required_packages <- c("tidyverse", "sf", "rnaturalearth", "rnaturalearthdata", "arrow")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# 1. Load Malaysia map outline cleanly
malaysia_map <- ne_countries(country = "Malaysia", scale = "medium", returnclass = "sf")

# 2. Load and process dataset
# Replace path with your actual file path if needed
input_file <- "C:/Users/MSI/OneDrive/Documents/files for R/Malaysia_Hydrology_ALL.csv"

df_processed <- read_csv(input_file) %>%
  mutate(
    aet_negative_corrected = if_else(actual_evapotranspiration_mm < 0, 1, 0, missing = 0)
  )

grid_lookup <- df_processed %>%
  select(grid_id, longitude, latitude) %>%
  distinct()

aet_summary <- df_processed %>%
  group_by(grid_id, longitude, latitude) %>%
  summarise(
    corrected_aet_records = sum(aet_negative_corrected, na.rm = TRUE),
    .groups = "drop"
  )
corrected_cells <- aet_summary %>% 
  filter(corrected_aet_records > 0)

# ==========================================================
# REFINED FIGURE 1: Publication-Ready Portrait Map
# ==========================================================

fig1_publication <- ggplot() +
  # Layer 1: Soft country boundary fill for geographic context
  geom_sf(
    data = malaysia_map, 
    fill = "gray92", 
    color = "gray50", 
    linewidth = 0.35,
    inherit.aes = FALSE
  ) +
  
  # Layer 2: Native 0.5° hydrology grid cells as structured tiles
  geom_tile(
    data = grid_lookup, 
    aes(x = longitude, y = latitude, fill = "Native 0.5° Grid Cells"), 
    width = 0.5, 
    height = 0.5, 
    color = "grey40", 
    linewidth = 0.15, 
    alpha = 0.85
  ) +
  
  # Layer 3: Overlay cells with corrected AET values (marker x) - safely added if present
  { if(nrow(corrected_cells) > 0) 
    geom_point(
      data = corrected_cells, 
      aes(x = longitude, y = latitude, size = corrected_aet_records, color = "Corrected AET Cells"),
      shape = 4, 
      stroke = 1.1
    ) 
  } +
  
  # Manual color and fill aesthetics for a unified legend
  scale_fill_manual(
    name = NULL, 
    values = c("Native 0.1° Grid Cells" = "steelblue3")
  ) +
  scale_color_manual(
    name = NULL, 
    values = c("Corrected AET Cells" = "black")
  ) +
  scale_size_continuous(
    name = "Corrected AET Records",
    range = c(2.0, 5.5),
    guide = guide_legend(override.aes = list(shape = 4, color = "black", stroke = 1.1))
  ) +
  
  # CRITICAL FIX: Use coord_sf with equal scaling for equatorial regions
  coord_sf(
    xlim = c(99.5, 119.5), 
    ylim = c(0.5, 7.5), 
    expand = FALSE,
    default_crs = sf::st_crs(4326)
  ) +
  
  # Clean theme suitable for academic reports
  theme_minimal(base_size = 11) +
  labs(
    title = "Malaysia Hydrology-Grid Coverage and Quality Control",
    subtitle = "Native 0.5° spatial resolution cells mapped over country boundaries (1982–2011)",
    x = "Longitude (°E)",
    y = "Latitude (°N)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 9, color = "gray30", hjust = 0, margin = margin(b = 10)),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = 0),
    legend.text = element_text(size = 9),
    axis.text = element_text(size = 9, color = "black"),
    axis.title = element_text(size = 10, face = "bold")
  )

print(fig1_publication)

# Save with portrait dimensions (taller format) to prevent stretching
ggsave(
  filename = "fig01_native_grid_coverage_publication.png", 
  plot = fig1_publication, 
  width = 6.2, 
  height = 8.0, 
  dpi = 400
)

ggsave(
  filename = "fig01_native_grid_coverage_publication.pdf", 
  plot = fig1_publication, 
  width = 6.2, 
  height = 8.0
)



# Increase timeout limit to 300 seconds (5 minutes)
options(timeout = 300)

# Try installing the package again
install.packages("arrow")