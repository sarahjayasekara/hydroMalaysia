# ==========================================================
# REFINED R SCRIPT: Regional Monthly Climatology (Figure 4)
# ==========================================================

library(tidyverse)
library(readr)

# Define root paths matching your project structure
root_dir <- "C:/Users/MSI/OneDrive/Desktop/MARC/sociohydrology/github - malay"

climatology_long_path <- file.path(root_dir, "data/processed/regional_monthly_climatology_1982_2011_long.csv")
inventory_path        <- file.path(root_dir, "results/tables/region_inventory_0p5degree.csv")
figure_dir            <- file.path(root_dir, "results/figures")

dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

# 1. Load Datasets
climatology_long <- read_csv(climatology_long_path, show_col_types = FALSE)
inventory        <- read_csv(inventory_path, show_col_types = FALSE)

# 2. Prepare Data and Filter Out Unused Spatial Combinations
target_variable <- "precipitation_mm"

var_data <- climatology_long %>%
  filter(variable == target_variable) %>%
  inner_join(
    inventory %>% select(region_id, region_lat_index, region_lon_index, native_cell_count), 
    by = "region_id"
  ) %>%
  drop_na(mean) # Ensures completely empty spatial coordinates are omitted

# 3. Create Optimized Multi-Panel Geographic Matrix Plot
p_climatology <- ggplot(var_data, aes(x = month)) +
  # Shaded Interannual IQR band
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "grey85", alpha = 0.8) +
  # Climatological mean line
  geom_line(aes(y = mean), color = "black", linewidth = 0.4) +
  
  # Facet grid using numeric indices with automatic dropping of empty panels
  facet_grid(
    rows = vars(region_lat_index), 
    cols = vars(region_lon_index), 
    drop = TRUE,
    scales = "fixed"
  ) +
  
  # Minimalist x-axis scaling to prevent overlapping text
  scale_x_continuous(
    breaks = c(1, 7), 
    labels = c("J", "J")
  ) +
  
  labs(
    title = "Regional monthly climatology of Precipitation, 1982–2011",
    x = "Calendar month (Jan / Jul)",
    y = "Precipitation (mm month⁻¹)",
    caption = "Shaded bands: interannual IQR. † Region contains fewer than four native cells."
  ) +
  theme_bw(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0.5, margin = margin(b = 8)),
    plot.caption = element_text(size = 7, hjust = 1, color = "grey30"),
    strip.background = element_rect(fill = "white", color = "grey70", linewidth = 0.2),
    strip.text = element_text(size = 4.5, face = "bold"),
    axis.text.x = element_text(size = 5, color = "black"),
    axis.text.y = element_blank(),  # Suppress cluttered inner y-axis text across small subplots
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 9, face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2, linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.05, "lines"), # Tighten spacing to mirror a true spatial grid matrix
    panel.border = element_rect(color = "grey60", linewidth = 0.3)
  )

# Print preview
print(p_climatology)

# 4. Save High-Resolution Outputs with Balanced Dimensions
ggsave(
  filename = file.path(figure_dir, "fig04_precipitation_regional_climatology.png"), 
  plot = p_climatology, 
  width = 11.0, 
  height = 9.0, 
  dpi = 400
)

ggsave(
  filename = file.path(figure_dir, "fig04_precipitation_regional_climatology.pdf"), 
  plot = p_climatology, 
  width = 11.0, 
  height = 9.0
)

cat("Refined Figure 4 generated and saved successfully!\n")
