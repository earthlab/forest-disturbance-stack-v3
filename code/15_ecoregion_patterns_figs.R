### Forest-disturbance-stack-v3

### Ecoregion patterns

### Matt Bitters
### matthew.bitters@colorado.edu

################################################################################

# Working directory
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

# Packages
packages <- c("here", "sf", "dplyr", "ggplot2", "patchwork", "tidyr", "readr", "scales")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) install.packages(packages[!installed])

library(here)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(patchwork)
library(scales)


terraOptions(
  tempdir = "tmp_terra",
  memfrac = 0.6,    # use 60% of RAM
  memmax = 180000,   # cap memory at 180 GB
  progress = 1
)


################################################################################
#### SETTINGS
################################################################################

# Color transformation for disturbance percentages.
#
# Recommended:
#   "sqrt"   = emphasizes differences at lower percentages
#   "linear" = conventional 0-100% scale
#

color_transform <- "sqrt"


################################################################################
#### PATHS
################################################################################

# Ecoregion-level cumulative spatial metrics

any_csv <- paste0(
  "data/derived/spatial_metrics/",
  "spatial_metrics_ecoregion_any.csv"
)

extreme_csv <- paste0(
  "data/derived/spatial_metrics/",
  "spatial_metrics_ecoregion_extreme.csv"
)


# EPA Level III ecoregion polygons

eco_shp <- paste0(
  "data/raw/us_eco_l3_state_boundaries/",
  "us_eco_l3_state_boundaries.shp"
)


# Figure output directory

fig_dir <- "figures/ecoregion_patterns"

dir.create(
  fig_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


################################################################################
#### READ ECOREGION METRICS
################################################################################

eco_any <- read_csv(
  any_csv,
  show_col_types = FALSE
) %>%
  mutate(
    threshold = "Any"
  )


eco_extreme <- read_csv(
  extreme_csv,
  show_col_types = FALSE
) %>%
  mutate(
    threshold = "Extreme"
  )


eco_metrics <- bind_rows(
  eco_any,
  eco_extreme
)


################################################################################
#### RETAIN HOTTER-DROUGHT DISTURBANCE FRAMEWORK
################################################################################

# PDSI and all PDSI combinations are excluded from the primary analysis.

disturbances_keep <- c(
  "wf",
  "bt",
  "hd",
  "wf_bt",
  "wf_hd",
  "bt_hd",
  "wf_bt_hd"
)


eco_metrics <- eco_metrics %>%
  filter(
    disturbance %in% disturbances_keep
  )


################################################################################
#### READ ECOREGIONS
################################################################################

eco <- st_read(
  eco_shp,
  quiet = TRUE
)


# Inspect available attributes if needed:
#
# names(eco)
#
# This script assumes:
#
#   US_L3CODE = Level III ecoregion code
#   US_L3NAME = Level III ecoregion name
#
# If the name field has a different name in your shapefile,
# change US_L3NAME below.


################################################################################
#### CREATE ECOREGION LOOKUP TABLE
################################################################################

eco_lookup <- eco %>%
  st_drop_geometry() %>%
  transmute(
    ecoregion_id = as.numeric(US_L3CODE),
    ecoregion_name = US_L3NAME
  ) %>%
  distinct()


################################################################################
#### JOIN ECOREGION NAMES
################################################################################

eco_metrics <- eco_metrics %>%
  mutate(
    ecoregion_id = as.numeric(ecoregion_id)
  ) %>%
  left_join(
    eco_lookup,
    by = "ecoregion_id"
  )


# Check for unmatched ecoregions

unmatched_ecoregions <- eco_metrics %>%
  filter(
    is.na(ecoregion_name)
  ) %>%
  distinct(
    ecoregion_id
  )


if (nrow(unmatched_ecoregions) > 0) {
  
  print(
    unmatched_ecoregions
  )
  
  warning(
    "Some ecoregion IDs did not match the ecoregion shapefile."
  )
}


################################################################################
#### DISTURBANCE LABELS
################################################################################

disturbance_labels <- c(
  "wf"       = "Wildfire",
  "bt"       = "Biotic",
  "hd"       = "Drought",
  "wf_bt"    = "Wildfire + Biotic",
  "wf_hd"    = "Wildfire + Drought",
  "bt_hd"    = "Biotic + Drought",
  "wf_bt_hd" = "Wildfire + Biotic +\nDrought"
)


eco_metrics <- eco_metrics %>%
  mutate(
    
    disturbance_label = factor(
      disturbance,
      levels = disturbances_keep,
      labels = disturbance_labels[disturbances_keep]
    )
    
  )


################################################################################
#### CALCULATE TOTAL FOREST AREA BY ECOREGION
################################################################################

# The CSV contains:
#
#   disturbed area (km2)
#   percent of forest in the ecoregion disturbed
#
# Therefore:
#
#   forest area =
#     disturbed area / (percent disturbed / 100)
#
# We use wildfire under the Any threshold to recover the denominator.
#
# This is mathematically the same forest-area denominator used when the
# original spatial_metrics_ecoregion CSV was created.


forest_area <- eco_metrics %>%
  filter(
    threshold == "Any",
    disturbance == "wf",
    percent_forest_ecoregion > 0
  ) %>%
  transmute(
    
    ecoregion_id,
    ecoregion_name,
    
    forest_area_km2 =
      area_km2 /
      (percent_forest_ecoregion / 100)
    
  )


################################################################################
#### CHECK FOREST AREA CALCULATION
################################################################################

print(
  forest_area %>%
    arrange(
      desc(forest_area_km2)
    )
)


################################################################################
#### ORDER ECOREGIONS BY TRIPLE-DISTURBANCE EXPOSURE
################################################################################

# Rank ecoregions according to the percentage of forest experiencing
# wildfire + biotic disturbance + hotter drought under the Any threshold.
#
# The same order will be retained in:
#
#   1. Any heatmap
#   2. Extreme heatmap
#   3. Forest-area bar chart


ecoregion_order <- eco_metrics %>%
  filter(
    threshold == "Any",
    disturbance == "wf_bt_hd"
  ) %>%
  arrange(
    desc(percent_forest_ecoregion)
  ) %>%
  pull(
    ecoregion_name
  )


# Reverse factor order so highest values appear at the TOP of ggplot.

ecoregion_levels <- rev(
  ecoregion_order
)


eco_metrics <- eco_metrics %>%
  mutate(
    
    ecoregion_name = factor(
      ecoregion_name,
      levels = ecoregion_levels
    )
    
  )


forest_area <- forest_area %>%
  mutate(
    
    ecoregion_name = factor(
      ecoregion_name,
      levels = ecoregion_levels
    )
    
  )


################################################################################
#### INSPECT ECOREGION RANKING
################################################################################

triple_ranking <- eco_metrics %>%
  filter(
    threshold == "Any",
    disturbance == "wf_bt_hd"
  ) %>%
  arrange(
    desc(percent_forest_ecoregion)
  ) %>%
  select(
    ecoregion_name,
    area_km2,
    percent_forest_ecoregion
  )


print(
  triple_ranking,
  n = Inf
)


################################################################################
#### SPLIT ANY AND EXTREME DATA
################################################################################

plot_any <- eco_metrics %>%
  filter(
    threshold == "Any"
  )


plot_extreme <- eco_metrics %>%
  filter(
    threshold == "Extreme"
  )


################################################################################
#### SAVE ECOREGION FIGURE DATA
################################################################################

# Join total forest area onto the disturbance metrics used in the figure.

ecoregion_figure_data <- eco_metrics %>%
  
  left_join(
    forest_area %>%
      select(
        ecoregion_id,
        forest_area_km2
      ),
    by = "ecoregion_id"
  ) %>%
  
  # Convert factors back to readable character values for CSV output
  mutate(
    ecoregion_name = as.character(ecoregion_name),
    disturbance_label = as.character(disturbance_label)
  ) %>%
  
  # Keep only variables relevant to the figure
  select(
    ecoregion_id,
    ecoregion_name,
    forest_area_km2,
    threshold,
    disturbance,
    disturbance_label,
    area_km2,
    percent_forest_ecoregion
  ) %>%
  
  # Preserve figure ordering
  mutate(
    ecoregion_order = match(
      ecoregion_name,
      ecoregion_order
    )
  ) %>%
  
  arrange(
    ecoregion_order,
    factor(
      threshold,
      levels = c("Any", "Extreme")
    ),
    factor(
      disturbance,
      levels = disturbances_keep
    )
  )


# Save
write_csv(
  ecoregion_figure_data,
  "data/derived/spatial_metrics/ecoregion_disturbance_figure_data.csv"
)


################################################################################
#### COLOR SCALE
################################################################################

# Square-root transformation emphasizes variation at lower percentages
# while retaining true zero values.

disturbance_fill_scale <- scale_fill_viridis_c(
  
  option = "magma",
  direction = -1,
  
  trans = "sqrt",
  
  limits = c(
    0,
    100
  ),
  
  breaks = c(
    0,
    1,
    5,
    10,
    25,
    50,
    100
  ),
  
  labels = c(
    "0",
    "1",
    "5",
    "10",
    "25",
    "50",
    "100"
  ),
  
  name = "Forest area disturbed (%)",
  
  guide = guide_colorbar(
    direction = "horizontal",
    title.position = "top",
    title.hjust = 0.5,
    barwidth = unit(5, "cm"),
    barheight = unit(0.25, "cm")
  )
)


################################################################################
#### PANEL A — TOTAL FOREST AREA
################################################################################

# This panel also carries the ecoregion labels for the entire figure.

p_forest <- ggplot(
  forest_area,
  aes(
    x = forest_area_km2,
    y = ecoregion_name
  )
) +
  
  geom_col(
    width = 0.78,
    fill = "grey40"
  ) +
  
  scale_x_continuous(
    
    labels = label_number(
      scale = 1 / 1000,
      suffix = "k",
      accuracy = 1
    ),
    
    breaks = c(
      0,
      25000,
      50000,
      75000
    ),
    
    expand = expansion(
      mult = c(
        0,
        0.03
      )
    )
    
  ) +
  
  labs(
    title = "Total forest \narea",
    x = "Forest area (km²)",
    y = NULL
  ) +
  
  theme_minimal(
    base_size = 8
  ) +
  
  theme(
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.y = element_text(
      size = 6.5,
      color = "grey30"
    ),
    
    axis.text.x = element_text(
      size = 6.5
    ),
    
    axis.ticks.y = element_blank(),
    
    axis.title.y = element_blank(),
    
    axis.title.x = element_text(
      size = 7
    ),
    
    plot.title = element_text(
      size = 9,
      face = "bold",
      hjust = 0.5,
      margin = margin(
        b = 5
      )
    ),
    
    plot.margin = margin(
      t = 3,
      r = 3,
      b = 3,
      l = 3
    )
    
  )


################################################################################
#### COMMON HEATMAP THEME
################################################################################

heatmap_theme <- theme_minimal(
  base_size = 8
) +
  
  theme(
    
    panel.grid = element_blank(),
    
    axis.title = element_blank(),
    
    # Ecoregion labels appear only beside forest-area panel
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 6.5
    ),
    
    axis.ticks.x = element_blank(),
    
    plot.title = element_text(
      size = 9,
      face = "bold",
      hjust = 0.5,
      margin = margin(
        b = 5
      )
    ),
    
    plot.margin = margin(
      t = 3,
      r = 3,
      b = 3,
      l = 3
    )
    
  )


################################################################################
#### PANEL B — ANY DISTURBANCE THRESHOLDS
################################################################################

p_any <- ggplot(
  plot_any,
  aes(
    x = disturbance_label,
    y = ecoregion_name,
    fill = percent_forest_ecoregion
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.15
  ) +
  
  disturbance_fill_scale +
  
  labs(
    title = "Any disturbance \nthresholds"
  ) +
  
  heatmap_theme


################################################################################
#### PANEL C — EXTREME DISTURBANCE THRESHOLDS
################################################################################

p_extreme <- ggplot(
  plot_extreme,
  aes(
    x = disturbance_label,
    y = ecoregion_name,
    fill = percent_forest_ecoregion
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.15
  ) +
  
  disturbance_fill_scale +
  
  labs(
    title = "Extreme disturbance \nthresholds"
  ) +
  
  heatmap_theme


################################################################################
#### COMBINE HEATMAPS
################################################################################

# First combine Any + Extreme and collect their identical legends.
#
# This creates ONE shared legend for both heatmaps.

p_heatmaps <- (
  p_any +
    p_extreme
) +
  
  plot_layout(
    widths = c(
      1,
      1
    ),
    guides = "collect"
  ) &
  
  theme(
    legend.position = "bottom",
    
    legend.title = element_text(
      size = 7
    ),
    
    legend.text = element_text(
      size = 6.5
    ),
    
    legend.margin = margin(
      t = 0,
      r = 0,
      b = 0,
      l = 0
    )
  )


################################################################################
#### FINAL FIGURE
################################################################################

# Final organization:
#
# Ecoregion labels -> Forest area -> Any -> Extreme
#
# The ecoregion labels are technically part of p_forest, but visually form
# the left-hand label column for the entire figure.

p_ecoregions <- (
  
  p_forest +
    p_heatmaps
  
) +
  
  plot_layout(
    widths = c(
      0.65,
      2
    )
  )


################################################################################
#### DISPLAY FIGURE
################################################################################

p_ecoregions


################################################################################
#### SAVE FIGURE
################################################################################

ggsave(
  
  filename = file.path(
    fig_dir,
    "ecoregion_disturbance_heatmap.png"
  ),
  
  plot = p_ecoregions,
  
  width = 7,
  height = 8,
  
  units = "in",
  
  dpi = 300,
  
  bg = "white"
  
)
