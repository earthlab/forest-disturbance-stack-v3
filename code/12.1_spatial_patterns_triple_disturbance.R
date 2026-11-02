
################################################################################
# Spatial patterns of triple disturbance interactions
# Wildfire + Biotic disturbance + Hotter drought
# Western U.S. forests, 2000–2020
################################################################################

# Working directory
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

# Packages
packages <- c(
  "here", "terra", "sf", "dplyr", "ggplot2",
  "ragg", "ggspatial", "tigris"
)

installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) install.packages(packages[!installed])

library(here)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(ragg)
library(ggspatial)
library(tigris)

dir.create("tmp_terra", showWarnings = FALSE)

terraOptions(
  tempdir = "tmp_terra",
  memfrac = 0.6,
  memmax = 180000,
  progress = 1
)

################################################################################
# 1. Paths
################################################################################

forest_mask <- "data/derived/resampled/forest_mask_30m_resampled.tif"

raster_paths <- list(
  any = paste0(
    "data/derived/annual_stacks/binary/spatial_presence/any/",
    "wf_bt_hd_any_presence.tif"
  ),
  extreme = paste0(
    "data/derived/annual_stacks/binary/spatial_presence/extreme/",
    "wf_bt_hd_extreme_presence.tif"
  )
)

figure_dir <- "figures/spatial_patterns"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

################################################################################
# 2. Visualization settings
################################################################################

# Disturbance color
color_any <- "#7A116A"

# State boundary appearance
state_border_color <- "grey45"
state_border_width <- 0.25

# Colorado boundary appearance
co_border_color <- "black"
co_border_width <- 1.0

# Raster transparency
raster_alpha <- 0.85

################################################################################
# 3. Load western U.S. state boundaries
################################################################################

west_states <- c(
  "California", "Oregon", "Washington", "Idaho",
  "Nevada", "Utah", "Arizona", "Montana",
  "Wyoming", "Colorado", "New Mexico"
)

# Download Census state boundaries
states <- tigris::states(
  cb = TRUE,
  year = 2024,
  class = "sf"
)

# Filter to western states
west_states_sf <- states %>%
  filter(NAME %in% west_states) %>%
  st_transform(5070)

# Separate Colorado for bold border
colorado_sf <- west_states_sf %>%
  filter(NAME == "Colorado")

# Bounding box for plotting
west_extent <- st_bbox(west_states_sf)

################################################################################
# 4. Prepare forest background
################################################################################

forest <- rast(forest_mask)

# Aggregate to 300 m for visualization
forest_ds <- aggregate(
  forest,
  fact = plot_agg_factor,
  fun = max,
  na.rm = TRUE
)

forest_df <- as.data.frame(forest_ds, xy = TRUE)

colnames(forest_df) <- c("x", "y", "presence")

forest_df <- forest_df %>%
  filter(presence == 1)

################################################################################
# 5. Function to aggregate and smooth disturbance rasters
################################################################################

raster_to_df <- function(
    r_path,
    agg_factor = plot_agg_factor,
    window_size = focal_window,
    threshold = smooth_threshold
) {
  
  # Load original 30 m binary raster
  r <- rast(r_path)
  
  # Aggregate to plotting resolution
  # Mean = proportion of disturbed 30 m cells
  r_agg <- aggregate(
    r,
    fact = agg_factor,
    fun = mean,
    na.rm = TRUE
  )
  
  # Focal smoothing (same as original script)
  w <- matrix(
    1 / (window_size^2),
    nrow = window_size,
    ncol = window_size
  )
  
  r_smooth <- focal(
    r_agg,
    w = w,
    fun = sum,
    na.rm = TRUE,
    fillvalue = NA
  )
  
  # Retain areas above smoothing threshold
  r_presence <- ifel(
    r_smooth >= threshold,
    1,
    NA
  )
  
  # Convert to plotting dataframe
  df <- as.data.frame(
    r_presence,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(df) <- c("x", "y", "value")
  
  return(df)
}

################################################################################
# 6. Prepare Any triple disturbance data
################################################################################

any_df <- raster_to_df(raster_paths$any)

################################################################################
# 7. Create map
################################################################################

p_triple <- ggplot() +
  
  # Forest background
  geom_raster(
    data = forest_df,
    aes(x = x, y = y),
    fill = "grey95"
  ) +
  
  # Any triple disturbance
  geom_raster(
    data = any_df,
    aes(x = x, y = y),
    fill = color_any,
    alpha = raster_alpha
  ) +
  
  # Western state boundaries
  geom_sf(
    data = west_states_sf,
    fill = NA,
    color = state_border_color,
    linewidth = state_border_width
  ) +
  
  # Bold Colorado boundary
  geom_sf(
    data = colorado_sf,
    fill = NA,
    color = co_border_color,
    linewidth = co_border_width
  ) +
  
  # Map extent
  coord_sf(
    crs = st_crs(5070),
    xlim = c(west_extent["xmin"], west_extent["xmax"]),
    ylim = c(west_extent["ymin"], west_extent["ymax"]),
    expand = FALSE
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.20,
    text_cex = 0.7,
    pad_x = grid::unit(0.3, "cm"),
    pad_y = grid::unit(0.3, "cm")
  ) +
  
  # Compass rose
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      text_size = 8
    ),
    height = grid::unit(0.8, "cm"),
    width = grid::unit(0.8, "cm"),
    pad_x = grid::unit(0.3, "cm"),
    pad_y = grid::unit(0.8, "cm")
  ) +
  
  labs(
    title = "Wildfire, beetle kill, and drought \noverlap (2000-2020)"
  ) +
  
  theme_void(base_size = 12) +
  
  theme(
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 10
    ),
    
    plot.subtitle = element_text(
      size = 11
    ),
    
    legend.position = "none",
    
    plot.margin = margin(10, 10, 10, 10)
  )

################################################################################
# 8. Display and save
################################################################################

print(p_triple)

ggsave(
  filename = file.path(
    figure_dir,
    "triple_disturbance_any_spatial_patterns.png"
  ),
  plot = p_triple,
  width = 3,
  height = 4,
  dpi = 300,
  bg = "white"
)

