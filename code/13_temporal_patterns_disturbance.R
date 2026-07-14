### Forest-disturbance-stack-v3

### Temporal patterns of disturbance

### Matt Bitters
### matthew.bitters@colorado.edu

################################################################################

# Working directory
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

# Packages
packages <- c("here", "terra", "sf", "dplyr", "ggplot2", "patchwork", "tidyr", "ragg", "ggspatial", "viridis")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) install.packages(packages[!installed])

library(here)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(ragg)
library(ggspatial)
library(viridis)


terraOptions(
  tempdir = "tmp_terra",
  memfrac = 0.6,    # use 60% of RAM
  memmax = 180000,   # cap memory at 180 GB
  progress = 1
)

################################################################################

###### Paths

# Forest mask path
forest_mask <- "data/derived/resampled/forest_mask_30m_resampled.tif"

# Ecoregion polygons
eco_shp <- "data/raw/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp"


# Frequency rasters
raster_paths_fr <- list(
  ### Any disturbance
  
  # Singles
  wf_any_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/any/wf_any_frequency.tif",
  bt_any_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/any/bt_any_frequency.tif",
  hd_any_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/any/hd_any_frequency.tif",
  pd_any_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/any/pd_any_frequency.tif",
  
  # Doubles
  wf_bt_any_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/any/wf_bt_any_frequency.tif",
  wf_hd_any_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/any/wf_hd_any_frequency.tif",
  wf_pd_any_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/any/wf_pd_any_frequency.tif",
  bt_hd_any_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/any/bt_hd_any_frequency.tif",
  bt_pd_any_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/any/bt_pd_any_frequency.tif",
  
  # Triples
  wf_bt_hd_any_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/any/wf_bt_hd_any_frequency.tif",
  wf_bt_pd_any_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/any/wf_bt_pd_any_frequency.tif",
  
  ### Extreme disturbance
  
  # Singles
  wf_extr_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/extreme/wf_extreme_frequency.tif",
  bt_extr_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/extreme/bt_extreme_frequency.tif",
  hd_extr_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/extreme/hd_extreme_frequency.tif",
  pd_extr_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/extreme/pd_extreme_frequency.tif",
  
  # Doubles
  wf_bt_extr_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/extreme/wf_bt_extreme_frequency.tif",
  wf_hd_extr_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/extreme/wf_hd_extreme_frequency.tif",
  wf_pd_extr_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/extreme/wf_pd_extreme_frequency.tif",
  bt_hd_extr_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/extreme/bt_hd_extreme_frequency.tif",
  bt_pd_extr_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/extreme/bt_pd_extreme_frequency.tif",
  
  # Triples
  wf_bt_hd_extr_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/extreme/wf_bt_hd_extreme_frequency.tif",
  wf_bt_pd_extr_fr = "data/derived/derived/annual_stacks/binary/frequency/frequency/extreme/wf_bt_pd_extreme_frequency.tif"
)

################################################################################

# Load forest mask as raster
forest <- rast(forest_mask)

# Aggregation factor for faster plotting
agg_factor <- 10

# Focal smoothing window, measured in aggregated pixels
# With 30 m input pixels and agg_factor = 10:
# each aggregated pixel is approximately 300 m.
# A 5 x 5 window therefore smooths across approximately 1.5 km.
smooth_window <- 5

# Equal-weight moving-window mean
smooth_weights <- matrix(
  1,
  nrow = smooth_window,
  ncol = smooth_window
)

# Aggregate forest mask
forest_ds <- aggregate(forest, fact = agg_factor, fun = max)  # small enough for plotting

# Convert forest_ds to dataframe and rename columns
forest_df <- as.data.frame(forest_ds, xy = TRUE)
colnames(forest_df) <- c("x", "y", "presence")
forest_df <- forest_df %>% filter(presence == 1)

# Load ecoregions
eco <- st_read(eco_shp)

# Flag western states
west_states <- c("California", "Oregon", "Washington", "Idaho", "Nevada",
                 "Utah", "Arizona", "Montana", "Wyoming", "Colorado", "New Mexico")
eco <- eco %>% mutate(west_flag = ifelse(STATE_NAME %in% west_states, 1, 0))
west_eco <- eco %>% filter(west_flag == 1)

# Dissolve ecoregions (remove state boundaries)
west_eco <- west_eco %>%
  group_by(US_L3CODE) %>%     # or group_by(L3_KEY)
  summarise(geometry = st_union(geometry), .groups = "drop")

# Bounding box for plotting
west_extent <- st_bbox(west_eco)

# Outline of western us for plotting
west_outline <- west_eco |>
  st_union() |>
  st_as_sf()

# ===========================================================
# Function to prepare raster for plotting
# ===========================================================
# ===========================================================
# Function to aggregate and smooth frequency rasters
# ===========================================================
raster_to_df <- function(r_path) {
  
  # Load original 30 m frequency raster
  r <- rast(r_path)
  
  # ---------------------------------------------------------
  # 1. Aggregate for faster plotting
  #
  # Use mean rather than sum so that the resulting value
  # represents average disturbance frequency within each
  # aggregated pixel.
  # ---------------------------------------------------------
  r_agg <- aggregate(
    r,
    fact = agg_factor,
    fun = mean,
    na.rm = TRUE
  )
  
  # ---------------------------------------------------------
  # 2. Smooth the aggregated frequency raster
  #
  # This calculates the mean frequency in the moving window.
  # Zeros remain valid observations during smoothing.
  # ---------------------------------------------------------
  r_smooth <- focal(
    r_agg,
    w = smooth_weights,
    fun = mean,
    na.rm = TRUE,
    na.policy = "omit"
  )
  
  # ---------------------------------------------------------
  # 3. Restrict smoothed values to the aggregated forest mask
  #
  # This prevents smoothing from being displayed outside the
  # mapped forest area.
  # ---------------------------------------------------------
  r_smooth <- mask(
    r_smooth,
    forest_ds,
    maskvalues = 0
  )
  
  # Give the raster layer a consistent name
  names(r_smooth) <- "frequency"
  
  # ---------------------------------------------------------
  # 4. Convert to dataframe for ggplot
  # ---------------------------------------------------------
  df <- as.data.frame(
    r_smooth,
    xy = TRUE,
    na.rm = TRUE
  )
  
  # Only plot locations with smoothed frequency above zero
  df <- df %>%
    filter(frequency > 0)
  
  return(df)
}

# Prepare all raster dfs
raster_dfs_fr <- lapply(raster_paths_fr, raster_to_df)

# ===========================================================
# Base map layers
# ===========================================================
base_map <- list(
  geom_raster(data = forest_df, aes(x = x, y = y),
              fill = "grey95", alpha = 1),
  coord_sf(
    xlim = c(west_extent["xmin"], west_extent["xmax"]),
    ylim = c(west_extent["ymin"], west_extent["ymax"])
  ),
  theme_void(),
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(size = 10),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.4, "cm")
  )
)




# ===========================================================
# Four-panel plot
# ===========================================================

# wf any
p_wf <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs_fr$wf_any_fr, aes(x = x, y = y, fill = frequency)) +
  scale_fill_viridis_c(name = "WF freq", option = "magma", direction = -1, na.value = "transparent") +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  # Add scale bar and compass rose to just wf panel
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    text_cex = 0.4,
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.2, "cm")
  ) +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 6),
    height = unit(0.5, "cm"),
    width  = unit(0.5, "cm"),
    pad_x  = unit(0.2, "cm"),
    pad_y  = unit(0.5, "cm")   # ← this is the key
  )+
  labs(title = "Wildfire") +
  theme(legend.position = "bottom")

# bt any
p_bt <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs_fr$bt_any_fr, aes(x = x, y = y, fill = frequency)) +
  scale_fill_viridis_c(name = "BT freq", option = "magma", direction = -1, na.value = "transparent") +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  labs(title = "Bark beetle tree\nmortality") +
  theme(legend.position = "bottom")

# hd any/extr
p_hd <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs_fr$hd_any_fr, aes(x = x, y = y, fill = frequency)) +
  scale_fill_viridis_c(name = "HD freq", option = "magma", direction = -1, na.value = "transparent") +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  labs(title = "Hotter drought index") +
  theme(legend.position = "bottom")

# wf, bt, hd any/extr
p_wf_bt_hd <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs_fr$wf_bt_hd_any_fr, aes(x = x, y = y, fill = frequency)) +
  scale_fill_viridis_c(name = "WF + BT + HD freq", option = "magma", direction = -1, na.value = "transparent") +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  labs(title = "Disturbance interactions") +
  theme(legend.position = "bottom")


# Combine panels
four_panel <- p_wf | p_bt | p_hd | p_wf_bt_hd 

# Display
four_panel

# Save
ggsave(
  filename = "figs/temporal_patterns/temporal_patterns_four_panel.png",
  plot     = four_panel,
  width    = 7,        # inches 
  height   = 4,         # inches 
  dpi      = 300,
  units    = "in",
  bg       = "white"
)



################################################################################


