### Forest-disturbance-stack-v3

### 

### Matt Bitters
### matthew.bitters@colorado.edu

################################################################################

# Working directory
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

# Packages
packages <- c("here", "terra", "sf", "dplyr", "ggplot2", "patchwork", "tidyr", "ragg")
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


# Spatial presence rasters
raster_paths <- list(
  ### Any disturbance
  
  # Singles
  wf_any = "data/derived/annual_stacks/binary/spatial_presence/any/wf_any_presence.tif",
  bt_any = "data/derived/annual_stacks/binary/spatial_presence/any/bt_any_presence.tif",
  hd_any = "data/derived/annual_stacks/binary/spatial_presence/any/hd_any_presence.tif",
  pd_any = "data/derived/annual_stacks/binary/spatial_presence/any/pd_any_presence.tif",

  # Doubles
  wf_bt_any = "data/derived/annual_stacks/binary/spatial_presence/any/wf_bt_any_presence.tif",
  wf_hd_any = "data/derived/annual_stacks/binary/spatial_presence/any/wf_hd_any_presence.tif",
  wf_pd_any = "data/derived/annual_stacks/binary/spatial_presence/any/wf_pd_any_presence.tif",
  bt_hd_any = "data/derived/annual_stacks/binary/spatial_presence/any/bt_hd_any_presence.tif",
  bt_pd_any = "data/derived/annual_stacks/binary/spatial_presence/any/bt_pd_any_presence.tif",

  # Triples
  wf_bt_hd_any = "data/derived/annual_stacks/binary/spatial_presence/any/wf_bt_hd_any_presence.tif",
  wf_bt_pd_any = "data/derived/annual_stacks/binary/spatial_presence/any/wf_bt_pd_any_presence.tif",

  ### Extreme disturbance

  # Singles
  wf_extr = "data/derived/annual_stacks/binary/spatial_presence/extreme/wf_extreme_presence.tif",
  bt_extr = "data/derived/annual_stacks/binary/spatial_presence/extreme/bt_extreme_presence.tif",
  hd_extr = "data/derived/annual_stacks/binary/spatial_presence/extreme/hd_extreme_presence.tif",
  pd_extr = "data/derived/annual_stacks/binary/spatial_presence/extreme/pd_extreme_presence.tif",

  # Doubles
  wf_bt_extr = "data/derived/annual_stacks/binary/spatial_presence/extreme/wf_bt_extreme_presence.tif",
  wf_hd_extr = "data/derived/annual_stacks/binary/spatial_presence/extreme/wf_hd_extreme_presence.tif",
  wf_pd_extr = "data/derived/annual_stacks/binary/spatial_presence/extreme/wf_pd_extreme_presence.tif",
  bt_hd_extr = "data/derived/annual_stacks/binary/spatial_presence/extreme/bt_hd_extreme_presence.tif",
  bt_pd_extr = "data/derived/annual_stacks/binary/spatial_presence/extreme/bt_pd_extreme_presence.tif",

  # Triples
  wf_bt_hd_extr = "data/derived/annual_stacks/binary/spatial_presence/extreme/wf_bt_hd_extreme_presence.tif",
  wf_bt_pd_extr = "data/derived/annual_stacks/binary/spatial_presence/extreme/wf_bt_pd_extreme_presence.tif"
)

################################################################################

# Load forest mask as raster
forest <- rast(forest_mask)

# Aggregation factor for faster plotting
agg_factor <- 5

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

# ===========================================================
# Function to prepare raster for plotting
# ===========================================================
raster_to_df <- function(r_path) {
  r <- rast(r_path)
  r_agg <- aggregate(r, fact = agg_factor, fun = max)   
  df <- as.data.frame(r_agg, xy = TRUE)
  colnames(df) <- c("x", "y", "presence")
  df <- df %>% filter(presence == 1)
  return(df)
}

# Prepare all raster dfs
raster_dfs <- lapply(raster_paths, raster_to_df)

# ===========================================================
# Base map layers
# ===========================================================
base_map <- list(
  geom_raster(data = df_forest, aes(x = x, y = y),
              fill = "grey88", alpha = 1),
  coord_sf(
    xlim = c(west_extent["xmin"], west_extent["xmax"]),
    ylim = c(west_extent["ymin"], west_extent["ymax"])
  ),
  theme_void(),
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA))
)


# ===========================================================
# Four-panel plot
# ===========================================================
p_wf <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs$wf_any, aes(x = x, y = y), fill = "orangered1", alpha = 0.7) +
  geom_raster(data = raster_dfs$wf_extr, aes(x = x, y = y), fill = "orangered4", alpha = 0.9) +
  geom_sf(data = west_eco, fill = NA, color = "black", size = 0.15) +
  labs(title = "wf")

p_bt <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs$bt_any, aes(x = x, y = y), fill = "khaki1", alpha = 0.7) +
  geom_raster(data = raster_dfs$bt_extr, aes(x = x, y = y), fill = "khaki4", alpha = 0.9) +
  geom_sf(data = west_eco, fill = NA, color = "black", size = 0.15) +
  labs(title = "bt")

p_hd <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs$hd_any, aes(x = x, y = y), fill = "dodgerblue1", alpha = 0.7) +
  geom_raster(data = raster_dfs$hd_extr, aes(x = x, y = y), fill = "dodgerblue4", alpha = 0.9) +
  geom_sf(data = west_eco, fill = NA, color = "black", size = 0.15) +
  labs(title = "hd")

p_combo <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs$wf_bt_hd_any, aes(x = x, y = y), fill = "darkorchid1", alpha = 0.7) +
  geom_raster(data = raster_dfs$wf_bt_hd_extr, aes(x = x, y = y), fill = "darkorchid4", alpha = 0.9) +
  geom_sf(data = west_eco, fill = NA, color = "black", size = 0.15) +
  labs(title = "wf, bt, hd")

# Combine panels
four_panel <- p_wf | p_bt | p_hd | p_combo 

# Display
four_panel

# Save
ggsave(
  filename = "figs/spatial_patterns/spatial_patterns_four_panel.png",
  plot     = four_panel,
  width    = 6,        # inches (adjust as needed)
  height   = 4,         # inches (single-row layout)
  dpi      = 300,
  units    = "in",
  bg       = "white"
)

# ===========================================================
# Eight-panel plot
# ===========================================================
p_wf_any <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs$wf_any, aes(x = x, y = y), fill = "orangered1", alpha = 0.7) +
  geom_sf(data = west_eco, fill = NA, color = "black", size = 0.1) +
  labs(title = "wf")

p_bt_any <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs$bt_any, aes(x = x, y = y), fill = "khaki1", alpha = 0.7) +
  geom_sf(data = west_eco, fill = NA, color = "black", size = 0.1) +
  labs(title = "bt")

p_hd_any <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs$hd_any, aes(x = x, y = y), fill = "dodgerblue1", alpha = 0.7) +
  geom_sf(data = west_eco, fill = NA, color = "black", size = 0.1) +
  labs(title = "hd")

p_combo_any <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs$wf_hd_any, aes(x = x, y = y), fill = "firebrick1", alpha = 0.7) +
  geom_raster(data = raster_dfs$bt_hd_any, aes(x = x, y = y), fill = "darkolivegreen1", alpha = 0.7) +
  geom_raster(data = raster_dfs$wf_bt_any, aes(x = x, y = y), fill = "darkorchid1", alpha = 0.7) +
  geom_raster(data = raster_dfs$wf_bt_hd_any, aes(x = x, y = y), fill = "darkgoldenrod1", alpha = 0.7) +
  geom_sf(data = west_eco, fill = NA, color = "black", size = 0.1) +
  labs(title = "int")


p_wf_extr <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs$wf_extr, aes(x = x, y = y), fill = "orangered4", alpha = 0.7) +
  geom_sf(data = west_eco, fill = NA, color = "black", size = 0.1)
  
p_bt_extr <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs$bt_extr, aes(x = x, y = y), fill = "khaki4", alpha = 0.7) +
  geom_sf(data = west_eco, fill = NA, color = "black", size = 0.1)
  
p_hd_extr <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs$hd_extr, aes(x = x, y = y), fill = "dodgerblue4", alpha = 0.7) +
  geom_sf(data = west_eco, fill = NA, color = "black", size = 0.1)
  
p_combo_extr <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs$wf_hd_extr, aes(x = x, y = y), fill = "firebrick4", alpha = 0.7) +
  geom_raster(data = raster_dfs$bt_hd_extr, aes(x = x, y = y), fill = "darkolivegreen4", alpha = 0.7) +
  geom_raster(data = raster_dfs$wf_bt_extr, aes(x = x, y = y), fill = "darkorchid4", alpha = 0.7) +
  geom_raster(data = raster_dfs$wf_bt_hd_extr, aes(x = x, y = y), fill = "darkgoldenrod4", alpha = 0.7) +
  geom_sf(data = west_eco, fill = NA, color = "black", size = 0.1)
  



# Combine panels
eight_panel <- (p_wf_any | p_bt_any | p_hd_any | p_combo_any) / 
               (p_wf_extr | p_bt_extr | p_hd_extr | p_combo_extr) +
  theme(plot.margin = margin(.1, 1, .1, 1))
  
  
# Display
eight_panel
  
# Save
ggsave(
  filename = "figs/spatial_patterns/spatial_patterns_eight_panel.png",
  plot     = eight_panel,
  width    = 6,        # inches (adjust as needed)
  height   = 8,         # inches (single-row layout)
  dpi      = 300,
  units    = "in",
  bg       = "white"
)




























forest_mask <- rast("data/derived/resampled/forest_mask_30m_resampled.tif")

wf_any = rast("data/derived/annual_stacks/binary/spatial_presence/any/wf_any_presence.tif")
bt_any = rast("data/derived/annual_stacks/binary/spatial_presence/any/bt_any_presence.tif")
hd_any = rast("data/derived/annual_stacks/binary/spatial_presence/any/hd_any_presence.tif")

wf_extr = rast("data/derived/annual_stacks/binary/spatial_presence/extreme/wf_extreme_presence.tif")
bt_extr = rast("data/derived/annual_stacks/binary/spatial_presence/extreme/bt_extreme_presence.tif")
hd_extr = rast("data/derived/annual_stacks/binary/spatial_presence/extreme/hd_extreme_presence.tif")


# Quick base plot for each raster
plot(wf_any, main="Wildfire Presence (Any)")
plot(bt_any, main="Biotic Disturbance Presence (Any)")
plot(hd_any, main="Hotter Drought Presence (Any)")





# Plot
# Forest mask
plot(forest_mask, col=gray.colors(10, start=0.9, end=0.3), legend=FALSE)
# Any
plot(wf_any, col=adjustcolor("orange", alpha.f=0.5), add=TRUE, legend=FALSE)
# Extreme
plot(wf_extr, col=adjustcolor("orangered2", alpha.f=0.7), add=TRUE, legend=FALSE)



plot(bt_any, col=adjustcolor("green", alpha.f=0.5), add=TRUE)

plot(hd_any, col=adjustcolor("orange", alpha.f=0.5), add=TRUE)


### Plot with ggplot

# Downsample by factor of 100 (adjust as needed)
forest_ds <- aggregate(forest_mask, fact=100, fun=mean)
wf_ds <- aggregate(wf_pa, fact=100, fun=max)
bt_ds <- aggregate(bt_pa, fact=100, fun=max)
hd_ds <- aggregate(hd_pa, fact=100, fun=max)
pd_ds <- aggregate(pd_pa, fact=100, fun=max)
































################################################################################



