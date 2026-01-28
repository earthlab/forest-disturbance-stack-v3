### Forest-disturbance-stack-v3

### 

### Matt Bitters
### matthew.bitters@colorado.edu

################################################################################

# Working directory
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

# Packages
packages <- c("here", "terra", "sf", "dplyr", "ggplot2", "patchwork", "tidyr")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) install.packages(packages[!installed])

library(here)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)


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
agg_factor <- 2

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
              fill = "lightgray", alpha = 0.4),
  geom_sf(data = west_eco, fill = NA, color = "black", size = 0.15),
  coord_sf(
    xlim = c(west_extent["xmin"], west_extent["xmax"]),
    ylim = c(west_extent["ymin"], west_extent["ymax"])
  ),
  theme_void()
)


# ===========================================================
# Four-panel plot
# ===========================================================
p_wf <- ggplot() +
  base_map +
  geom_tile(data = raster_dfs$wf_any, aes(x = x, y = y), fill = "orangered", alpha = 0.7) +
  geom_tile(data = raster_dfs$wf_extr, aes(x = x, y = y), fill = "orangered4", alpha = 0.9) +
  labs(title = "wf")

p_bt <- ggplot() +
  base_map +
  geom_tile(data = raster_dfs$bt_any, aes(x = x, y = y), fill = "tan", alpha = 0.7) +
  geom_tile(data = raster_dfs$bt_extr, aes(x = x, y = y), fill = "tan4", alpha = 0.9) +
  labs(title = "bt")

p_hd <- ggplot() +
  base_map +
  geom_tile(data = raster_dfs$hd_any, aes(x = x, y = y), fill = "coral", alpha = 0.7) +
  geom_tile(data = raster_dfs$hd_extr, aes(x = x, y = y), fill = "coral4", alpha = 0.9) +
  labs(title = "hd")

p_combo <- ggplot() +
  base_map +
  geom_tile(data = raster_dfs$wf_bt_hd_any, aes(x = x, y = y), fill = "royalblue", alpha = 0.7) +
  geom_tile(data = raster_dfs$wf_bt_hd_extr, aes(x = x, y = y), fill = "royalblue4", alpha = 0.9) +
  labs(title = "wf, bt, hd")

# Combine panels
four_panel <- p_wf | p_bt | p_hd | p_combo +
  plot_annotation(
    title = "Spatial Presence of Disturbance (2000–2020)",
    theme = theme(plot.title = element_text(size = 12, face = "bold"))
  )

# Display
four_panel







































wf_any = rast("data/derived/annual_stacks/binary/spatial_presence/any/wf_any_presence.tif")
bt_any = rast("data/derived/annual_stacks/binary/spatial_presence/any/bt_any_presence.tif")
hd_any = rast("data/derived/annual_stacks/binary/spatial_presence/any/hd_any_presence.tif")




# Quick base plot for each raster
plot(wf_any, main="Wildfire Presence (Any)")
plot(bt_any, main="Biotic Disturbance Presence (Any)")
plot(hd_any, main="Hotter Drought Presence (Any)")





# Plot
# Forest mask
plot(forest_mask, col=gray.colors(10, start=0.9, end=0.3), legend=FALSE)
# Any
plot(wf_any, col=adjustcolor("red", alpha.f=0.5), add=TRUE)
# Extreme




plot(bt_any, col=adjustcolor("green", alpha.f=0.5), add=TRUE)

plot(hd_any, col=adjustcolor("orange", alpha.f=0.5), add=TRUE)

wf_extr = "data/derived/annual_stacks/binary/spatial_presence/extreme/wf_extreme_presence.tif",
bt_extr = "data/derived/annual_stacks/binary/spatial_presence/extreme/bt_extreme_presence.tif",
hd_extr = "data/derived/annual_stacks/binary/spatial_presence/extreme/hd_extreme_presence.tif",
### Plot with ggplot

# Downsample by factor of 100 (adjust as needed)
forest_ds <- aggregate(forest_mask, fact=100, fun=mean)
wf_ds <- aggregate(wf_pa, fact=100, fun=max)
bt_ds <- aggregate(bt_pa, fact=100, fun=max)
hd_ds <- aggregate(hd_pa, fact=100, fun=max)
pd_ds <- aggregate(pd_pa, fact=100, fun=max)
































################################################################################

### Load spatial presence/absence rasters, forest mask, ecoregions

# Directory
pa_dir <- "data/derived/annual_stacks/spatial_pa"

# Forest mask (for stats only, not plotting)
forest_mask <- rast("data/derived/resampled/forest_mask_30m_resampled.tif")

# EPA Level III ecoregions
#eco <- st_read("data/derived/vector/epa_level3_west.gpkg") |>
#  st_transform(5070)

# Rasters

# Singles
wf <- rast(file.path(pa_dir, "wildfire.tif"))
bt <- rast(file.path(pa_dir, "biotic.tif"))
hd <- rast(file.path(pa_dir, "hd.tif"))
pdsi <- rast(file.path(pa_dir, "pdsi.tif"))

# Doubles
wf_bt <- rast(file.path(pa_dir, "fire_biotic.tif"))
wf_hd <- rast(file.path(pa_dir, "fire_hd.tif"))
wf_pdsi <- rast(file.path(pa_dir, "fire_pdsi.tif"))
bt_hd <- rast(file.path(pa_dir, "biotic_hd.tif"))
bt_pdsi <- rast(file.path(pa_dir, "biotic_pdsi.tif"))

# Triples
wf_bt_hd <- rast(file.path(pa_dir, "fire_biotic_hd.tif"))
wf_bt_pdsi <- rast(file.path(pa_dir, "fire_biotic_pdsi.tif"))

plot(wf)
################################################################################

### Downsample for plotting

# Using temp folder
wf_pa <- app(
  wf,
  fun = function(x) as.integer(!is.na(x)),
  filename = "tmp_terra/wildfire_pa.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "Byte",
    gdal = c("COMPRESS=DEFLATE", "TILED=YES")
  )
)

wf_plot <- aggregate(
  !is.na(wf),   # convert to TRUE/FALSE
  fact = 50,
  fun  = max
)
ncell(wf_plot)




# Convert downsampled raster to data frame from ggplot plotting
wf_df <- as.data.frame(wf_plot, xy = TRUE, na.rm = TRUE)
names(wf_df)[3] <- "presence"



ggplot(wf_df) +
  geom_raster(aes(x = x, y = y, fill = factor(presence))) +
  #geom_sf(data = eco, fill = NA, color = "grey40", linewidth = 0.2) +
  scale_fill_manual(
    values = c("0" = "white", "1" = "#d73027"),
    name = "Wildfire"
  ) +
  coord_sf(crs = st_crs(5070)) +
  theme_minimal() +
  theme(panel.grid = element_blank())




wf_plot


