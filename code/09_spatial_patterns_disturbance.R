### Forest-disturbance-stack-v3

### 

### Matt Bitters
### matthew.bitters@colorado.edu

################################################################################

# Working directory
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

# Packages
packages <- c("here", "terra", "sf", "dplyr", "ggplot2", "patchwork")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) install.packages(packages[!installed])

library(here)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)


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
  wf_extr = "data/derived/annual_stacks/binary/spatial_presence/any/wf_extreme_presence.tif",
  bt_extr = "data/derived/annual_stacks/binary/spatial_presence/any/bt_extreme_presence.tif",
  hd_extr = "data/derived/annual_stacks/binary/spatial_presence/any/hd_extreme_presence.tif",
  pd_extr = "data/derived/annual_stacks/binary/spatial_presence/any/pd_extreme_presence.tif",

  # Doubles
  wf_bt_extr = "data/derived/annual_stacks/binary/spatial_presence/any/wf_bt_extreme_presence.tif",
  wf_hd_extr = "data/derived/annual_stacks/binary/spatial_presence/any/wf_hd_extreme_presence.tif",
  wf_pd_extr = "data/derived/annual_stacks/binary/spatial_presence/any/wf_pd_extreme_presence.tif",
  bt_hd_extr = "data/derived/annual_stacks/binary/spatial_presence/any/bt_hd_extreme_presence.tif",
  bt_pd_extr = "data/derived/annual_stacks/binary/spatial_presence/any/bt_pd_extreme_presence.tif",

  # Triples
  wf_bt_hd_extr = "data/derived/annual_stacks/binary/spatial_presence/any/wf_bt_hd_extreme_presence.tif",
  wf_bt_pd_extr = "data/derived/annual_stacks/binary/spatial_presence/any/wf_bt_pd_extreme_presence.tif"
)

################################################################################

### Load and aggregate rasters

# Aggregation factor
agg_factor <- 100  # reduce by 100x in each dimension for plotting

# Load forest mask
forest <- rast(forest_mask)
forest_ds <- aggregate(forest, fact=agg_factor, fun=max)

# Load all disturbance rasters into a named list
dist_rasters <- lapply(raster_paths, function(p) {
  r <- rast(p)
  aggregate(r, fact=agg_factor, fun=max)
})

################################################################################

### Convert all rasters to a single tidy dataframe

# Start with forest mask
df <- as.data.frame(forest_ds, xy = TRUE)
colnames(df) <- c("x", "y", "forest")

# Add all disturbances
for (nm in names(dist_rasters)) {
  df[[nm]] <- as.vector(dist_rasters[[nm]])
}

# Convert to long format for ggplot if desired
df_long <- df %>%
  pivot_longer(
    cols = -c(x, y, forest),
    names_to = "disturbance",
    values_to = "presence"
  )

# Load and prepare ecoregions for overlay
eco <- st_read(eco_shp)
eco <- st_transform(eco, crs = crs(forest_ds))




# Plot wildfire
ggplot() +
  # Forest background
  geom_raster(data = df, aes(x = x, y = y), fill = "lightgray", alpha = 0.5) +
  # Any wildfire
  geom_raster(data = df %>% filter(wf_any == 1), aes(x = x, y = y), fill = "orange", alpha = 0.5) +
  # Extreme wildfire overlay
  geom_raster(data = df %>% filter(wf_extreme == 1), aes(x = x, y = y), fill = "red", alpha = 0.7) +
  # Ecoregion boundaries
  geom_sf(data = eco, fill = NA, color = "black", size = 0.2) +
  theme_minimal() +
  labs(title = "Wildfire: Any vs Extreme")





























# Quick base plot for each raster
plot(wf_pa, main="Wildfire Presence (Any)")
plot(bt_pa, main="Biotic Disturbance Presence (Any)")
plot(hd_pa, main="Hotter Drought Presence (Any)")
plot(pd_pa, main="PDSI Disturbance Presence (Any)")


# Optional: Combine rasters into a stack
dist_stack <- c(wf_pa, bt_pa, hd_pa, pd_pa)
names(dist_stack) <- c("WF", "BT", "HD", "PD")

# Visualize multiple layers using terra::plot
plot(dist_stack, col=c("white","red"), main=names(dist_stack))

# Plot with forest mask
plot(forest_mask, col=gray.colors(10, start=0.9, end=0.3), legend=FALSE)
plot(wf_pa, col=adjustcolor("red", alpha.f=0.5), add=TRUE)
plot(bt_pa, col=adjustcolor("green", alpha.f=0.5), add=TRUE)
plot(hd_pa, col=adjustcolor("orange", alpha.f=0.5), add=TRUE)
plot(pd_pa, col=adjustcolor("blue", alpha.f=0.5), add=TRUE)


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


