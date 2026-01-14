### Forest-disturbance-stack-v3

### 

### Matt Bitters
### matthew.bitters@colorado.edu

################################################################################

# Working directory
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

# Packages
packages <- c("here", "terra", "sf", "dplyr", "ggplot2")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) install.packages(packages[!installed])

library(here)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)


terraOptions(
  tempdir = "tmp_terra",
  memfrac = 0.6,    # use 60% of RAM
  memmax = 180000,   # cap memory at 180 GB
  progress = 1
)


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


