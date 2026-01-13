### Forest-disturbance-stack-v3

### 

### Matt Bitters
### matthew.bitters@colorado.edu

################################################################################

# Working directory
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

# Packages
packages <- c("here", "terra")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) install.packages(packages[!installed])

library(here)
library(terra)



terraOptions(
  memfrac = 0.4,    # use 40% of RAM
  memmax = 180000   # cap memory at 180 GB
)


################################################################################

### Load annual raster stacks

# List all raster stacks in directory
raster_files <- list.files("data/derived/annual_stacks/", pattern = "tif$", full.names = TRUE)
raster_files

# Load all stacks as a list
stacks <- lapply(raster_files, rast)
stacks

################################################################################

### Binarize disturbance layers
### *Change thresholds as you want

# If wildfire exceeds 0.1 CBI
wildfire_all <- lapply(stacks, function(x) x[[1]] > 0.1)

# If biotic exceeds 10%
biotic_all   <- lapply(stacks, function(x) x[[2]] > 10)

# If hotter drought exceeds 4.9
hd_all  <- lapply(stacks, function(x) x[[3]] > 4.9)

# If pdsi exceeds 4499
pdsi_all  <- lapply(stacks, function(x) x[[4]] > 4499)

################################################################################

# Calculate km² burned per year
pixel_area_km2 <- prod(res(stacks[[1]])) / 1e6

###


area_burned <- sapply(wildfire_all, function(x) global(x, "sum", na.rm=TRUE) * pixel_area_km2)
area_burned

