### Forest-disturbance-stack-v3

### This script uses previously developed annual bias corrected CBI rasters from 
### Welty and Jeffries fire polygons and creates a full stack to be used in 
### later steps. This script can be run on a local machine in 1-2 hours.

### Matt Bitters
### matthew.bitters@colorado.edu



# Install and load required packages
packages <- c("here", "terra")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) {
  install.packages(packages[!installed])
}

library(here)
library(terra)



################################################################################



# List files in directory
cbi_files <- list.files(
  "data/raw/bccbi_welty_west/",
  pattern = "\\.tif$",
  full.names = TRUE
)

# Inspect directory
length(cbi_files)
head(cbi_files)


# Look at an example
cbi_2000 <- rast(cbi_files[grep("2010", cbi_files)])

cbi_2000

datatype(cbi_2000)

freq(cbi_2000)

plot(cbi_2000, main = "CBI severity (example year)")



################################################################################



# Directory with CBI rasters
cbi_dir <- here("data", "raw", "bccbi_welty_west")

# List files in chronological order
cbi_files <- list.files(cbi_dir, pattern = "\\.tif$", full.names = TRUE)
cbi_files <- sort(cbi_files)  # ensures 2000–2020 order

# Stack all rasters
cbi_stack <- rast(cbi_files)

# Rename layers
years <- 2000:2020
names(cbi_stack) <- paste0("wildfire_cbi_", years)

# Save compressed stack
writeRaster(cbi_stack,
            "data/derived/wildfire_cbi.tif",
            datatype = "FLT4S",           # keep continuous float
            overwrite = TRUE,
            gdal = c("COMPRESS=DEFLATE"))

message("CBI raster stack saved to: data/derived/wildfire_cbi.tif")

# Load raster stack and check names
fire_stack_test <- rast(here("data", "derived", "wildfire_cbi.tif"))
names(fire_stack_test)



