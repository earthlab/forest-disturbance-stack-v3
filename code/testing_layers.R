### forest-disturbance-stack-v3

### This script creates a function to create a custom disturbance stack. It accepts
### a template raster path (to determine resolution and extent), a list of
### individual raster file paths (e.g., biotic, wildfire, drought stacks), and a 
### forest mask path (binary mask).
### Returns and saves a final GeoTIFF (.tif).


# If working in cyverse, set working directory and project root
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")   # or any file guaranteed to exist in the project



# Install and load required packages
packages <- c("here", "terra")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) {
  install.packages(packages[!installed])
}

library(here)
library(terra)

# Set cyverse memory max to avoid crashing
terraOptions(memmax=256)



################################################################################



### Usage

# Paths
template_path <- here("data", "derived", "template_30m_2000_2020.tif")

raster_paths <- c(
  here("data", "derived", "wildfire_id.tif"),
  here("data", "derived", "biotic_gridded_1km_all_years_severity.tif"),
  here("data", "derived", "pdsi_annual.tif")
)

forest_mask_path <- here("data", "derived", "relaxed_forest_mask_2000_2020.tif")

output_path <- here("data", "derived", "disturbance_stack_resampled_masked_30m.tif")




##################################################################################


# Load the final raster stack
stack_path <- here("data", "derived", "disturbance_stack_resampled_masked_30m_TEST.tif")
dist_stack <- rast(stack_path)

# 1. Check number of layers
nlyr(dist_stack)

# 2. Look at current names (terra auto-assigns names like lyr1, lyr2…)
names(dist_stack)

# 3. Confirm bands visually
dist_stack

terra::nlyr(rast("gdal_tmp/wildfire_id_30m.tif"))   # should be 37
terra::nlyr(rast("gdal_tmp/biotic_gridded_1km_all_years_severity_30m.tif")) # should be 27
terra::nlyr(rast("gdal_tmp/pdsi_annual_30m.tif"))   # should be 21

plot(dist_stack[[1]])   # first wildfire band
plot(dist_stack[[37]])  # last wildfire band
plot(dist_stack[[38]])  # should be first biotic band
plot(dist_stack[[64]])  # last biotic band
plot(dist_stack[[65]])  # first pdsi band
plot(dist_stack[[85]])  # last pdsi band

terra::sources(dist_stack)
terra::minmax(dist_stack)

nlyr(dist_stack)        # total number of bands
names(dist_stack)[1:10] # first 10 band names
names(dist_stack)[55:64] # last 10 band names

#terra::setMinMax(dist_stack)
#terra::minmax(dist_stack)














#################################################################################



### Assign layer names and re-save final stacked raster

# Load final stacked raster
stack_path <- here("data", "derived", "disturbance_stack_resampled_masked_30m.tif")
dist_stack <- rast(stack_path)

# Paths to original resampled rasters (already aligned and same resolution)
resampled_paths <- c(
  here("gdal_tmp", "wildfire_id_30m.tif"),
  here("gdal_tmp", "biotic_gridded_1km_all_years_severity_30m.tif"),
  here("gdal_tmp", "pdsi_annual_30m.tif")
)
names(resampled_paths) <- c("wildfire", "biotic", "pdsi")

# Years of interest
years_of_interest <- 2000:2020

# Number of bands in each source raster
n_bands <- sapply(resampled_paths, function(f) nlyr(rast(f)))

# Start years for each source raster
start_years <- c(
  wildfire = 1984,
  biotic   = 1997,
  pdsi     = 2000
)

# Calculate which bands correspond to 2000-2020
band_indices <- list()
current_offset <- 0
for (name in names(resampled_paths)) {
  yrs <- start_years[name]:(start_years[name] + n_bands[name] - 1)
  bands <- which(yrs %in% years_of_interest)
  # Adjust for position in the final stack
  band_indices[[name]] <- bands + current_offset
  current_offset <- current_offset + n_bands[name]
}

# Flatten to a single vector
bands_2000_2020 <- unlist(band_indices)

# Subset stack
stack_2000_2020 <- dist_stack[[bands_2000_2020]]

# Assign names
names(stack_2000_2020) <- c(
  paste0("wildfire_", years_of_interest),
  paste0("biotic_", years_of_interest),
  paste0("pdsi_", years_of_interest)
)

# Save final raster
writeRaster(
  stack_2000_2020,
  here("data", "derived", "disturbance_stack_2000_2020_30m.tif"),
  overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE")
)

message("✅ Saved final stack with only 2000-2020 bands.")
