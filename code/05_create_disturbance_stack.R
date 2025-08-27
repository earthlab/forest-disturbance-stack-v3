### forest-disturbance-stack-v3

### This script creates a function to create a custom disturbance stack. It accepts
### a template raster path (to determine resolution and extent), a list of
### individual raster file paths (e.g., biotic, wildfire, drought stacks), a 
### forest mask path (binary mask), and a vector of thresholds (e.g., biotic thresholds).
### Returns and saves a final stacked spatRaster.


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






### The function
stack_resample_mask <- function(template_path,
                                raster_paths,
                                forest_mask_path,
                                thresholds = NULL,
                                apply_mask = TRUE,
                                output_path,
                                sample_size = 1000) {
  
  template <- rast(template_path)
  
  choose_method <- function(r) {
    dt <- terra::datatype(r)[1]
    if (grepl("INT", dt)) return("near")
    vals <- terra::spatSample(r, size = sample_size, method = "random", na.rm = TRUE)
    if (all(floor(vals) == vals)) return("near")
    return("bilinear")
  }
  
  choose_datatype <- function(r, method) {
    dt <- terra::datatype(r)[1]
    if (method == "near" && grepl("FLT", dt)) return("INT4S")
    if (grepl("INT", dt)) return("INT4S")
    return("FLT4S")
  }
  
  # Resample rasters
  resampled_files <- lapply(raster_paths, function(path) {
    r <- rast(path)
    method <- choose_method(r)
    message("Resampling ", basename(path), " using method: ", method)
    
    if (method == "near" && grepl("FLT", terra::datatype(r)[1])) {
      r <- as.int(round(r))
    }
    
    temp_file <- tempfile(fileext = ".tif")
    r_res <- resample(r, template, method = method, filename = temp_file, overwrite = TRUE)
    terra::datatype(r_res) <- choose_datatype(r, method)
    return(temp_file)
  })
  
  raster_stack <- rast(resampled_files)
  
  # Apply forest mask if requested
  if (apply_mask) {
    forest_mask <- rast(forest_mask_path)
    forest_mask[forest_mask == 0] <- NA
    forest_method <- choose_method(forest_mask)
    if (forest_method == "near" && grepl("FLT", terra::datatype(forest_mask)[1])) {
      forest_mask <- as.int(round(forest_mask))
    }
    temp_forest <- tempfile(fileext = ".tif")
    forest_resampled <- resample(forest_mask, template, method = forest_method,
                                 filename = temp_forest, overwrite = TRUE)
    terra::datatype(forest_resampled) <- choose_datatype(forest_mask, forest_method)
    raster_stack <- mask(raster_stack, rast(temp_forest))
  }
  
  # Add thresholds per layer (per band) if provided
  if (!is.null(thresholds)) {
    if (length(thresholds) != nlyr(raster_stack)) {
      stop("Number of thresholds must match number of layers in the stack")
    }
    for (i in seq_len(nlyr(raster_stack))) {
      terra::setBandDescription(raster_stack, i, paste0("threshold=", thresholds[i]))
    }
  }
  
  # Write output
  writeRaster(raster_stack,
              output_path,
              overwrite = TRUE,
              gdal = c("COMPRESS=DEFLATE"))
  
  message("Resampled stack saved to: ", output_path)
  
  terra::tmpFiles(current = TRUE, remove = TRUE)
  
  return(rast(output_path))
}













##############################################################################



### Create template raster


# Path to a reference raster (e.g., wildfire disturbance file)
ref_raster_path <- here("data", "derived", "wildfire_id.tif")

# Load reference raster
ref <- rast(ref_raster_path)

# Define years for the template
years <- 2000:2020       # adjust to dataset

# Create a new raster at 30 m resolution with same extent & CRS as reference
template <- rast(
  extent = ext(ref),     # match extent
  resolution = 30,       # 30 m
  crs = crs(ref)         # match CRS
)

# Stack for each year
template_stack <- rast(replicate(length(years), template))
names(template_stack) <- paste0("year_", years)

# Fill with NA values so it can be written
values(template_stack) <- NA

# Save to file
output_path_temp <- here("data", "manual", "template_30m_2000_2020.tif")
writeRaster(
  template_stack,
  output_path_temp,
  datatype = "FLT4S",
  overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE")
)

message("Template saved to: ", output_path_temp)









##############################################################################



### Usage 


# File paths
template_path <- here("data", "derived", "template_30m_2000_2020.tif")            # defines target resolution

forest_mask_path <- here("data", "derived", "relaxed_forest_mask_2000_2020.tif") # forest mask

raster_paths <- list(
  here("data", "derived", "wildfire_id.tif"),                                    # wildfire
  here("data", "derived", "biotic_gridded_1km_all_years_severity.tif"),          # biotic
  here("data", "derived", "hd_fingerprint.tif"),                                 # hotter drought
  here("data", "derived", "pdsi_annual.tif")                                     # water balance
)

output_path <- here("data", "derived", "disturbance_stack_resampled_masked_30m_2000_2020.tif")

# Example thresholds
biotic_sev_thresholds <- c(0, 25, 50)

# Stack, resample, mask
disturbance_stack <- stack_resample_mask(
  template_path = template_path,
  raster_paths = raster_paths,
  forest_mask_path = forest_mask_path,
  thresholds = biotic_sev_thresholds,
  apply_mask = TRUE,
  output_path = output_path
)







###############################################################################










### Estimate raster sizes

r <- rast(here("data", "derived", "pdsi_annual.tif"))

nrow(r)
ncol(r)
nlyr(r)
terra::datatype(r)

estimate_raster_size <- function(r) {
  dt <- unique(terra::datatype(r))  # make sure it's length 1
  bytes_per_value <- switch(dt,
                            "INT1U" = 1, "INT2S" = 2, "INT2U" = 2,
                            "INT4S" = 4, "INT4U" = 4,
                            "FLT4S" = 4, "FLT8S" = 8,
                            4)  # default 4 bytes
  
  size_bytes <- nrow(r) * ncol(r) * nlyr(r) * bytes_per_value
  size_gb <- size_bytes / (1024^3)
  return(size_gb)
}

estimate_raster_size(r)



