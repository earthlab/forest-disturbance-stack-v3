
# This script downloads, unzips, and merges the 2025 version of the
# Hicke ADS 1km gridded biotic disturbance agent datasets into west-wide annualized
# rasters of a) total severities, and b) binary presence based on a given severity threshold.
# The script should run in 5 minutes or less on a local machine.

# Tyler L. McIntosh, CU Boulder

# Thresholds - NOT USING AT THE MOMENT
#biotic_sev_thresholds <- c(0, 25, 50)

# Setup
if(!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

source(here::here("utils", "functions.R"))

install_and_load_packages(c(
  "tidyverse",
  "terra",
  "mapview",
  "tictoc"))

dir_figs <- here::here('figs')
dir_derived <- here::here('data', 'derived')
dir_raw <- here::here('data', 'raw')
dir_manual <- here::here('data', 'manual')
dir_ensure(c(dir_figs,
             dir_derived,
             dir_manual,
             dir_raw))



# Download and unzip insect data, get just tifs
url <- 'https://webpages.uidaho.edu/~jhicke/outgoing/gridded_mortality_area_v4.1_250307/results/west_us/final_product/us_19972023_MA_FHPR1-R4_capping_to_2023.zip'
zipped_ads_grids <- safe_download(
  url = url,
  dest_dir = dir_raw,
  timeout = 1000
)
#Unzip all
all_unzipped_paths <- safe_unzip(zipped_ads_grids,
                                 recursive = TRUE)

#unique(tools::file_ext(all_unzipped_paths))
tif_files <- all_unzipped_paths[tools::file_ext(all_unzipped_paths) == 'tif'] #Get only tifs

# Create by-year lists
#Get years
biotic_years <- stringr::str_extract(tif_files, "(?<=us_)\\d{4}") |>
  as.integer() |>
  unique() |>
  purrr::discard(is.na)

#Split into a list by year
biotic_by_year <- purrr::map(
  biotic_years,
  ~ tif_files[str_detect(tif_files, str_c("us_", .x))]
)
names(biotic_by_year) <- paste0('biotic_', as.character(biotic_years))

# Use function to merge and create single spatraster
biotic_all <- biotic_by_year |>
  purrr::map(.f = merge_biotic_year) |>
  terra::rast()
names(biotic_all) <- names(biotic_by_year)

# Write raster
terra::writeRaster(
  biotic_all,
  filename = here::here(dir_derived, "biotic_gridded_1km_all_years_severity.tif"),
  overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE"),
  datatype = "FLT4S"
)


# # Create thresholded binaries
# for(t in biotic_sev_thresholds) {
#   biotic_all_binary <- (biotic_all > t) * 1
#   names(biotic_all_binary) <- names(biotic_all)
  
#   # Write raster
#   terra::writeRaster(
#     biotic_all,
#     filename = here::here(dir_derived, paste0("biotic_gridded_1km_all_years_binary_threshold_", t, ".tif")),
#     overwrite = TRUE,
#     gdal = c("COMPRESS=DEFLATE"),
#     datatype = "INT2S"
#   )
# }
