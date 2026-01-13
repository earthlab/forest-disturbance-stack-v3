### Forest-disturbance-stack-v3

### 

### Matt Bitters
### matthew.bitters@colorado.edu

################################################################################

# Working directory
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

# Packages
packages <- c("here", "terra", "stringr")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) install.packages(packages[!installed])

library(here)
library(terra)
library(stringr)

################################################################################

# Raw file - 2000 is band 1
raw_2000 <- rast("data/derived/not_resampled/wildfire_cbi.tif")[[1]]
# Subset from raw file - 2000 is band 1
subset_2000 <- rast("data/derived/resampled/wildfire_cbi_2000_2020_subset.tif")[[1]]
# Resampled from subset - 2000 is band 1
resampled_2000 <- rast("data/derived/resampled/wildfire_cbi_30m_resampled.tif")[[1]]
# Masked from resampled - only one band (2000)
masked_2000 <- rast("data/derived/resampled/wildfire_cbi_30m_resampled_masked_2000.tif")
# Annual stack from all disturbance layers - wildfire is band 1
annual_wf_2000 <- rast("data/derived/annual_stacks/annual_stack_2000.tif")[[1]]
# Full annual stack
annual_2000 <- rast("data/derived/annual_stacks/annual_stack_2000.tif")

# Summaries
summary(raw_2000)
summary(subset_2000)
summary(resampled_2000)
summary(masked_2000)
summary(annual_wf_2000)

# Wildfire bands plots
plot(raw_2000, maxcell = 2e5)
plot(subset_2000, maxcell = 2e5)
plot(resampled_2000, maxcell = 2e5)
plot(masked_2000, maxcell = 2e5)
plot(annual_wf_2000, maxcell = 2e5)

# Full annual stack
plot(annual_2000, maxcell = 2e5)

# Full annual binarized stack
annual_bin_2000 <- rast("data/derived/annual_stacks/binary/annual_stack_bin_2000.tif")
plot(annual_bin_2000, maxcell = 2e5)











################################################################################

stack_dir <- "data/derived/annual_stacks"

for (yr in 2000:2020) {
  f <- file.path(stack_dir, paste0("annual_stack_", yr, ".tif"))
  if (!file.exists(f)) {
    message("Skipping missing: ", f)
    next
  }
  
  r <- rast(f)
  stopifnot(nlyr(r) == 4)
  
  names(r) <- c(
    paste0("wildfire_", yr),
    paste0("biotic_", yr),
    paste0("hd_", yr),
    paste0("pdsi_", yr)
  )
  
  tmp <- tempfile(fileext = ".tif")
  
  writeRaster(
    r, tmp,
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES")
  )
  
  file.rename(tmp, f)
  
  message("✅ Renamed bands for ", yr)
}



