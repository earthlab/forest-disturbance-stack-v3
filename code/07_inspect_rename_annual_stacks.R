### Forest-disturbance-stack-v3

### 

### Matt Bitters
### matthew.bitters@colorado.edu



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



test2000 <- rast("data/derived/annual_stacks/annual_stack_2000.tif")
summary(test2000)

plot(test2000, maxcell = 100000)


raw <- rast("data/derived/not_resampled/pdsi_annual.tif")[[1]] # raw file - 2000
res <- rast("data/derived/resampled/pdsi_annual_30m_resampled.tif")[[1]] # resampled file - 2000
masked <- rast("data/derived/resampled/pdsi_annual_30m_resampled_masked_2000.tif") # separated into individual years at this point - 2000
test2000 <- rast("data/derived/annual_stacks/annual_stack_2000.tif") # pdsi is 4th band - 2000

nlyr(raw)
nlyr(res)
nlyr(masked)
nlyr(test2000)

summary(raw)
summary(res)
summary(masked)
summary(test2000)


summary(rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif"))
unique(rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif"), na.rm=TRUE)


compareGeom(res, rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif"))
freq(rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif"))


plot(rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif"))

res
ext(res)

forest <- rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif")
ext(forest)

res(ext(res), ext(forest))



plot(rast("data/derived/resampled/biotic_gridded_1km_all_years_severity_30m_resampled.tif"))

unique(rast("data/derived/resampled/wildfire_id_2000_2020_subset.tif"))















# Path to annual stacks directory
stack_dir <- here("data", "derived", "annual_stacks")

# List all annual stacks (annual_stack_YYYY.tif)
stack_files <- list.files(
  stack_dir,
  pattern = "^annual_stack_\\d{4}\\.tif$",
  full.names = TRUE
)

stack_files


### Inspect one stack (structure, crs, band names)

# Read one stack (e.g., 2000)
s2000 <- rast(stack_files[1])
s2000

# View CRS
crs(s2000)

# Names of individual layers
names(s2000)

### Check all stacks for consistency

# Number of layers per year
sapply(stack_files, function(f) nlyr(rast(f)))

# CRS consistency
crs_list <- sapply(stack_files, function(f) crs(rast(f)))
unique(crs_list)

### Rename to meaningful band names

# Function to rename bands
rename_bands <- function(fpath) {
  r <- rast(fpath)
  year <- str_extract(basename(fpath), "\\d{4}")
  
  band_names <- paste0(c("wildfire", "biotic", "hd", "pdsi"), "_", year)
  names(r) <- band_names
  
  return(r)
}

# Apply to all annual stacks
renamed <- lapply(stack_files, rename_bands)




f <- "data/derived/annual_stacks/annual_stack_2000.tif"
r <- rast(f)

# Plot all layers in separate panels
plot(r, maxcell = 100000)



pdsi_2000 <- rast("data/derived/resampled/pdsi_annual_30m_resampled_masked_2000.tif")

summary(pdsi_2000)
minmax(pdsi_2000)
unique(pdsi_2000, size = 1e6)

pdsi_src <- rast("data/derived/resampled/pdsi_annual_30m_resampled.tif")

nlyr(pdsi_src)
summary(pdsi_src[[1]])
summary(pdsi_src[[2]])
summary(pdsi_src[[3]])


pdsi_raw <- rast("data/derived/not_resampled/pdsi_annual.tif")  
summary(pdsi_raw)

ext(pdsi_src)
ext(rast("data/derived/resampled/forest_mask_30m_resampled.tif"))

x <- rast("data/derived/annual_stacks/annual_stack_2000.tif")
# assuming 4 bands: fire, beetle, drought, pdsi
pdsi_band <- x[[4]]

pdsi_band
plot(pdsi_band)
hist(pdsi_band)
summary(pdsi_band)

pdsi_res <- rast("data/derived/resampled/pdsi_annual_30m_resampled.tif")
plot(pdsi_res[[1]])  # before masking
summary(pdsi_res)

pdsi_raw <- rast("data/derived/not_resampled/pdsi_annual.tif")
pdsi_raw


mask <- rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif")
unique(mask)





pdsi_test_2000 <- rast("data/derived/resampled/test_pdsi_2000.tif")
summary(pdsi_test_2000)

pdsi_test <- rast("tmp_terra/pdsi_annual_30m_resampled.tif")
summary(pdsi_test)

