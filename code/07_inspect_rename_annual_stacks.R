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

plot(test2000, maxcell = 50000)


raw <- rast("data/derived/not_resampled/pdsi_annual.tif")[[1]] # raw file - 2000
res <- rast("data/derived/resampled/pdsi_annual_30m_resampled.tif")[[1]] # resampled file - 2000
masked_pdsi <- rast("data/derived/resampled/pdsi_annual_30m_resampled_masked_2000.tif") # separated into individual years at this point - 2000
masked_wildfire <- rast("data/derived/resampled/wildfire_id_30m_resampled_masked_2000.tif") # separated into individual years at this point - 2000
test2000 <- rast("data/derived/annual_stacks/annual_stack_2000.tif") # pdsi is 4th band - 2000

nlyr(raw)
nlyr(res)
nlyr(masked_pdsi)
nlyr(test2000)

summary(raw)
summary(res)
summary(masked_pdsi)
summary(masked_wildfire)
summary(test2000)


summary(rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif"))
unique(rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif"), na.rm=TRUE)


compareGeom(res, rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif"))
freq(rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif"))


plot(rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif"))

res
ext(res)

forest_aligned <- rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif")
ext(forest_aligned)
crs(forest_aligned)
res(forest_aligned)

forest <- rast("data/derived/resampled/forest_mask_30m_resampled.tif")
ext(forest)
crs(forest)
res(forest)

ext(res)
crs(res)
res(res)



plot(rast("data/derived/resampled/biotic_gridded_1km_all_years_severity_30m_resampled.tif"))

unique(rast("data/derived/resampled/wildfire_id_2000_2020_subset.tif"))














w <- rast("data/derived/resampled/wildfire_id_30m_resampled.tif")[[1]]
m <- rast("data/derived/resampled/forest_mask_30m_resampled.tif")
ma <- rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif")

res(w)
res(m)
res(ma)

ext(w)
ext(m)
ext(ma)

terra::compareGeom(w, ma, stopOnError = FALSE)















