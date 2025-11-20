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
masked_wildfire <- rast("data/derived/resampled/wildfire_id_30m_resampled_masked_2000.tif") # separated into individual years at this point - 2000
masked_biotic <- rast("data/derived/resampled/biotic_gridded_1km_all_years_severity_30m_resampled_masked_2000.tif") # separated into individual years at this point - 2000
masked_hd <- rast("data/derived/resampled/hd_fingerprint_30m_resampled_masked_2000.tif") # separated into individual years at this point - 2000
masked_pdsi <- rast("data/derived/resampled/pdsi_annual_30m_resampled_masked_2000.tif") # separated into individual years at this point - 2000
test2000 <- rast("data/derived/annual_stacks/annual_stack_2000.tif") # pdsi is 4th band - 2000

nlyr(raw)
nlyr(res)
nlyr(masked_pdsi)
nlyr(test2000)

summary(raw)
summary(res)
summary(masked_wildfire)
summary(masked_biotic)
summary(masked_hd)
summary(masked_pdsi)
summary(test2000)

summary(masked_biotic[[1]])
mask <- rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif")
freq(mask)
sum(values(mask) == 1, na.rm=TRUE)

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



biotic_resampled <- rast("data/derived/resampled/biotic_gridded_1km_all_years_severity_30m_resampled.tif")
summary(biotic_resampled[[1]])









b <- rast("data/derived/resampled/biotic_gridded_1km_all_years_severity_30m_resampled.tif")[[1]]
w <- rast("data/derived/resampled/wildfire_id_30m_resampled.tif")[[1]]
m <- rast("data/derived/resampled/forest_mask_30m_resampled.tif")
ma <- rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif")

res(b)
res(w)
res(m)
res(ma)

ext(b)
ext(w)
ext(m)
ext(ma)

terra::compareGeom(b, ma, stopOnError = FALSE)

freq(ma)  # look for non-integer 1s






raw_biotic <- rast("data/derived/not_resampled/biotic_gridded_1km_all_years_severity.tif")
summary(raw_biotic)
plot(raw_biotic)

biotic_resampled <- rast("data/derived/resampled/biotic_gridded_1km_all_years_severity_30m_resampled.tif")
summary(biotic_resampled)
plot(biotic_resampled)

compareGeom(
  rast("data/derived/resampled/wildfire_id_30m_resampled.tif"),
  rast("data/derived/resampled/biotic_gridded_1km_all_years_severity_30m_resampled.tif"),
  stopOnError = FALSE
)

mask_aligned <- rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif")

compareGeom(biotic_resampled, mask_aligned, stopOnError = FALSE)
ext(biotic_resampled)
ext(mask_aligned)


# 1) exact numeric extents and their differences
e_b <- ext(rast("data/derived/resampled/biotic_gridded_1km_all_years_severity_30m_resampled.tif"))
e_m <- ext(rast("data/derived/resampled/forest_mask_30m_resampled_aligned.tif"))

e_b_vals <- c(xmin(e_b), xmax(e_b), ymin(e_b), ymax(e_b))
e_m_vals <- c(xmin(e_m), xmax(e_m), ymin(e_m), ymax(e_m))

print(e_b_vals, digits = 15)
print(e_m_vals, digits = 15)

# Differences
diffs <- e_b_vals - e_m_vals
names(diffs) <- c("xmin","xmax","ymin","ymax")
print(diffs, digits = 15)
