###



# If working in cyverse, set working directory and project root
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")   # or any file guaranteed to exist in the project



# Install and load required packages
packages <- c("here", "terra", "dplyr", "ggplot2", "tidyr")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) {
  install.packages(packages[!installed])
}

library(here)
library(terra)
library(dplyr)
library(ggplot2)
library(tidyr)


# Set cyverse memory max to avoid crashing
terraOptions(memmax=256)






################################################################################


### Explore disturbance stack




# Load disturbance stack
stack_path <- here("data", "derived", "disturbance_stack_resampled_masked_30m.tif")
dist_stack <- rast(stack_path)


terra::sources(dist_stack)

system2("gdalinfo", stack_path)


# Check if the resampled file exists
file.exists("gdal_tmp/hd_fingerprint_30m.tif")

# Inspect input raster properties
terra::rast(here("data", "derived", "hd_fingerprint.tif"))

# Optional: manually run gdalwarp on hd_fingerprint to see if it completes
system2("gdalwarp", args = c(
  "-tr", 30, 30,
  "-r", "bilinear",
  "-te", ext(template)[1], ext(template)[3], ext(template)[2], ext(template)[4],
  "-multi", "-wo", "NUM_THREADS=8",
  "-co", "COMPRESS=DEFLATE",
  here("data", "derived", "hd_fingerprint.tif"),
  "gdal_tmp/hd_fingerprint_30m.tif"
))





# Inspect
dist_stack
nlyr(dist_stack)
names(dist_stack)
ext(dist_stack)
res(dist_stack)

# -------------------------
# 2. Identify bands for each type
# -------------------------
years <- 2000:2020

fire_bands   <- grep("^wildfire", names(dist_stack), value = TRUE)
biotic_bands <- grep("^biotic", names(dist_stack), value = TRUE)
hd_bands     <- grep("^hd", names(dist_stack), value = TRUE)
pdsi_bands   <- grep("^pdsi", names(dist_stack), value = TRUE)

fire_stack   <- dist_stack[[fire_bands]]
biotic_stack <- dist_stack[[biotic_bands]]
hd_stack     <- dist_stack[[hd_bands]]
pdsi_stack   <- dist_stack[[pdsi_bands]]

# -------------------------
# 3. Create binary presence/absence masks
# -------------------------
fire_bin   <- fire_stack > 0
biotic_bin <- biotic_stack > 0
hd_bin     <- hd_stack > 0
pdsi_bin   <- pdsi_stack > 0

# -------------------------
# 4. Compound disturbance masks
# -------------------------
any_disturbance <- fire_bin | biotic_bin | hd_bin | pdsi_bin
all_four <- fire_bin & biotic_bin & hd_bin & pdsi_bin

# Example 2-way overlaps
fire_biotic <- fire_bin & biotic_bin
fire_hd     <- fire_bin & hd_bin
biotic_hd   <- biotic_bin & hd_bin

# -------------------------
# 5. Area disturbed per year
# -------------------------
calc_area_km2 <- function(bin_stack) {
  global(bin_stack, "sum", na.rm = TRUE) * prod(res(bin_stack)) / 1e6
}

fire_area   <- calc_area_km2(fire_bin)
biotic_area <- calc_area_km2(biotic_bin)
hd_area     <- calc_area_km2(hd_bin)
pdsi_area   <- calc_area_km2(pdsi_bin)

area_df <- data.frame(
  year = years,
  fire   = fire_area,
  biotic = biotic_area,
  hd     = hd_area,
  pdsi   = pdsi_area
)

area_long <- area_df %>% pivot_longer(-year, names_to = "disturbance", values_to = "area_km2")

# Plot annual area disturbed
ggplot(area_long, aes(year, area_km2, fill = disturbance)) +
  geom_col(position = "dodge") +
  labs(title = "Annual Area Disturbed by Each Type (2000–2020)", y = "Area (km²)")

# -------------------------
# 6. Pixel-level disturbance diversity
# -------------------------
diversity_map <- (app(fire_bin, "sum") > 0) +
  (app(biotic_bin, "sum") > 0) +
  (app(hd_bin, "sum") > 0) +
  (app(pdsi_bin, "sum") > 0)

plot(diversity_map, main = "Number of Disturbance Types per Pixel (2000–2020)")

# -------------------------
# 7. First year of disturbance
# -------------------------
first_year_fun <- function(x) ifelse(any(x > 0), years[which(x > 0)[1]], NA)

first_fire   <- app(fire_bin, first_year_fun)
first_biotic <- app(biotic_bin, first_year_fun)
first_hd     <- app(hd_bin, first_year_fun)
first_pdsi   <- app(pdsi_bin, first_year_fun)

# Optional: lag between fire and biotic
lag_fb <- first_biotic - first_fire

# -------------------------
# 8. Summary statistics
# -------------------------
total_pixels <- ncell(any_disturbance)
compound_pixels <- global(all_four, "sum", na.rm = TRUE)

cat("Total disturbed pixels:", total_pixels, "\n")
cat("Pixels with all four disturbances:", compound_pixels, "\n")
cat("Proportion compound:", compound_pixels / total_pixels, "\n")

# -------------------------
# 9. Save outputs (optional)
# -------------------------
writeRaster(diversity_map, here("data", "derived", "disturbance_diversity_map.tif"),
            overwrite = TRUE, gdal = c("COMPRESS=DEFLATE"))

writeRaster(first_fire, here("data", "derived", "first_fire_year.tif"),
            overwrite = TRUE, gdal = c("COMPRESS=DEFLATE"))

writeRaster(lag_fb, here("data", "derived", "fire_biotic_lag.tif"),
            overwrite = TRUE, gdal = c("COMPRESS=DEFLATE"))

write.csv(area_df, here("data", "derived", "disturbance_area_per_year.csv"), row.names = FALSE)
