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




