### Forest-disturbance-stack-v3
### Spatial frequency of single and compound disturbances
### Singles: number of disturbed years (2000–2020)
### Doubles/triples: distinct co-occurrence episodes within 3 calendar years
### Separate figures for ANY and EXTREME
################################################################################

packages <- c(
  "terra",
  "sf",
  "dplyr",
  "patchwork",
  "scales",
  "ggspatial"
)

installed <- packages %in% installed.packages()[, "Package"]

if (any(!installed)) {
  install.packages(packages[!installed])
}

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(ggspatial)

# ==============================================================================
# 1. SETTINGS
# ==============================================================================

root <- "/home/jovyan/data-store/forest-disturbance-stack-v3"
setwd(root)

terraOptions(
  tempdir = file.path(root, "tmp_terra"),
  memfrac = 0.5,
  progress = 1
)

dir.create(
  file.path(root, "tmp_terra"),
  showWarnings = FALSE,
  recursive = TRUE
)

# Original annual-frequency rasters
frequency_dir <- file.path(
  root, "data/derived/annual_stacks/binary/frequency"
)

# New three-year episode-frequency rasters
episode_dir <- file.path(
  root, "data/derived/disturbance_sequence/frequency_3yr_episodes"
)

# Forest mask
forest_path <- file.path(
  root, "data/derived/resampled/forest_mask_30m_resampled.tif"
)

# EPA Level III ecoregion/state boundary shapefile
boundary_path <- file.path(
  root,
  "data/raw/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp"
)

# Figure output directory
figure_dir <- file.path(
  root, "figures/temporal_patterns/three_year_frequency"
)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Raster aggregation and smoothing
agg_factor <- 10
smooth_window <- 5

# Use a shared color scale for ANY and EXTREME
shared_scale <- FALSE

# Use the same map scale across all panels
# Setting FALSE would allow separate panel scales
shared_panel_scale <- FALSE

# Western states
western_states <- c(
  "Washington", "Oregon", "California",
  "Idaho", "Montana", "Wyoming",
  "Nevada", "Utah", "Colorado",
  "Arizona", "New Mexico"
)

# Panel order
combos <- c(
  "wf", "bt", "hd",
  "wf_bt", "wf_hd", "bt_hd",
  "wf_bt_hd"
)

panel_titles <- c(
  wf       = "Wildfire",
  bt       = "Biotic",
  hd       = "Drought",
  wf_bt    = "Wildfire + Biotic",
  wf_hd    = "Wildfire + Drought",
  bt_hd    = "Biotic + Drought",
  wf_bt_hd = "Wildfire + Biotic + Drought"
)


# ==============================================================================
# 2. BUILD INPUT PATHS
# ==============================================================================

get_raster_path <- function(combo, threshold) {
  
  if (combo %in% c("wf", "bt", "hd")) {
    
    # Existing single-disturbance annual-frequency rasters
    file.path(
      frequency_dir,
      threshold,
      paste0(combo, "_", threshold, "_frequency.tif")
    )
    
  } else {
    
    # New three-year episode-frequency rasters
    file.path(
      episode_dir,
      paste0(combo, "_frequency_3yr_", threshold, ".tif")
    )
    
  }
}

input_paths <- expand.grid(
  combo = combos,
  threshold = c("any", "extreme"),
  stringsAsFactors = FALSE
)

input_paths$path <- mapply(
  get_raster_path,
  input_paths$combo,
  input_paths$threshold
)

missing_files <- input_paths$path[
  !file.exists(input_paths$path)
]

if (length(missing_files) > 0) {
  stop(
    "Missing raster files:\n",
    paste(missing_files, collapse = "\n")
  )
}


# ==============================================================================
# 3. LOAD AND PREPARE WESTERN U.S. BOUNDARIES
# ==============================================================================

boundaries <- st_read(boundary_path, quiet = TRUE)

west_eco <- boundaries |>
  filter(STATE_NAME %in% western_states) |>
  st_make_valid() |>
  group_by(US_L3CODE) |>
  summarise(geometry = st_union(geometry), .groups = "drop")

west_outline <- st_sf(
  geometry = st_union(west_eco)
)

# Project boundaries to raster CRS
reference_raster <- rast(input_paths$path[1])

west_eco <- st_transform(
  west_eco,
  crs(reference_raster)
)

west_outline <- st_transform(
  west_outline,
  crs(reference_raster)
)

# Bounding box used for all panels
map_bbox <- st_bbox(west_outline)


# ==============================================================================
# 4. PREPARE FOREST MASK
# ==============================================================================

forest <- rast(forest_path)

# Forest mask at aggregated resolution
forest_agg <- aggregate(
  forest,
  fact = agg_factor,
  fun = max,
  na.rm = TRUE
)

forest_agg <- ifel(
  forest_agg == 1,
  1,
  NA
)


# ==============================================================================
# 5. PROCESS FREQUENCY RASTERS
# ==============================================================================

process_frequency <- function(path) {
  
  cat("Processing:", basename(path), "\n")
  
  r <- rast(path)
  
  # Aggregate frequency values to reduce plotting resolution
  r_agg <- aggregate(
    r,
    fact = agg_factor,
    fun = mean,
    na.rm = TRUE
  )
  
  # Smooth spatial patterns
  weights <- matrix(
    1,
    nrow = smooth_window,
    ncol = smooth_window
  )
  
  r_smooth <- focal(
    r_agg,
    w = weights,
    fun = mean,
    na.rm = TRUE
  )
  
  # Apply aggregated forest mask
  r_smooth <- mask(r_smooth, forest_agg)
  
  # Convert to plotting dataframe
  df <- as.data.frame(
    r_smooth,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(df) <- c("x", "y", "frequency")
  
  df <- df |>
    filter(
      is.finite(frequency),
      frequency > 0
    )
  
  return(df)
}


# Process each input once
plot_data <- list()

for (i in seq_len(nrow(input_paths))) {
  
  threshold <- input_paths$threshold[i]
  combo <- input_paths$combo[i]
  
  key <- paste(threshold, combo, sep = "_")
  
  plot_data[[key]] <- process_frequency(
    input_paths$path[i]
  )
}


# ==============================================================================
# 6. DETERMINE COLOR SCALE
# ==============================================================================

all_maxima <- vapply(
  plot_data,
  function(x) {
    if (nrow(x) == 0) return(0)
    max(x$frequency, na.rm = TRUE)
  },
  numeric(1)
)

global_max <- max(all_maxima)

threshold_max <- c(
  any = max(all_maxima[grepl("^any_", names(all_maxima))]),
  extreme = max(all_maxima[grepl("^extreme_", names(all_maxima))])
)

get_fill_max <- function(threshold, combo) {
  
  if (!shared_panel_scale) {
    
    return(max(
      all_maxima[paste(threshold, combo, sep = "_")],
      1
    ))
  }
  
  if (shared_scale) {
    return(max(global_max, 1))
  }
  
  return(max(threshold_max[threshold], 1))
}


# ==============================================================================
# 7. CREATE INDIVIDUAL MAP PANELS
# ==============================================================================

make_panel <- function(threshold, combo, show_legend = TRUE,
                       show_map_elements = FALSE) {
  
  key <- paste(threshold, combo, sep = "_")
  
  df <- plot_data[[key]]
  
  fill_max <- get_fill_max(threshold, combo)
  
  p <- ggplot() +
    
    geom_raster(
      data = df,
      aes(x = x, y = y, fill = frequency)
    ) +
    
    geom_sf(
      data = west_eco,
      fill = NA,
      color = "grey65",
      linewidth = 0.12
    ) +
    
    geom_sf(
      data = west_outline,
      fill = NA,
      color = "grey20",
      linewidth = 0.4
    ) +
    
    scale_fill_viridis_c(
      option = "magma",
      direction = -1,
      trans = "sqrt",
      limits = c(0, fill_max),
      oob = scales::squish,
      na.value = "transparent",
      name = NULL,
      guide = guide_colorbar(
        barwidth = grid::unit(2.5, "cm"),
        barheight = grid::unit(0.3, "cm")
      )
    ) +
    
    coord_sf(
      xlim = c(map_bbox["xmin"], map_bbox["xmax"]),
      ylim = c(map_bbox["ymin"], map_bbox["ymax"]),
      expand = FALSE,
      datum = NA
    ) +
    
    labs(
      title = panel_titles[[combo]]
    ) +
    
    theme_void() +
    
    theme(
      plot.title = element_text(
        size = 10,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 4)
      ),
      plot.margin = margin(2, 2, 2, 2),
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )

  # Add scale bar and compass rose to triple panel
  if (show_map_elements) {
    
    p <- p +
      
      # Scale bar: bottom-left
      annotation_scale(
        location = "bl",
        width_hint = 0.25,
        unit_category = "metric",
        text_cex = 0.7,
        line_width = 0.5,
        pad_x = grid::unit(0.20, "cm"),
        pad_y = grid::unit(0.15, "cm")
      ) +
      
      # Compass rose: bottom-left, above scale bar
      annotation_north_arrow(
        location = "bl",
        which_north = "true",
        style = north_arrow_fancy_orienteering(
          text_size = 7,
          line_width = 0.5
        ),
        height = grid::unit(0.8, "cm"),
        width = grid::unit(0.8, "cm"),
        pad_x = grid::unit(0.20, "cm"),
        pad_y = grid::unit(1.10, "cm")
      )
  }
  
  return(p)
}


# ==============================================================================
# 8. BUILD THREE-ROW FIGURE
# ==============================================================================

make_figure <- function(threshold) {
  
  # Row 1: single disturbances
  p_wf <- make_panel(threshold, "wf")
  p_bt <- make_panel(threshold, "bt")
  p_hd <- make_panel(threshold, "hd")
  
  # Row 2: double combinations
  p_wf_bt <- make_panel(threshold, "wf_bt")
  p_wf_hd <- make_panel(threshold, "wf_hd")
  p_bt_hd <- make_panel(threshold, "bt_hd")
  
  # Row 3: triple combination
  p_triple <- make_panel(
    threshold,
    "wf_bt_hd",
    show_legend = TRUE,
    show_map_elements = TRUE
  )
  
  # Blank panels to center the triple map
  blank <- plot_spacer()
  
  figure <- (
    (p_wf | p_bt | p_hd) /
      (p_wf_bt | p_wf_hd | p_bt_hd) /
      (p_triple | blank | blank)
  ) +
    plot_layout(
      heights = c(1, 1, 1),
      guides = "keep"
    )
  
  return(figure)
}


# ==============================================================================
# 9. GENERATE AND SAVE FIGURES
# ==============================================================================

fig_any <- make_figure("any")

fig_extreme <- make_figure("extreme")

ggsave(
  filename = file.path(
    figure_dir,
    "spatial_frequency_3yr_any.png"
  ),
  plot = fig_any,
  width = 7,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    figure_dir,
    "spatial_frequency_3yr_extreme.png"
  ),
  plot = fig_extreme,
  width = 7,
  height = 8,
  dpi = 300,
  bg = "white"
)

print(fig_any)
print(fig_extreme)

cat("\nFigures saved to:\n", figure_dir, "\n")