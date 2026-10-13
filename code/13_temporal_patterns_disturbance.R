### Forest-disturbance-stack-v3

### Temporal patterns of disturbance

### Matt Bitters
### matthew.bitters@colorado.edu

################################################################################

# Working directory
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

# Packages
packages <- c("here", "terra", "sf", "dplyr", "ggplot2", "patchwork", "tidyr", "ragg", "ggspatial", "viridis")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) install.packages(packages[!installed])

library(here)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(ragg)
library(ggspatial)
library(viridis)


terraOptions(
  tempdir = "tmp_terra",
  memfrac = 0.6,    # use 60% of RAM
  memmax = 180000,   # cap memory at 180 GB
  progress = 1
)

################################################################################

###### Paths

# Forest mask path
forest_mask <- "data/derived/resampled/forest_mask_30m_resampled.tif"

# Ecoregion polygons
eco_shp <- "data/raw/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp"


# Frequency rasters
raster_paths_fr <- list(
  ### Any disturbance
  
  # Singles
  wf_any_fr = "data/derived/annual_stacks/binary/frequency/any/wf_any_frequency.tif",
  bt_any_fr = "data/derived/annual_stacks/binary/frequency/any/bt_any_frequency.tif",
  hd_any_fr = "data/derived/annual_stacks/binary/frequency/any/hd_any_frequency.tif",
  pd_any_fr = "data/derived/annual_stacks/binary/frequency/any/pd_any_frequency.tif",
  
  # Doubles
  wf_bt_any_fr = "data/derived/annual_stacks/binary/frequency/any/wf_bt_any_frequency.tif",
  wf_hd_any_fr = "data/derived/annual_stacks/binary/frequency/any/wf_hd_any_frequency.tif",
  wf_pd_any_fr = "data/derived/annual_stacks/binary/frequency/any/wf_pd_any_frequency.tif",
  bt_hd_any_fr = "data/derived/annual_stacks/binary/frequency/any/bt_hd_any_frequency.tif",
  bt_pd_any_fr = "data/derived/annual_stacks/binary/frequency/any/bt_pd_any_frequency.tif",
  
  # Triples
  wf_bt_hd_any_fr = "data/derived/annual_stacks/binary/frequency/any/wf_bt_hd_any_frequency.tif",
  wf_bt_pd_any_fr = "data/derived/annual_stacks/binary/frequency/any/wf_bt_pd_any_frequency.tif",
  
  ### Extreme disturbance
  
  # Singles
  wf_extr_fr = "data/derived/annual_stacks/binary/frequency/extreme/wf_extreme_frequency.tif",
  bt_extr_fr = "data/derived/annual_stacks/binary/frequency/extreme/bt_extreme_frequency.tif",
  hd_extr_fr = "data/derived/annual_stacks/binary/frequency/extreme/hd_extreme_frequency.tif",
  pd_extr_fr = "data/derived/annual_stacks/binary/frequency/extreme/pd_extreme_frequency.tif",
  
  # Doubles
  wf_bt_extr_fr = "data/derived/annual_stacks/binary/frequency/extreme/wf_bt_extreme_frequency.tif",
  wf_hd_extr_fr = "data/derived/annual_stacks/binary/frequency/extreme/wf_hd_extreme_frequency.tif",
  wf_pd_extr_fr = "data/derived/annual_stacks/binary/frequency/extreme/wf_pd_extreme_frequency.tif",
  bt_hd_extr_fr = "data/derived/annual_stacks/binary/frequency/extreme/bt_hd_extreme_frequency.tif",
  bt_pd_extr_fr = "data/derived/annual_stacks/binary/frequency/extreme/bt_pd_extreme_frequency.tif",
  
  # Triples
  wf_bt_hd_extr_fr = "data/derived/annual_stacks/binary/frequency/extreme/wf_bt_hd_extreme_frequency.tif",
  wf_bt_pd_extr_fr = "data/derived/annual_stacks/binary/frequency/extreme/wf_bt_pd_extreme_frequency.tif"
)

################################################################################

# Load forest mask as raster
forest <- rast(forest_mask)

# Aggregation factor for faster plotting
agg_factor <- 10

# Focal smoothing window, measured in aggregated pixels
# With 30 m input pixels and agg_factor = 10:
# each aggregated pixel is approximately 300 m.
# A 5 x 5 window therefore smooths across approximately 1.5 km.
smooth_window <- 5

# Equal-weight moving-window mean
smooth_weights <- matrix(
  1,
  nrow = smooth_window,
  ncol = smooth_window
)

# Aggregate forest mask
forest_ds <- aggregate(forest, fact = agg_factor, fun = max)  # small enough for plotting

# Convert forest_ds to dataframe and rename columns
forest_df <- as.data.frame(forest_ds, xy = TRUE)
colnames(forest_df) <- c("x", "y", "presence")
forest_df <- forest_df %>% filter(presence == 1)

# Load ecoregions
eco <- st_read(eco_shp)

# Flag western states
west_states <- c("California", "Oregon", "Washington", "Idaho", "Nevada",
                 "Utah", "Arizona", "Montana", "Wyoming", "Colorado", "New Mexico")
eco <- eco %>% mutate(west_flag = ifelse(STATE_NAME %in% west_states, 1, 0))
west_eco <- eco %>% filter(west_flag == 1)

# Dissolve ecoregions (remove state boundaries)
west_eco <- west_eco %>%
  group_by(US_L3CODE) %>%     # or group_by(L3_KEY)
  summarise(geometry = st_union(geometry), .groups = "drop")

# Bounding box for plotting
west_extent <- st_bbox(west_eco)

# Outline of western us for plotting
west_outline <- west_eco |>
  st_union() |>
  st_as_sf()

# ===========================================================
# Function to prepare raster for plotting
# ===========================================================
# ===========================================================
# Function to aggregate and smooth frequency rasters
# ===========================================================
raster_to_df <- function(r_path) {
  
  # Load original 30 m frequency raster
  r <- rast(r_path)
  
  # ---------------------------------------------------------
  # 1. Aggregate for faster plotting
  #
  # Use mean rather than sum so that the resulting value
  # represents average disturbance frequency within each
  # aggregated pixel.
  # ---------------------------------------------------------
  r_agg <- aggregate(
    r,
    fact = agg_factor,
    fun = mean,
    na.rm = TRUE
  )
  
  # ---------------------------------------------------------
  # 2. Smooth the aggregated frequency raster
  #
  # This calculates the mean frequency in the moving window.
  # Zeros remain valid observations during smoothing.
  # ---------------------------------------------------------
  r_smooth <- focal(
    r_agg,
    w = smooth_weights,
    fun = mean,
    na.rm = TRUE,
    na.policy = "omit"
  )
  
  # ---------------------------------------------------------
  # 3. Restrict smoothed values to the aggregated forest mask
  #
  # This prevents smoothing from being displayed outside the
  # mapped forest area.
  # ---------------------------------------------------------
  r_smooth <- mask(
    r_smooth,
    forest_ds,
    maskvalues = 0
  )
  
  # Give the raster layer a consistent name
  names(r_smooth) <- "frequency"
  
  # ---------------------------------------------------------
  # 4. Convert to dataframe for ggplot
  # ---------------------------------------------------------
  df <- as.data.frame(
    r_smooth,
    xy = TRUE,
    na.rm = TRUE
  )
  
  # Only plot locations with smoothed frequency above zero
  df <- df %>%
    filter(frequency > 0)
  
  return(df)
}

# Prepare all raster dfs
raster_dfs_fr <- lapply(raster_paths_fr, raster_to_df)

# ===========================================================
# Base map layers
# ===========================================================
base_map <- list(
  geom_raster(data = forest_df, aes(x = x, y = y),
              fill = "grey95", alpha = 1),
  coord_sf(
    xlim = c(west_extent["xmin"], west_extent["xmax"]),
    ylim = c(west_extent["ymin"], west_extent["ymax"])
  ),
  theme_void(),
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(size = 10),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.4, "cm")
  )
)




# ===========================================================
# Four-panel plot
# ===========================================================

# wf any
p_wf <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs_fr$wf_any_fr, aes(x = x, y = y, fill = frequency)) +
  scale_fill_viridis_c(name = "WF freq", option = "magma", direction = -1, na.value = "transparent") +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  # Add scale bar and compass rose to just wf panel
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    text_cex = 0.4,
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.2, "cm")
  ) +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 6),
    height = unit(0.5, "cm"),
    width  = unit(0.5, "cm"),
    pad_x  = unit(0.2, "cm"),
    pad_y  = unit(0.5, "cm")   # ← this is the key
  )+
  labs(title = "Wildfire") +
  theme(legend.position = "bottom")

# bt any
p_bt <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs_fr$bt_any_fr, aes(x = x, y = y, fill = frequency)) +
  scale_fill_viridis_c(name = "BT freq", option = "magma", direction = -1, na.value = "transparent") +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  labs(title = "Bark beetle tree\nmortality") +
  theme(legend.position = "bottom")

# hd any/extr
p_hd <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs_fr$hd_any_fr, aes(x = x, y = y, fill = frequency)) +
  scale_fill_viridis_c(name = "HD freq", option = "magma", direction = -1, na.value = "transparent") +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  labs(title = "Hotter drought index") +
  theme(legend.position = "bottom")

# wf, bt, hd any/extr
p_wf_bt_hd <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs_fr$wf_bt_hd_any_fr, aes(x = x, y = y, fill = frequency)) +
  scale_fill_viridis_c(name = "WF + BT + HD freq", option = "magma", direction = -1, na.value = "transparent") +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  labs(title = "Disturbance interactions") +
  theme(legend.position = "bottom")


# Combine panels
four_panel <- p_wf | p_bt | p_hd | p_wf_bt_hd 

# Display
four_panel

# Save
ggsave(
  filename = "figures/temporal_patterns/temporal_patterns_four_panel.png",
  plot     = four_panel,
  width    = 7,        # inches 
  height   = 4,         # inches 
  dpi      = 300,
  units    = "in",
  bg       = "white"
)



################################################################################



##### Temporal progression of disturbance


################################################################################
#### TEMPORAL PROGRESSION OF SPATIAL CO-EXPOSURE
################################################################################

# This figure shows the spatial footprint of compound disturbances within
# four temporal windows:
#
#   2000-2004
#   2005-2009
#   2010-2014
#   2015-2020
#
# Within each period, each disturbance is first collapsed across years using
# presence/absence. Compound disturbance is then calculated from those
# period-level presence rasters.
#
# For example, WF + BT during 2000-2004 means:
#
#   wildfire occurred at least once during 2000-2004
#   AND
#   biotic disturbance occurred at least once during 2000-2004
#
# The two disturbances do not need to occur in the same year.
#
# The final period contains six years; the first three contain five.
#
# For visualization, these binary 30 m rasters are processed using the same
# approach as the spatial-footprint figure:
#
#   1. aggregate 30 m -> 300 m using the mean
#   2. smooth using a 5 x 5 focal window
#   3. retain smoothed values >= 0.05
#   4. convert the retained footprint back to binary presence
#
# Aggregation, smoothing, and thresholding are for visualization only.


# ===========================================================
# Paths
# ===========================================================

temporal_spatial_dir <- "data/derived/temporal_spatial_coexposure"


# ===========================================================
# Visualization settings
# ===========================================================

# Original rasters are 30 m.
# Factor 10 produces 300 m plotting cells.
temporal_plot_agg_factor <- 10

# At 300 m resolution, a 5 x 5 focal window spans
# approximately 1.5 km.
temporal_focal_window <- 5

# Same display threshold used for the spatial-footprint figure.
temporal_smooth_threshold <- 0.05


# ===========================================================
# Define compound types and periods
# ===========================================================

temporal_compounds <- c(
  "wf_bt",
  "wf_hd",
  "bt_hd",
  "wf_bt_hd"
)

temporal_periods <- c(
  "2000_2004",
  "2005_2009",
  "2010_2014",
  "2015_2020"
)


# ===========================================================
# Labels
# ===========================================================

temporal_compound_labels <- c(
  "wf_bt"    = "Wildfire + Biotic",
  "wf_hd"    = "Wildfire + Drought",
  "bt_hd"    = "Biotic + Drought",
  "wf_bt_hd" = "Wildfire + Biotic + Drought"
)

temporal_period_labels <- c(
  "2000_2004" = "2000–2004",
  "2005_2009" = "2005–2009",
  "2010_2014" = "2010–2014",
  "2015_2020" = "2015–2020"
)


# ===========================================================
# Colors
# ===========================================================

# Colors remain constant across periods.
# Triple disturbance uses purple to match the spatial-footprint figure.

temporal_compound_colors <- c(
  "wf_bt"    = "#B2182B",  # dark red
  "wf_hd"    = "#E66101",  # orange
  "bt_hd"    = "#018571",  # teal
  "wf_bt_hd" = "#7B3294"   # purple
)


# ===========================================================
# Create table of raster paths
# ===========================================================

temporal_spatial_paths <- expand.grid(
  compound = temporal_compounds,
  period = temporal_periods,
  stringsAsFactors = FALSE
) %>%
  mutate(
    path = file.path(
      temporal_spatial_dir,
      paste0(
        compound,
        "_",
        period,
        "_presence.tif"
      )
    )
  )


# Check that all expected rasters exist
if (!all(file.exists(temporal_spatial_paths$path))) {
  
  missing_files <- temporal_spatial_paths$path[
    !file.exists(temporal_spatial_paths$path)
  ]
  
  stop(
    paste(
      "Missing temporal spatial co-exposure rasters:\n",
      paste(
        missing_files,
        collapse = "\n"
      )
    )
  )
}


################################################################################
#### FUNCTION TO PREPARE TEMPORAL SPATIAL RASTERS FOR PLOTTING
################################################################################

temporal_spatial_to_df <- function(
    r_path,
    agg_factor = temporal_plot_agg_factor,
    window_size = temporal_focal_window,
    threshold = temporal_smooth_threshold
) {
  
  # ---------------------------------------------------------
  # Load original 30 m binary raster
  #
  # 0 = compound absent during the temporal period
  # 1 = compound present during the temporal period
  # ---------------------------------------------------------
  
  r <- rast(r_path)
  
  
  # ---------------------------------------------------------
  # 1. Aggregate for visualization
  #
  # Mean represents the fraction of underlying 30 m cells
  # containing the compound disturbance.
  #
  # This continuous value is only an intermediate quantity
  # used for smoothing.
  # ---------------------------------------------------------
  
  r_agg <- aggregate(
    r,
    fact = agg_factor,
    fun = mean,
    na.rm = TRUE
  )
  
  
  # ---------------------------------------------------------
  # 2. Smooth broad spatial pattern
  #
  # The normalized 5 x 5 focal window calculates the local
  # mean of the aggregated surface.
  # ---------------------------------------------------------
  
  w <- matrix(
    1 / (window_size^2),
    nrow = window_size,
    ncol = window_size
  )
  
  
  r_smooth <- focal(
    r_agg,
    w = w,
    fun = sum,
    na.rm = TRUE,
    fillvalue = NA
  )
  
  
  # ---------------------------------------------------------
  # 3. Convert smoothed result back to displayed presence
  #
  # Values >= 0.05 are retained as presence.
  # Values below the threshold are not plotted.
  #
  # The final visualization is therefore binary rather than
  # a continuous proportion surface.
  # ---------------------------------------------------------
  
  r_presence <- ifel(
    r_smooth >= threshold,
    1,
    NA
  )
  
  
  # ---------------------------------------------------------
  # 4. Apply aggregated forest mask
  # ---------------------------------------------------------
  
  r_presence <- mask(
    r_presence,
    forest_ds,
    maskvalues = 0
  )
  
  
  # ---------------------------------------------------------
  # 5. Convert plotting raster to dataframe
  # ---------------------------------------------------------
  
  df <- as.data.frame(
    r_presence,
    xy = TRUE,
    na.rm = TRUE
  )
  
  
  names(df) <- c(
    "x",
    "y",
    "value"
  )
  
  
  return(df)
}


################################################################################
#### PREPARE ALL TEMPORAL SPATIAL RASTERS
################################################################################

temporal_spatial_dfs <- vector(
  "list",
  nrow(temporal_spatial_paths)
)


for (i in seq_len(nrow(temporal_spatial_paths))) {
  
  message(
    "Preparing ",
    temporal_spatial_paths$compound[i],
    " — ",
    temporal_spatial_paths$period[i]
  )
  
  
  df <- temporal_spatial_to_df(
    temporal_spatial_paths$path[i]
  )
  
  
  # Add identifiers
  df$compound <- temporal_spatial_paths$compound[i]
  df$period <- temporal_spatial_paths$period[i]
  
  
  temporal_spatial_dfs[[i]] <- df
}


# Combine all 16 plotting dataframes
temporal_spatial_df <- bind_rows(
  temporal_spatial_dfs
)


# ===========================================================
# Add labels, ordering, and plotting colors
# ===========================================================

temporal_spatial_df <- temporal_spatial_df %>%
  mutate(
    
    compound_label = factor(
      temporal_compound_labels[compound],
      levels = temporal_compound_labels[
        temporal_compounds
      ]
    ),
    
    period_label = factor(
      temporal_period_labels[period],
      levels = temporal_period_labels[
        temporal_periods
      ]
    ),
    
    plot_color = temporal_compound_colors[
      compound
    ]
  )


################################################################################
#### PLOT TEMPORAL SPATIAL FOOTPRINT
################################################################################

# Small data frame used only to identify the facet that should receive
# the scale bar and north arrow.
#
# Bottom-left panel:
# Wildfire + Biotic + Drought, 2000-2004

map_annotation_df <- data.frame(
  compound_label = factor(
    "Wildfire + Biotic + Drought",
    levels = temporal_compound_labels[
      temporal_compounds
    ]
  ),
  period_label = factor(
    "2000–2004",
    levels = temporal_period_labels[
      temporal_periods
    ]
  )
)


temporal_spatial_plot <- ggplot() +
  
  # ---------------------------------------------------------
# Forest background
# ---------------------------------------------------------

geom_raster(
  data = forest_df,
  aes(
    x = x,
    y = y
  ),
  fill = "grey95"
) +
  
  
  # ---------------------------------------------------------
# Compound-disturbance spatial footprint
#
# Each compound has one solid color.
# ---------------------------------------------------------

geom_raster(
  data = temporal_spatial_df,
  aes(
    x = x,
    y = y,
    fill = plot_color
  )
) +
  
  scale_fill_identity() +
  
  
  # ---------------------------------------------------------
# Ecoregion boundaries
# ---------------------------------------------------------

geom_sf(
  data = west_eco,
  fill = NA,
  color = "grey75",
  linewidth = 0.08
) +
  
  
  # ---------------------------------------------------------
# Western US outline
# ---------------------------------------------------------

geom_sf(
  data = west_outline,
  fill = NA,
  color = "black",
  linewidth = 0.2
) +
  
  
  # ---------------------------------------------------------
# Scale bar
#
# The data argument restricts this layer to the facet
# represented in map_annotation_df.
# ---------------------------------------------------------

annotation_scale(
  data = map_annotation_df,
  location = "bl",
  width_hint = 0.2,
  text_cex = 0.4,
  pad_x = unit(
    0.2,
    "cm"
  ),
  pad_y = unit(
    0.2,
    "cm"
  )
) +
  
  
  # ---------------------------------------------------------
# North arrow
#
# Also restricted to the bottom-left facet.
# ---------------------------------------------------------

annotation_north_arrow(
  data = map_annotation_df,
  location = "bl",
  which_north = "true",
  style = north_arrow_fancy_orienteering(
    text_size = 6
  ),
  height = unit(
    0.5,
    "cm"
  ),
  width = unit(
    0.5,
    "cm"
  ),
  pad_x = unit(
    0.2,
    "cm"
  ),
  pad_y = unit(
    0.5,
    "cm"
  )
) +
  
  
  # ---------------------------------------------------------
# Facets
#
# switch = "y" moves the row labels from the right side
# to the left side.
# ---------------------------------------------------------

facet_grid(
  rows = vars(compound_label),
  cols = vars(period_label),
  switch = "y"
) +
  
  
  # ---------------------------------------------------------
# Geographic extent
# ---------------------------------------------------------

coord_sf(
  xlim = c(
    west_extent["xmin"],
    west_extent["xmax"]
  ),
  ylim = c(
    west_extent["ymin"],
    west_extent["ymax"]
  ),
  expand = FALSE
) +
  
  
  # ---------------------------------------------------------
# Theme
# ---------------------------------------------------------

theme_void() +
  
  theme(
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    
    # -------------------------------------------------------
    # Column labels
    # -------------------------------------------------------
    
    strip.text.x = element_text(
      size = 10,
      face = "bold",
      margin = margin(
        b = 4
      )
    ),
    
    
    # -------------------------------------------------------
    # Row labels
    #
    # angle = 90 makes these read like y-axis labels.
    # -------------------------------------------------------
    
    strip.text.y.left = element_text(
      size = 9,
      face = "bold",
      angle = 90,
      margin = margin(
        r = 4
      )
    ),
    
    
    # Remove facet-strip backgrounds
    strip.background = element_blank(),
    
    
    # Put switched strips outside the plotting panels
    strip.placement = "outside",
    
    
    # Tight spacing between maps
    panel.spacing = unit(
      0.08,
      "cm"
    ),
    
    
    # No legend
    legend.position = "none"
  )


# Display
temporal_spatial_plot


################################################################################
#### SAVE FIGURE
################################################################################

ggsave(
  filename = paste0(
    "figures/temporal_patterns/",
    "temporal_progression_spatial_footprint.png"
  ),
  plot = temporal_spatial_plot,
  width = 7,
  height = 8,
  dpi = 300,
  units = "in",
  bg = "white"
)
