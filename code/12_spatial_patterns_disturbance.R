### Forest-disturbance-stack-v3

### Spatial patterns of disturbance

### Matt Bitters
### matthew.bitters@colorado.edu

################################################################################

# Working directory
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

# Packages
packages <- c("here", "terra", "sf", "dplyr", "ggplot2", "patchwork", "tidyr", "ragg", "ggspatial")
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


# Spatial presence rasters
raster_paths_sp <- list(
  ### Any disturbance
  
  # Singles
  wf_any_sp = "data/derived/annual_stacks/binary/spatial_presence/any/wf_any_presence.tif",
  bt_any_sp = "data/derived/annual_stacks/binary/spatial_presence/any/bt_any_presence.tif",
  hd_any_sp = "data/derived/annual_stacks/binary/spatial_presence/any/hd_any_presence.tif",
  pd_any_sp = "data/derived/annual_stacks/binary/spatial_presence/any/pd_any_presence.tif",

  # Doubles
  wf_bt_any_sp = "data/derived/annual_stacks/binary/spatial_presence/any/wf_bt_any_presence.tif",
  wf_hd_any_sp = "data/derived/annual_stacks/binary/spatial_presence/any/wf_hd_any_presence.tif",
  wf_pd_any_sp = "data/derived/annual_stacks/binary/spatial_presence/any/wf_pd_any_presence.tif",
  bt_hd_any_sp = "data/derived/annual_stacks/binary/spatial_presence/any/bt_hd_any_presence.tif",
  bt_pd_any_sp = "data/derived/annual_stacks/binary/spatial_presence/any/bt_pd_any_presence.tif",

  # Triples
  wf_bt_hd_any_sp = "data/derived/annual_stacks/binary/spatial_presence/any/wf_bt_hd_any_presence.tif",
  wf_bt_pd_any_sp = "data/derived/annual_stacks/binary/spatial_presence/any/wf_bt_pd_any_presence.tif",

  ### Extreme disturbance

  # Singles
  wf_extr_sp = "data/derived/annual_stacks/binary/spatial_presence/extreme/wf_extreme_presence.tif",
  bt_extr_sp = "data/derived/annual_stacks/binary/spatial_presence/extreme/bt_extreme_presence.tif",
  hd_extr_sp = "data/derived/annual_stacks/binary/spatial_presence/extreme/hd_extreme_presence.tif",
  pd_extr_sp = "data/derived/annual_stacks/binary/spatial_presence/extreme/pd_extreme_presence.tif",

  # Doubles
  wf_bt_extr_sp = "data/derived/annual_stacks/binary/spatial_presence/extreme/wf_bt_extreme_presence.tif",
  wf_hd_extr_sp = "data/derived/annual_stacks/binary/spatial_presence/extreme/wf_hd_extreme_presence.tif",
  wf_pd_extr_sp = "data/derived/annual_stacks/binary/spatial_presence/extreme/wf_pd_extreme_presence.tif",
  bt_hd_extr_sp = "data/derived/annual_stacks/binary/spatial_presence/extreme/bt_hd_extreme_presence.tif",
  bt_pd_extr_sp = "data/derived/annual_stacks/binary/spatial_presence/extreme/bt_pd_extreme_presence.tif",

  # Triples
  wf_bt_hd_extr_sp = "data/derived/annual_stacks/binary/spatial_presence/extreme/wf_bt_hd_extreme_presence.tif",
  wf_bt_pd_extr_sp = "data/derived/annual_stacks/binary/spatial_presence/extreme/wf_bt_pd_extreme_presence.tif"
)

################################################################################

# ===========================================================
# Visualization settings
# ===========================================================

# Load forest mask as raster 
forest <- rast(forest_mask)

# Original rasters are 30 m.
# A factor of 10 creates 300 m plotting cells.
plot_agg_factor <- 10

# Neighborhood size used to smooth the aggregated raster.
# At 300 m resolution, a 5 x 5 window spans approximately 1.5 km.
focal_window <- 5

# Minimum value of the smoothed surface retained as broad
# disturbance presence. Lower values preserve more scattered
# disturbance; higher values emphasize denser regional patterns.
smooth_threshold <- 0.05

# Aggregate forest mask for plotting only.
# max retains a plotting cell when any underlying 30 m cell is forest.
forest_ds <- aggregate(
  forest,
  fact = plot_agg_factor,
  fun = max,
  na.rm = TRUE
)


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
# Function to prepare and smooth raster for plotting
# ===========================================================
raster_to_df <- function(
    r_path,
    agg_factor = plot_agg_factor,
    window_size = focal_window,
    threshold = smooth_threshold
) {
  
  # Load original 30 m binary raster
  r <- rast(r_path)
  
  # ---------------------------------------------------------
  # 1. Aggregate for faster visualization
  # ---------------------------------------------------------
  # The mean is used only as an intermediate summary.
  # It represents the fraction of disturbed 30 m cells within
  # each aggregated plotting cell.
  r_agg <- aggregate(
    r,
    fact = agg_factor,
    fun = mean,
    na.rm = TRUE
  )
  
  # ---------------------------------------------------------
  # 2. Smooth the broad spatial pattern
  # ---------------------------------------------------------
  # A normalized focal-weight matrix calculates the local mean.
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
  # 3. Convert the smoothed result back to presence/absence
  # ---------------------------------------------------------
  # Values below the threshold become NA so they are not plotted.
  # Retained cells are assigned a value of 1.
  r_presence <- ifel(
    r_smooth >= threshold,
    1,
    NA
  )
  
  # ---------------------------------------------------------
  # 4. Convert plotting raster to a data frame
  # ---------------------------------------------------------
  df <- as.data.frame(
    r_presence,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(df) <- c("x", "y", "value")
  
  return(df)
}

# Prepare all raster dfs
raster_dfs_sp <- lapply(raster_paths_sp, raster_to_df)

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


# Combine all disturbance layers into a single dataframe for colors
colors_df <- bind_rows(
  raster_dfs_sp$wf_any_sp %>% mutate(disturbance = "WF (any)"),
  raster_dfs_sp$bt_any_sp %>% mutate(disturbance = "BT (any)"),
  raster_dfs_sp$hd_any_sp %>% mutate(disturbance = "HD (any)"),
  raster_dfs_sp$pd_any_sp %>% mutate(disturbance = "PD (any)"),
  
  raster_dfs_sp$wf_extr_sp %>% mutate(disturbance = "WF (extr)"),
  raster_dfs_sp$bt_extr_sp %>% mutate(disturbance = "BT (extr)"),
  raster_dfs_sp$hd_extr_sp %>% mutate(disturbance = "HD (extr)"),
  raster_dfs_sp$pd_extr_sp %>% mutate(disturbance = "PD (extr)"),

  raster_dfs_sp$wf_hd_any_sp %>% mutate(disturbance = "WF + HD (any)"),
  raster_dfs_sp$bt_hd_any_sp %>% mutate(disturbance = "BT + HD (any)"),
  raster_dfs_sp$wf_bt_any_sp %>% mutate(disturbance = "WF + BT (any)"),
  raster_dfs_sp$wf_pd_any_sp %>% mutate(disturbance = "WF + PD (any)"),
  raster_dfs_sp$bt_pd_any_sp %>% mutate(disturbance = "BT + PD (any)"),
  raster_dfs_sp$wf_bt_hd_any_sp %>% mutate(disturbance = "WF + BT + HD (any)"),
  raster_dfs_sp$wf_bt_pd_any_sp %>% mutate(disturbance = "WF + BT + PD (any)"),
  
  raster_dfs_sp$wf_hd_extr_sp %>% mutate(disturbance = "WF + HD (extr)"),
  raster_dfs_sp$bt_hd_extr_sp %>% mutate(disturbance = "BT + HD (extr)"),
  raster_dfs_sp$wf_bt_extr_sp %>% mutate(disturbance = "WF + BT (extr)"),
  raster_dfs_sp$wf_pd_extr_sp %>% mutate(disturbance = "WF + PD (extr)"),
  raster_dfs_sp$bt_pd_extr_sp %>% mutate(disturbance = "BT + PD (extr)"),
  raster_dfs_sp$wf_bt_hd_extr_sp %>% mutate(disturbance = "WF + BT + HD (extr)"),
  raster_dfs_sp$wf_bt_pd_extr_sp %>% mutate(disturbance = "WF + BT + PD (extr)")
)


# Panel titles and legend labels

wf_legend_labels <- c(
  "WF (any)"  = "CBI > 0.1",
  "WF (extr)" = "CBI ≥ 2.25"
)

bt_legend_labels <- c(
  "BT (any)"  = "% > 0.5",
  "BT (extr)" = "% ≥ 14.94"
)

hd_legend_labels <- c(
  "HD (any)"  = "Index ≥ 4",
  "HD (extr)" = "Index ≥ 6"
)

wf_bt_hd_legend_labels <- c(
  "WF + BT + HD (any)"  = "Any disturbance\nthresholds",
  "WF + BT + HD (extr)" = "Extreme disturbance\nthresholds"
)

# ===========================================================
# Four-panel plot
# ===========================================================

# wf any/extr
p_wf <- ggplot() +
  base_map +
  geom_raster(data = filter(colors_df, disturbance %in% c("WF (any)", "WF (extr)")), aes(x = x, y = y, fill = disturbance), alpha = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "WF (any)"    = "orangered1",
      "WF (extr)"   = "orangered4"
    ),
    breaks = c("WF (any)", "WF (extr)"),
    labels = wf_legend_labels
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Burned area by\nwildfire") +
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
  )

# bt any/extr
p_bt <- ggplot() +
  base_map +
  geom_raster(data = filter(colors_df, disturbance %in% c("BT (any)", "BT (extr)")), aes(x = x, y = y, fill = disturbance), alpha = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "BT (any)"    = "goldenrod1",
      "BT (extr)"   = "goldenrod4"
    ),
    breaks = c("BT (any)", "BT (extr)"),
    labels = bt_legend_labels
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Bark beetle tree\nmortality ")

# hd any/extr
p_hd <- ggplot() +
  base_map +
  geom_raster(data = filter(colors_df, disturbance %in% c("HD (any)", "HD (extr)")), aes(x = x, y = y, fill = disturbance), alpha = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "HD (any)"    = "dodgerblue1",
      "HD (extr)"   = "dodgerblue4"
    ),
    breaks = c("HD (any)", "HD (extr)"),
    labels = hd_legend_labels
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Hotter drought index")

# wf, bt, hd any/extr
p_wf_bt_hd <- ggplot() +
  base_map +
  geom_raster(data = filter(colors_df, disturbance %in% c("WF + BT + HD (any)", "WF + BT + HD (extr)")), aes(x = x, y = y, fill = disturbance), alpha = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "WF + BT + HD (any)"    = "darkorchid1",
      "WF + BT + HD (extr)"   = "darkorchid4"
    ),
    breaks = c("WF + BT + HD (any)", "WF + BT + HD (extr)"),
    labels = wf_bt_hd_legend_labels
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Disturbance interactions")


# Combine panels
four_panel <- p_wf | p_bt | p_hd | p_wf_bt_hd 

# Display
four_panel

# Save
ggsave(
  filename = "figs/spatial_patterns/spatial_patterns_four_panel.png",
  plot     = four_panel,
  width    = 7,        # inches 
  height   = 4,         # inches 
  dpi      = 300,
  units    = "in",
  bg       = "white"
)





# ===========================================================
# Eight-panel plot
# ===========================================================

### Any disturbance

# wf any
p_wf_any <- ggplot() +
  base_map +
  geom_raster(data = filter(colors_df, disturbance %in% c("WF (any)")), aes(x = x, y = y, fill = disturbance), alpha = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "WF (any)"    = "orangered1"
    )
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom") +
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
  )

# bt any
p_bt_any <- ggplot() +
  base_map +
  geom_raster(data = filter(colors_df, disturbance %in% c("BT (any)")), aes(x = x, y = y, fill = disturbance), alpha = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "BT (any)"    = "goldenrod1"
    )
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom")

# hd any
p_hd_any <- ggplot() +
  base_map +
  geom_raster(data = filter(colors_df, disturbance %in% c("HD (any)")), aes(x = x, y = y, fill = disturbance), alpha = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "HD (any)"    = "dodgerblue1"
    )
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom")

# combos any
p_wf_bt_hd_any <- ggplot() +
  base_map +
  geom_raster(data = filter(colors_df, disturbance %in% c("WF + BT (any)", "WF + HD (any)", "BT + HD (any)", "WF + BT + HD (any)")), aes(x = x, y = y, fill = disturbance), alpha = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "WF + BT (any)"      = "firebrick1",
      "WF + HD (any)"      = "darkolivegreen1",
      "BT + HD (any)"      = "darkorchid1",
      "WF + BT + HD (any)" = "darkgoldenrod1"
    )
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom")


### Extreme disturbance

# wf extr
p_wf_extr <- ggplot() +
  base_map +
  geom_raster(data = filter(colors_df, disturbance %in% c("WF (extr)")), aes(x = x, y = y, fill = disturbance), alpha = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "WF (extr)"    = "orangered4"
    )
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom")

# bt extr
p_bt_extr <- ggplot() +
  base_map +
  geom_raster(data = filter(colors_df, disturbance %in% c("BT (extr)")), aes(x = x, y = y, fill = disturbance), alpha = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "BT (extr)"    = "goldenrod4"
    )
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom")

# hd extr
p_hd_extr <- ggplot() +
  base_map +
  geom_raster(data = filter(colors_df, disturbance %in% c("HD (extr)")), aes(x = x, y = y, fill = disturbance), alpha = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "HD (extr)"    = "dodgerblue4"
    )
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom")

# combos extr
p_wf_bt_hd_extr <- ggplot() +
  base_map +
  geom_raster(data = filter(colors_df, disturbance %in% c("WF + BT (extr)", "WF + HD (extr)", "BT + HD (extr)", "WF + BT + HD (extr)")), aes(x = x, y = y, fill = disturbance), alpha = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "WF + BT (extr)"      = "firebrick4",
      "WF + HD (extr)"      = "darkolivegreen4",
      "BT + HD (extr)"      = "darkorchid4",
      "WF + BT + HD (extr)" = "darkgoldenrod4"
    )
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom")


# Combine panels
eight_panel <- (p_wf_any | p_bt_any | p_hd_any | p_wf_bt_hd_any) / 
               (p_wf_extr | p_bt_extr | p_hd_extr | p_wf_bt_hd_extr) +
  theme(plot.margin = margin(.1, 1, .1, 1))
  
  
# Display
eight_panel
  
# Save
ggsave(
  filename = "figs/spatial_patterns/spatial_patterns_eight_panel.png",
  plot     = eight_panel,
  width    = 6,        
  height   = 8,         
  dpi      = 300,
  units    = "in",
  bg       = "white"
)









































################################################################################



