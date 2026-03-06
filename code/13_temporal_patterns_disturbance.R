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
raster_paths <- list(
  ### Any disturbance
  
  # Singles
  wf_any = "data/derived/annual_stacks/binary/frequency/wf_any_frequency.tif",
  bt_any = "data/derived/annual_stacks/binary/frequency/bt_any_frequency.tif",
  hd_any = "data/derived/annual_stacks/binary/frequency/hd_any_frequency.tif",
  pd_any = "data/derived/annual_stacks/binary/frequency/pd_any_frequency.tif",
  
  # Doubles
  wf_bt_any = "data/derived/annual_stacks/binary/frequency/wf_bt_any_frequency.tif",
  wf_hd_any = "data/derived/annual_stacks/binary/frequency/wf_hd_any_frequency.tif",
  wf_pd_any = "data/derived/annual_stacks/binary/frequency/wf_pd_any_frequency.tif",
  bt_hd_any = "data/derived/annual_stacks/binary/frequency/bt_hd_any_frequency.tif",
  bt_pd_any = "data/derived/annual_stacks/binary/frequency/bt_pd_any_frequency.tif",
  
  # Triples
  wf_bt_hd_any = "data/derived/annual_stacks/binary/frequency/wf_bt_hd_any_frequency.tif",
  wf_bt_pd_any = "data/derived/annual_stacks/binary/frequency/wf_bt_pd_any_frequency.tif",
  
  ### Extreme disturbance
  
  # Singles
  wf_extr = "data/derived/annual_stacks/binary/frequency/wf_extreme_frequency.tif",
  bt_extr = "data/derived/annual_stacks/binary/frequency/bt_extreme_frequency.tif",
  hd_extr = "data/derived/annual_stacks/binary/frequency/hd_extreme_frequency.tif",
  pd_extr = "data/derived/annual_stacks/binary/frequency/pd_extreme_frequency.tif",
  
  # Doubles
  wf_bt_extr = "data/derived/annual_stacks/binary/frequency/wf_bt_extreme_frequency.tif",
  wf_hd_extr = "data/derived/annual_stacks/binary/frequency/wf_hd_extreme_frequency.tif",
  wf_pd_extr = "data/derived/annual_stacks/binary/frequency/wf_pd_extreme_frequency.tif",
  bt_hd_extr = "data/derived/annual_stacks/binary/frequency/bt_hd_extreme_frequency.tif",
  bt_pd_extr = "data/derived/annual_stacks/binary/frequency/bt_pd_extreme_frequency.tif",
  
  # Triples
  wf_bt_hd_extr = "data/derived/annual_stacks/binary/frequency/wf_bt_hd_extreme_frequency.tif",
  wf_bt_pd_extr = "data/derived/annual_stacks/binary/frequency/wf_bt_pd_extreme_frequency.tif"
)

################################################################################

# Load forest mask as raster
forest <- rast(forest_mask)

# Aggregation factor for faster plotting
agg_factor <- 10

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
raster_to_df <- function(r_path) {
  r <- rast(r_path)
  
  # If these frequency rasters were written with nodata=0, undo that
  NAflag(r) <- NA
  r[is.na(r)] <- 0
  
  r_agg <- aggregate(r, fact = agg_factor, fun = mean, na.rm = TRUE)
  
  df <- as.data.frame(r_agg, xy = TRUE, na.rm = TRUE)
  colnames(df) <- c("x", "y", "frequency")
  
  df <- df %>% filter(frequency > 0)
  return(df)
}

# Prepare all raster dfs
raster_dfs <- lapply(raster_paths, raster_to_df)

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
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.4, "cm")
  )
)


r <- rast(raster_paths$wf_any)
r
terra::global(r, c("min","max","mean"), na.rm = TRUE)
terra::freq(r, digits = 0, value = TRUE)
terra::nlyr(r)
terra::NAflag(r)


# ===========================================================
# Four-panel plot
# ===========================================================

# wf any
p_wf <- ggplot() +
  base_map +
  geom_raster(data = raster_dfs$wf_any, aes(x = x, y = y, fill = frequency)) +
  scale_fill_viridis_c(name = "WF freq", option = "magma") +
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
    )
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom")

# hd any/extr
p_hd <- ggplot() +
  base_map +
  geom_raster(data = filter(colors_df, disturbance %in% c("HD (any)", "HD (extr)")), aes(x = x, y = y, fill = disturbance), alpha = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "HD (any)"    = "dodgerblue1",
      "HD (extr)"   = "dodgerblue4"
    )
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom")

# wf, bt, hd any/extr
p_wf_bt_hd <- ggplot() +
  base_map +
  geom_raster(data = filter(colors_df, disturbance %in% c("WF + BT + HD (any)", "WF + BT + HD (extr)")), aes(x = x, y = y, fill = disturbance), alpha = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "WF + BT + HD (any)"    = "darkorchid1",
      "WF + BT + HD (extr)"   = "darkorchid4"
    )
  ) +
  geom_sf(data = west_eco, fill = NA, color = "grey75", linewidth = 0.08) +
  geom_sf(data = west_outline, fill = NA, color = "black", linewidth = 0.2) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom")


# Combine panels
four_panel <- p_wf | p_bt | p_hd | p_wf_bt_hd 

# Display
four_panel

# Save
ggsave(
  filename = "figs/spatial_patterns/spatial_patterns_four_panel.png",
  plot     = four_panel,
  width    = 6,        # inches 
  height   = 4,         # inches 
  dpi      = 300,
  units    = "in",
  bg       = "white"
)