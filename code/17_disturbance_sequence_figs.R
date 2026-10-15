### Forest-disturbance-stack-v3

### Disturbance sequences

### Matt Bitters
### matthew.bitters@colorado.edu

################################################################################

# Working directory
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

# Packages
packages <- c("here", "terra", "sf", "dplyr", "ggplot2", "patchwork", "tidyr", "ragg", "ggspatial", "tibble", "ggalluvial", "scales")
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
library(ggalluvial)
library(scales)
library(tibble)


terraOptions(
  tempdir = "tmp_terra",
  memfrac = 0.6,    # use 60% of RAM
  memmax = 180000,   # cap memory at 180 GB
  progress = 1
)

################################################################################


################################################################################
# Forest Disturbance Stack
# Wildfire-centered temporal disturbance patterns
# Balanced ±5-year analysis
################################################################################



summary_dir <- "data/derived/disturbance_sequence/summaries"
figure_dir  <- "figures/wildfire_context"

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

#------------------------------------------------------------------------------
# 2. Load and prepare data
#------------------------------------------------------------------------------

df <- read_csv(
  file.path(summary_dir, "wildfire_context_balanced_5yr.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    threshold = factor(
      threshold,
      levels = c("any", "extreme"),
      labels = c("Any disturbance", "Extreme disturbance")
    ),
    context_disturbance = factor(
      context_disturbance,
      levels = c("BT", "HD"),
      labels = c("Biotic disturbance", "Hotter drought")
    )
  )

#------------------------------------------------------------------------------
# 3. Colors
#------------------------------------------------------------------------------

disturbance_colors <- c(
  "Any disturbance"     = "#4D4D4D",
  "Extreme disturbance" = "#D95F02"
)

#------------------------------------------------------------------------------
# 4. Create figure
#------------------------------------------------------------------------------

p <- ggplot(
  df,
  aes(
    x = relative_year,
    y = percent,
    color = threshold,
    group = threshold
  )
) +
  
  # Wildfire year
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey65",
    linewidth = 0.5
  ) +
  
  # Temporal lines
  geom_line(
    linewidth = 1.1
  ) +
  
  # Annual observations
  geom_point(
    size = 1.8
  ) +
  
  # Two panels
  facet_wrap(
    ~ context_disturbance,
    nrow = 1,
    scales = "free_y"
  ) +
  
  # Colors
  scale_color_manual(
    values = disturbance_colors
  ) +
  
  # X-axis
  scale_x_continuous(
    breaks = -5:5,
    limits = c(-5, 5)
  ) +
  
  # Y-axis
  scale_y_continuous(
    labels = scales::label_number(
      accuracy = 1,
      suffix = "%"
    ),
    expand = expansion(mult = c(0.03, 0.08))
  ) +
  
  # Labels
  labs(
    x = "Years relative to wildfire",
    y = "Wildfire pixel-events \nwith disturbance (%)",
    color = NULL
  ) +
  
  # Theme
  theme_classic(base_size = 12) +
  
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 12,
      hjust = 0
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.key.width = grid::unit(1.5, "cm"),
    panel.spacing = grid::unit(1.5, "lines"),
    axis.text.x = element_text(size = 9),
    axis.title = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10)
  )

# Display
print(p)

#------------------------------------------------------------------------------
# 5. Save figure
#------------------------------------------------------------------------------

ggsave(
  filename = file.path(
    figure_dir,
    "wildfire_context_balanced_5yr.png"
  ),
  plot = p,
  width = 6,
  height = 3.5,
  bg = "white"
)





################################################################################







################################################################################
# Forest Disturbance Stack
# First-observed extreme disturbance sequences: Alluvial diagram
# 2000–2020
################################################################################





input_dir <- "data/derived/disturbance_sequence/extreme_sequences"
figure_dir <- "figures/extreme_sequences"

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Load sequence data
df <- read_csv(
  file.path(input_dir, "extreme_sequences_alluvial.csv"),
  show_col_types = FALSE
) %>%
  filter(n_disturbance_types >= 2)

# Collapse same-year combinations for visualization
simplify_stage <- function(x) {
  ifelse(grepl("+", x, fixed = TRUE), "Simultaneous", x)
}

df_simple <- df %>%
  filter(
    !grepl("\\+", stage_1),
    !grepl("\\+", stage_2),
    stage_2 != "END"
  ) %>%
  group_by(
    first = stage_1,
    second = stage_2
  ) %>%
  summarise(
    area_km2 = sum(area_km2),
    .groups = "drop"
  )

# Verify that all included area is represented
cat(
  "Area represented:",
  sum(df_simple$area_km2),
  "km²\n"
)

stage_levels <- c(
  "WF", "BT", "HD"
)

df_simple <- df_simple %>%
  mutate(
    first = factor(first, levels = stage_levels),
    second = factor(second, levels = stage_levels)
  )

stage_levels <- c("WF", "BT", "HD")

stage_colors <- c(
  "WF" = "#D95F02",
  "BT" = "#4DAF4A",
  "HD" = "#377EB8"
)

p_simple <- ggplot(
  df_simple,
  aes(
    axis1 = first,
    axis2 = second,
    y = area_km2
  )
) +
  geom_alluvium(
    aes(fill = first),
    width = 0.12,
    alpha = 0.65,
    color = NA
  ) +
  geom_stratum(
    width = 0.12,
    fill = "grey95",
    color = "grey50",
    linewidth = 0.3
  ) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 4
  ) +
  scale_x_discrete(
    limits = c("First observed", "Second observed"),
    expand = c(0.12, 0.12)
  ) +
  scale_fill_manual(values = stage_colors) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  labs(
    x = NULL,
    y = "Forest area (km²)",
    fill = "First observed",
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(face = "bold", size = 11, color = "black"),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(12, 15, 12, 15)
  )

print(p_simple)

ggsave(
  file.path(figure_dir, "extreme_sequences_alluvial_simple.png"),
  p_simple,
  width = 4.5,
  height = 4,
  dpi = 300,
  bg = "white"
)



################################################################################









################################################################################
# Forest Disturbance Stack
# Two-panel alluvial figure: extreme disturbance sequences
# Western U.S. forests, 2000–2020
################################################################################



# ==============================================================================
# 1. DIRECTORIES
# ==============================================================================



input_dir <- "data/derived/disturbance_sequence/extreme_sequences"
figure_dir <- "figures/extreme_sequences"

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 2. LOAD DATA
# ==============================================================================

df <- read_csv(
  file.path(input_dir, "extreme_sequences_alluvial.csv"),
  show_col_types = FALSE
)

# ==============================================================================
# 3. FILTER TO STRICTLY ORDERED SEQUENCES
# ==============================================================================

# A strictly ordered sequence contains no simultaneous (+) stages.
# Keep only pixels with exactly 2 or exactly 3 disturbance types.

df_ordered <- df %>%
  filter(
    n_disturbance_types %in% c(2, 3),
    !grepl("+", stage_1, fixed = TRUE),
    !grepl("+", stage_2, fixed = TRUE),
    !grepl("+", stage_3, fixed = TRUE)
  )

# Check how much area is retained in each panel.

area_summary <- df %>%
  filter(n_disturbance_types %in% c(2, 3)) %>%
  group_by(n_disturbance_types) %>%
  summarise(
    total_km2 = sum(area_km2),
    .groups = "drop"
  ) %>%
  left_join(
    df_ordered %>%
      group_by(n_disturbance_types) %>%
      summarise(
        plotted_km2 = sum(area_km2),
        .groups = "drop"
      ),
    by = "n_disturbance_types"
  ) %>%
  mutate(
    plotted_km2 = replace_na(plotted_km2, 0),
    excluded_km2 = total_km2 - plotted_km2,
    percent_retained = 100 * plotted_km2 / total_km2
  )

print(area_summary)

# ==============================================================================
# 4. COLORS AND FACTOR LEVELS
# ==============================================================================

stage_levels <- c("WF", "BT", "HD")

stage_colors <- c(
  "WF" = "#D95F02",
  "BT" = "#4DAF4A",
  "HD" = "#377EB8"
)

# ==============================================================================
# 5. PANEL A: EXACTLY TWO DISTURBANCE TYPES
# ==============================================================================

df_two <- df_ordered %>%
  filter(n_disturbance_types == 2) %>%
  group_by(
    first = stage_1,
    second = stage_2
  ) %>%
  summarise(
    area_km2 = sum(area_km2),
    .groups = "drop"
  ) %>%
  mutate(
    first = factor(first, levels = stage_levels),
    second = factor(second, levels = stage_levels)
  )

p_two <- ggplot(
  df_two,
  aes(
    axis1 = first,
    axis2 = second,
    y = area_km2
  )
) +
  geom_alluvium(
    aes(fill = first),
    width = 0.14,
    alpha = 0.75,
    color = NA
  ) +
  geom_stratum(
    width = 0.14,
    fill = "grey95",
    color = "grey45",
    linewidth = 0.35
  ) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 4,
    color = "black"
  ) +
  scale_x_discrete(
    limits = c("First observed", "Second observed"),
    expand = c(0.12, 0.12)
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_fill_manual(values = stage_colors) +
  labs(
    x = NULL,
    y = "Forest area (km²)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(
      size = 11,
      color = "black"
    ),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 15, 10, 15)
  )

# ==============================================================================
# 6. PANEL B: EXACTLY THREE DISTURBANCE TYPES
# ==============================================================================

df_three <- df_ordered %>%
  filter(n_disturbance_types == 3) %>%
  group_by(
    first = stage_1,
    second = stage_2,
    third = stage_3
  ) %>%
  summarise(
    area_km2 = sum(area_km2),
    .groups = "drop"
  ) %>%
  mutate(
    first = factor(first, levels = stage_levels),
    second = factor(second, levels = stage_levels),
    third = factor(third, levels = stage_levels)
  )

p_three <- ggplot(
  df_three,
  aes(
    axis1 = first,
    axis2 = second,
    axis3 = third,
    y = area_km2
  )
) +
  geom_alluvium(
    aes(fill = first),
    width = 0.28,
    alpha = 0.75,
    color = NA
  ) +
  geom_stratum(
    width = 0.28,
    fill = "grey95",
    color = "grey45",
    linewidth = 0.35
  ) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 4,
    color = "black"
  ) +
  scale_x_discrete(
    limits = c(
      "First observed",
      "Second observed",
      "Third observed"
    ),
    expand = c(0.12, 0.12)
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_fill_manual(values = stage_colors) +
  labs(
    x = NULL,
    y = "Forest area (km²)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(
      size = 11,
      color = "black"
    ),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 15, 10, 15)
  )

# ==============================================================================
# 7. COMBINE PANELS
# ==============================================================================

p_combined <- (
  p_two / p_three
) +
  plot_layout(heights = c(1, 1)) 

print(p_combined)

# ==============================================================================
# 8. SAVE FIGURE
# ==============================================================================

ggsave(
  file.path(
    figure_dir,
    "extreme_sequences_alluvial_two_panel.png"
  ),
  p_combined,
  width = 6,
  height = 5,
  dpi = 300,
  bg = "white"
)





################################################################################






################################################################################
# Forest Disturbance Stack
# Extreme disturbance transition lags
# Western U.S. forests, 2000–2020
################################################################################



# ==============================================================================
# 1. DIRECTORIES
# ==============================================================================



input_dir <- "data/derived/disturbance_sequence/extreme_sequences"
figure_dir <- "figures/extreme_sequences"

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 2. LOAD DATA
# ==============================================================================

df <- read_csv(
  file.path(input_dir, "extreme_transition_lags_westwide.csv"),
  show_col_types = FALSE
)

print(names(df))
print(head(df))

# ==============================================================================
# 3. PREPARE TRANSITION DATA
# ==============================================================================

# Expected columns:
# from, to, lag_years, area_km2

required_cols <- c("from_stage", "to_stage", "lag_years", "area_km2")

if (!all(required_cols %in% names(df))) {
  stop(
    "Unexpected CSV column names. Available columns: ",
    paste(names(df), collapse = ", ")
  )
}

transition_levels <- c(
  "WF → BT",
  "WF → HD",
  "BT → WF",
  "BT → HD",
  "HD → WF",
  "HD → BT"
)

df_plot <- df %>%
  mutate(
    transition = paste(from_stage, to_stage, sep = " → ")
  ) %>%
  filter(
    transition %in% transition_levels,
    lag_years >= 1,
    lag_years <= 20
  ) %>%
  group_by(transition, lag_years) %>%
  summarise(
    area_km2 = sum(area_km2),
    .groups = "drop"
  ) %>%
  group_by(transition) %>%
  mutate(
    total_area_km2 = sum(area_km2),
    percent = 100 * area_km2 / total_area_km2
  ) %>%
  ungroup() %>%
  complete(
    transition = transition_levels,
    lag_years = 1:20,
    fill = list(area_km2 = 0, percent = 0)
  ) %>%
  mutate(
    transition = factor(
      transition,
      levels = rev(transition_levels)
    )
  )

# ==============================================================================
# 4. TRANSITION TOTALS
# ==============================================================================

transition_totals <- df_plot %>%
  group_by(transition) %>%
  summarise(
    total_area_km2 = sum(area_km2),
    .groups = "drop"
  )

print(transition_totals)

# Create labels with total transition area
transition_labels <- transition_totals %>%
  mutate(
    label = paste0(
      as.character(transition),
      "\n",
      comma(round(total_area_km2)),
      " km²"
    )
  ) %>%
  select(transition, label) %>%
  deframe()

# ==============================================================================
# 5. HEATMAP
# ==============================================================================

p_lags <- ggplot(
  df_plot,
  aes(
    x = lag_years,
    y = transition,
    fill = percent
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.35,
    width = 0.95,
    height = 0.85
  ) +
  geom_text(
    aes(
      label = ifelse(percent >= 10, sprintf("%.1f", percent), "")
    ),
    size = 3,
    color = "black"
  ) +
  scale_fill_stepsn(
    colors = c(
      "#FFF5EB",
      "#FDD0A2",
      "#FDAE6B",
      "#F16913",
      "#A63603"
    ),
    breaks = c(0, 2, 5, 10, 20, 50),
    limits = c(0, 50),
    labels = label_number(suffix = "%"),
    name = "Transition area (%)"
  ) +
  scale_x_continuous(
    breaks = c(1, 5, 10, 15, 20),
    expand = expansion(add = 0.5)
  ) +
  scale_y_discrete(
    labels = transition_labels,
    expand = expansion(add = 0.5)
  ) + 
  labs(
    x = "Years between first observations",
    y = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.y = element_text(
      color = "black",
      size = 11
    ),
    axis.text.x = element_text(
      color = "black",
      size = 10
    ),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(12, 15, 12, 15)
  )

print(p_lags)

# ==============================================================================
# 6. SAVE FIGURE
# ==============================================================================

ggsave(
  file.path(
    figure_dir,
    "extreme_transition_lag_heatmap.png"
  ),
  p_lags,
  width = 7,
  height = 4,
  dpi = 300,
  bg = "white"
)









