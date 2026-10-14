### Forest-disturbance-stack-v3

### Disturbance sequences

### Matt Bitters
### matthew.bitters@colorado.edu

################################################################################

# Working directory
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

# Packages
packages <- c("here", "terra", "sf", "dplyr", "ggplot2", "patchwork", "tidyr", "ragg", "ggspatial", "ggalluvial", "scales")
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
      labels = c("A. Biotic disturbance", "B. Hotter drought")
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

ggsave(
  file.path(figure_dir, "extreme_sequences_alluvial_simple.pdf"),
  p_simple,
  width = 9,
  height = 6,
  bg = "white"
)