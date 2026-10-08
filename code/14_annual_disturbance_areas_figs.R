################################################################################
### Forest-disturbance-stack-v3

### Annual disturbance figures

### Matt Bitters
### matthew.bitters@colorado.edu

################################################################################
#
# Inputs:
#   data/derived/spatial_metrics/annual_spatial_metrics_west_any.csv
#   data/derived/spatial_metrics/annual_spatial_metrics_west_extreme.csv
#
# Outputs:
#   Annual and total disturbance-area figures
################################################################################


# ==============================================================================
# 1. Packages
# ==============================================================================

# Working directory
setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

# Packages
packages <- c("here", "tidyverse", "scales", "patchwork", "ggplot2", "patchwork")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) install.packages(packages[!installed])

library(here)
library(tidyverse)
library(scales)
library(ggplot2)
library(patchwork)


# ==============================================================================
# 2. Paths
# ==============================================================================

metrics_dir <- here(
  "data",
  "derived",
  "spatial_metrics"
)

fig_dir <- here(
  "figures",
  "annual_disturbance_area"
)

dir.create(
  fig_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 3. Read data
# ==============================================================================

any_dat <- read_csv(
  file.path(
    metrics_dir,
    "annual_spatial_metrics_west_any.csv"
  ),
  show_col_types = FALSE
) %>%
  mutate(mode = "Any")


extreme_dat <- read_csv(
  file.path(
    metrics_dir,
    "annual_spatial_metrics_west_extreme.csv"
  ),
  show_col_types = FALSE
) %>%
  mutate(mode = "Extreme")


dat <- bind_rows(
  any_dat,
  extreme_dat
)


# ==============================================================================
# 4. Disturbance labels and ordering
# ==============================================================================

disturbance_levels <- c(
  "wf",
  "bt",
  "hd",
  "pd",
  "wf_bt",
  "wf_hd",
  "wf_pd",
  "bt_hd",
  "bt_pd",
  "wf_bt_hd",
  "wf_bt_pd"
)


disturbance_labels <- c(
  "Wildfire",
  "Biotic",
  "Hotter drought",
  "PDSI drought",
  "Wildfire + Biotic",
  "Wildfire + Hotter drought",
  "Wildfire + PDSI drought",
  "Biotic + Hotter drought",
  "Biotic + PDSI drought",
  "Wildfire + Biotic + Hotter drought",
  "Wildfire + Biotic + PDSI drought"
)


dat <- dat %>%
  mutate(
    disturbance = factor(
      disturbance,
      levels = disturbance_levels,
      labels = disturbance_labels
    ),
    mode = factor(
      mode,
      levels = c(
        "Any",
        "Extreme"
      )
    )
  )


# ==============================================================================
# 5. Separate annual observations and totals
# ==============================================================================

annual_dat <- dat %>%
  filter(period != "Total") %>%
  mutate(
    year = as.integer(period)
  )


total_dat <- dat %>%
  filter(period == "Total")


# ==============================================================================
# 6. Basic checks
# ==============================================================================

print(
  annual_dat %>%
    group_by(mode, disturbance) %>%
    summarise(
      n_years = n(),
      min_year = min(year),
      max_year = max(year),
      .groups = "drop"
    )
)


print(
  total_dat %>%
    select(
      mode,
      disturbance,
      area_km2
    )
)


# ==============================================================================
# 7. Plot theme
# ==============================================================================

theme_disturbance <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    
    strip.background = element_blank(),
    
    strip.text = element_text(
      face = "bold"
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    plot.title = element_text(
      face = "bold"
    ),
    
    legend.position = "top"
  )


# ==============================================================================
# 8. FIGURE 1
# Annual area — all disturbance types, ANY
#
# Each panel represents one disturbance or disturbance combination.
# ==============================================================================

p_annual_any <- annual_dat %>%
  filter(mode == "Any") %>%
  ggplot(
    aes(
      x = year,
      y = area_km2
    )
  ) +
  geom_col(
    width = 0.8
  ) +
  facet_wrap(
    ~ disturbance,
    scales = "free_y",
    ncol = 3
  ) +
  scale_x_continuous(
    breaks = seq(
      2000,
      2020,
      by = 5
    )
  ) +
  scale_y_continuous(
    labels = label_number(
      big.mark = ","
    ),
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    title = "Annual forest disturbance area",
    subtitle = "Any disturbance thresholds, western U.S., 2000–2020",
    x = NULL,
    y = expression("Area (km"^2*")")
  ) +
  theme_disturbance


p_annual_any


ggsave(
  filename = file.path(
    fig_dir,
    "annual_disturbance_area_any.png"
  ),
  plot = p_annual_any,
  width = 12,
  height = 11,
  dpi = 300
)


# ==============================================================================
# 9. FIGURE 2
# Annual area — all disturbance types, EXTREME
# ==============================================================================

p_annual_extreme <- annual_dat %>%
  filter(mode == "Extreme") %>%
  ggplot(
    aes(
      x = year,
      y = area_km2
    )
  ) +
  geom_col(
    width = 0.8
  ) +
  facet_wrap(
    ~ disturbance,
    scales = "free_y",
    ncol = 3
  ) +
  scale_x_continuous(
    breaks = seq(
      2000,
      2020,
      by = 5
    )
  ) +
  scale_y_continuous(
    labels = label_number(
      big.mark = ","
    ),
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    title = "Annual extreme forest disturbance area",
    subtitle = "Extreme disturbance thresholds, western U.S., 2000–2020",
    x = NULL,
    y = expression("Area (km"^2*")")
  ) +
  theme_disturbance


p_annual_extreme


ggsave(
  filename = file.path(
    fig_dir,
    "annual_disturbance_area_extreme.png"
  ),
  plot = p_annual_extreme,
  width = 12,
  height = 11,
  dpi = 300
)


# ==============================================================================
# 10. FIGURE 3
# ANY and EXTREME annual areas together
#
# This is useful for comparing the temporal patterns under the two thresholds.
# ==============================================================================

p_annual_both <- annual_dat %>%
  ggplot(
    aes(
      x = year,
      y = area_km2,
      fill = mode
    )
  ) +
  geom_col(
    position = position_dodge(
      width = 0.8
    ),
    width = 0.75
  ) +
  facet_wrap(
    ~ disturbance,
    scales = "free_y",
    ncol = 3
  ) +
  scale_x_continuous(
    breaks = seq(
      2000,
      2020,
      by = 5
    )
  ) +
  scale_y_continuous(
    labels = label_number(
      big.mark = ","
    ),
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    title = "Annual forest disturbance area",
    subtitle = "Any and extreme disturbance thresholds, western U.S., 2000–2020",
    x = NULL,
    y = expression("Area (km"^2*")"),
    fill = "Threshold"
  ) +
  theme_disturbance


p_annual_both


ggsave(
  filename = file.path(
    fig_dir,
    "annual_disturbance_area_any_extreme.png"
  ),
  plot = p_annual_both,
  width = 12,
  height = 11,
  dpi = 300
)


# ==============================================================================
# 11. FIGURE 4
# Total area-through-time, 2000–2020
#
# IMPORTANT:
# These totals are sums of annual disturbed area.
# They are NOT unique cumulative spatial footprints.
# ==============================================================================

p_total <- total_dat %>%
  ggplot(
    aes(
      x = disturbance,
      y = area_km2,
      fill = mode
    )
  ) +
  geom_col(
    position = position_dodge(
      width = 0.8
    ),
    width = 0.7
  ) +
  scale_y_continuous(
    labels = label_number(
      big.mark = ","
    ),
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    title = "Total forest disturbance area, 2000–2020",
    subtitle = "Sum of annual disturbed area",
    x = NULL,
    y = expression("Area-through-time (km"^2*")"),
    fill = "Threshold"
  ) +
  theme_disturbance +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


p_total


ggsave(
  filename = file.path(
    fig_dir,
    "total_disturbance_area_any_extreme.png"
  ),
  plot = p_total,
  width = 11,
  height = 7,
  dpi = 300
)


# ==============================================================================
# 12. OPTIONAL FIGURE
# Focus specifically on disturbance combinations
#
# This may be more useful than putting singles and combinations into the same
# figure because their areas differ substantially.
# ==============================================================================

combination_names <- c(
  "Wildfire + Biotic",
  "Wildfire + Hotter drought",
  "Wildfire + PDSI drought",
  "Biotic + Hotter drought",
  "Biotic + PDSI drought",
  "Wildfire + Biotic + Hotter drought",
  "Wildfire + Biotic + PDSI drought"
)


p_combinations <- annual_dat %>%
  filter(
    disturbance %in% combination_names
  ) %>%
  ggplot(
    aes(
      x = year,
      y = area_km2,
      fill = mode
    )
  ) +
  geom_col(
    position = position_dodge(
      width = 0.8
    ),
    width = 0.75
  ) +
  facet_wrap(
    ~ disturbance,
    scales = "free_y",
    ncol = 2
  ) +
  scale_x_continuous(
    breaks = seq(
      2000,
      2020,
      by = 5
    )
  ) +
  scale_y_continuous(
    labels = label_number(
      big.mark = ","
    ),
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    title = "Annual compound disturbance area",
    subtitle = "Same-year disturbance combinations, western U.S., 2000–2020",
    x = NULL,
    y = expression("Area (km"^2*")"),
    fill = "Threshold"
  ) +
  theme_disturbance


p_combinations


ggsave(
  filename = file.path(
    fig_dir,
    "annual_compound_disturbance_area.png"
  ),
  plot = p_combinations,
  width = 10,
  height = 11,
  dpi = 300
)
