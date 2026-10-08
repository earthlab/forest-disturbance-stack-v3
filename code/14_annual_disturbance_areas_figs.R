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
  mutate(
    mode = "Any"
  )


extreme_dat <- read_csv(
  file.path(
    metrics_dir,
    "annual_spatial_metrics_west_extreme.csv"
  ),
  show_col_types = FALSE
) %>%
  mutate(
    mode = "Extreme"
  )


dat <- bind_rows(
  any_dat,
  extreme_dat
)


# ==============================================================================
# 4. Keep only wildfire, biotic disturbance, and hotter drought
#
# PDSI and all combinations involving PDSI are excluded here.
# The underlying CSV files and raster stacks remain unchanged.
# ==============================================================================

keep_disturbances <- c(
  "wf",
  "bt",
  "hd",
  "wf_bt",
  "wf_hd",
  "bt_hd",
  "wf_bt_hd"
)


dat <- dat %>%
  filter(
    disturbance %in% keep_disturbances
  )


# ==============================================================================
# 5. Add readable disturbance labels and panel positions
#
# Layout:
#
# Row 1:
#   Wildfire | Biotic | Hotter drought
#
# Row 2:
#   Wildfire + Biotic | Wildfire + Hotter drought | Biotic + Hotter drought
#
# Row 3:
#   Wildfire + Biotic + Hotter drought
#
# ==============================================================================

disturbance_lookup <- tibble(
  disturbance = c(
    "wf",
    "bt",
    "hd",
    "wf_bt",
    "wf_hd",
    "bt_hd",
    "wf_bt_hd"
  ),
  
  disturbance_label = c(
    "Wildfire",
    "Biotic",
    "Drought",
    "Wildfire + Biotic",
    "Wildfire + Drought",
    "Biotic + Drought",
    "Wildfire + Biotic + Drought"
  ),
  
  disturbance_group = c(
    "Single disturbances",
    "Single disturbances",
    "Single disturbances",
    "Two-way combinations",
    "Two-way combinations",
    "Two-way combinations",
    "Three-way combination"
  ),
  
  panel_column = c(
    "1",
    "2",
    "3",
    "1",
    "2",
    "3",
    "1"
  )
)


dat <- dat %>%
  left_join(
    disturbance_lookup,
    by = "disturbance"
  ) %>%
  mutate(
    
    disturbance_group = factor(
      disturbance_group,
      levels = c(
        "Single disturbances",
        "Two-way combinations",
        "Three-way combination"
      )
    ),
    
    panel_column = factor(
      panel_column,
      levels = c(
        "1",
        "2",
        "3"
      )
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
# 6. Separate annual observations and totals
# ==============================================================================

annual_dat <- dat %>%
  filter(
    period != "Total"
  ) %>%
  mutate(
    year = as.integer(period)
  )


total_dat <- dat %>%
  filter(
    period == "Total"
  )


# ==============================================================================
# 7. Basic checks
# ==============================================================================

print(
  annual_dat %>%
    group_by(
      mode,
      disturbance_label
    ) %>%
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
      disturbance_label,
      area_km2
    )
)


# ==============================================================================
# 8. Plot theme
# ==============================================================================

theme_disturbance <- theme_bw(
  base_size = 11
) +
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
# 9. Helper function for annual figures
#
# facet_grid() places:
#   singles on row 1
#   doubles on row 2
#   triple on row 3
#
# The labeller uses disturbance_label so that each occupied panel gets the
# correct disturbance name.
# ==============================================================================

make_annual_plot <- function(
    plot_data,
    plot_title,
    plot_subtitle,
    show_legend = FALSE
) {
  
  p <- ggplot(
    plot_data,
    aes(
      x = year,
      y = area_km2
    )
  ) +
    
    geom_col(
      width = 0.8
    ) +
    
    facet_grid(
      rows = vars(
        disturbance_group
      ),
      cols = vars(
        panel_column
      ),
      scales = "free_y",
      switch = "y"
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
        mult = c(
          0,
          0.05
        )
      )
    ) +
    
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = NULL,
      y = expression(
        "Area (km"^2*")"
      )
    ) +
    
    theme_disturbance +
    
    theme(
      strip.placement = "outside"
    )
  
  return(p)
}


# ==============================================================================
# NOTE ABOUT PANEL TITLES
#
# Because facet_grid() is being used to force the exact 3 x 3 layout, the
# default column strips would otherwise say "1", "2", and "3".
#
# We therefore use a slightly different approach below: panel-specific titles
# are added directly as text inside each panel.
# ==============================================================================


# ==============================================================================
# 10. Create label dataset for panel titles
# ==============================================================================

panel_labels <- disturbance_lookup %>%
  mutate(
    disturbance_group = factor(
      disturbance_group,
      levels = c(
        "Single disturbances",
        "Two-way combinations",
        "Three-way combination"
      )
    ),
    
    panel_column = factor(
      panel_column,
      levels = c(
        "1",
        "2",
        "3"
      )
    )
  )


# ==============================================================================
# 11. FIGURE 1
# Annual area — ANY disturbance
# ==============================================================================

p_annual_any <- annual_dat %>%
  
  filter(
    mode == "Any"
  ) %>%
  
  ggplot(
    aes(
      x = year,
      y = area_km2
    )
  ) +
  
  geom_col(
    width = 0.8
  ) +
  
  geom_text(
    data = panel_labels,
    aes(
      x = -Inf,
      y = Inf,
      label = disturbance_label
    ),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = 1.3,
    fontface = "bold",
    size = 3.3
  ) +
  
  facet_grid(
    rows = vars(
      disturbance_group
    ),
    cols = vars(
      panel_column
    ),
    scales = "free_y",
    switch = "y"
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
      mult = c(
        0,
        0.12
      )
    )
  ) +
  
  labs(
    title = "Annual forest disturbance area",
    subtitle = "Any disturbance thresholds, western U.S., 2000–2020",
    x = NULL,
    y = expression(
      "Area (km"^2*")"
    )
  ) +
  
  theme_disturbance +
  
  theme(
    strip.placement = "outside",
    
    # Hide the meaningless 1/2/3 column strips
    strip.text.x = element_blank()
  )


p_annual_any


ggsave(
  filename = file.path(
    fig_dir,
    "annual_disturbance_area_any.png"
  ),
  plot = p_annual_any,
  width = 12,
  height = 9,
  dpi = 300
)


# ==============================================================================
# 12. FIGURE 2
# Annual area — EXTREME disturbance
# ==============================================================================

p_annual_extreme <- annual_dat %>%
  
  filter(
    mode == "Extreme"
  ) %>%
  
  ggplot(
    aes(
      x = year,
      y = area_km2
    )
  ) +
  
  geom_col(
    width = 0.8
  ) +
  
  geom_text(
    data = panel_labels,
    aes(
      x = -Inf,
      y = Inf,
      label = disturbance_label
    ),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = 1.3,
    fontface = "bold",
    size = 3.3
  ) +
  
  facet_grid(
    rows = vars(
      disturbance_group
    ),
    cols = vars(
      panel_column
    ),
    scales = "free_y",
    switch = "y"
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
      mult = c(
        0,
        0.12
      )
    )
  ) +
  
  labs(
    title = "Annual extreme forest disturbance area",
    subtitle = "Extreme disturbance thresholds, western U.S., 2000–2020",
    x = NULL,
    y = expression(
      "Area (km"^2*")"
    )
  ) +
  
  theme_disturbance +
  
  theme(
    strip.placement = "outside",
    strip.text.x = element_blank()
  )


p_annual_extreme


ggsave(
  filename = file.path(
    fig_dir,
    "annual_disturbance_area_extreme.png"
  ),
  plot = p_annual_extreme,
  width = 12,
  height = 9,
  dpi = 300
)


# ==============================================================================
# 13. FIGURE 3
# ANY vs EXTREME annual area
#
# Side-by-side bars within each year.
# ==============================================================================

# Colors
threshold_colors <- c(
  "Any" = "#4D4D4D",
  "Extreme" = "#D95F02"
)



# Function for making one disturbance panel
make_disturbance_panel <- function(
    data,
    disturbance_name,
    show_x = FALSE,
    show_y_title = FALSE
) {
  
  p <- data %>%
    
    filter(
      disturbance_label == disturbance_name
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
    
    scale_fill_manual(
      values = threshold_colors,
      name = "Threshold"
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
        mult = c(0, 0.08)
      )
    ) +
    
    labs(
      title = disturbance_name,
      x = NULL,
      y = if (show_y_title) {
        expression("Area (km"^2*")")
      } else {
        NULL
      }
    ) +
    
    theme_bw(
      base_size = 10
    ) +
    
    theme(
      panel.grid.minor = element_blank(),
      
      plot.title = element_text(
        face = "bold",
        size = 10
      ),
      
      legend.position = "none",
      
      axis.title.x = element_blank()
    )
  
  
  if (!show_x) {
    
    p <- p +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }
  
  
  return(p)
}



# Create seven individual panels



# Row 1 — single disturbances
p_wf <- make_disturbance_panel(
  annual_dat,
  "Wildfire"
)

p_bt <- make_disturbance_panel(
  annual_dat,
  "Biotic"
)

p_hd <- make_disturbance_panel(
  annual_dat,
  "Drought"
)


# Row 2 — two-way combinations
p_wf_bt <- make_disturbance_panel(
  annual_dat,
  "Wildfire + Biotic",
  show_y_title = TRUE
)

p_wf_hd <- make_disturbance_panel(
  annual_dat,
  "Wildfire + Drought",
  show_x = TRUE
)


p_bt_hd <- make_disturbance_panel(
  annual_dat,
  "Biotic + Drought",
  show_x = TRUE
)


# Row 3 — three-way combination
p_wf_bt_hd <- make_disturbance_panel(
  annual_dat,
  "Wildfire + Biotic + Drought",
  show_x = TRUE
)



# Allow the legend to be collected from the individual plots
p_wf <- p_wf +
  theme(
    legend.position = "bottom"
  )

p_bt <- p_bt +
  theme(
    legend.position = "bottom"
  )

p_hd <- p_hd +
  theme(
    legend.position = "bottom"
  )

p_wf_bt <- p_wf_bt +
  theme(
    legend.position = "bottom"
  )

p_wf_hd <- p_wf_hd +
  theme(
    legend.position = "bottom"
  )

p_bt_hd <- p_bt_hd +
  theme(
    legend.position = "bottom"
  )

p_wf_bt_hd <- p_wf_bt_hd +
  theme(
    legend.position = "bottom"
  )



# Create layout
#
# "A" through "G" = disturbance plots
# "#" = blank space
# "L" = collected legend


design <- "
ABC
DEF
G#L
"



# Assemble figure
p_annual_both <-
  
  p_wf +
  p_bt +
  p_hd +
  p_wf_bt +
  p_wf_hd +
  p_bt_hd +
  p_wf_bt_hd +
  
  plot_layout(
    design = design,
    guides = "collect"
  ) +
  
  plot_annotation(
    title = "Annual disturbance area across western US forests (2000-2020)",
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 12
      )
    )
  ) &
  
  theme(
    legend.position = "bottom"
  )


# View figure
p_annual_both



# Save figure
ggsave(
  filename = file.path(
    fig_dir,
    "annual_disturbance_area_any_extreme.png"
  ),
  plot = p_annual_both,
  width = 6.5,
  height = 5,
  dpi = 300
)














# ==============================================================================
# 14. FIGURE 4
# Total area-through-time, 2000–2020
#
# IMPORTANT:
# These are sums of annual disturbed area.
#
# A pixel disturbed in multiple years contributes once for each year.
#
# These values therefore differ conceptually from the cumulative spatial
# presence rasters, which represent unique spatial footprint.
# ==============================================================================

total_dat <- total_dat %>%
  mutate(
    
    disturbance_label = factor(
      disturbance_label,
      levels = disturbance_lookup$disturbance_label
    )
  )


p_total <- total_dat %>%
  
  ggplot(
    aes(
      x = disturbance_label,
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
      mult = c(
        0,
        0.05
      )
    )
  ) +
  
  labs(
    title = "Total forest disturbance area, 2000–2020",
    subtitle = "Sum of annual disturbed area",
    x = NULL,
    y = expression(
      "Area-through-time (km"^2*")"
    ),
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