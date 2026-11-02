################################################################################
### Forest-disturbance-stack-v3

### Annual disturbance figures

### Matt Bitters
### matthew.bitters@colorado.edu


################################################################################
### Forest-disturbance-stack-v3
### Annual disturbance area: Any vs. Extreme
###
### Western U.S. forests, 2000–2020
###
### Includes:
###   - Annual disturbance area (Any and Extreme)
###   - Period boundary at 2010/2011
###   - Mean annual area for each period
###   - Absolute change (km2/year)
###   - Percent change
###
### Input:
###   data/derived/spatial_metrics/annual_spatial_metrics_west_any.csv
###   data/derived/spatial_metrics/annual_spatial_metrics_west_extreme.csv
################################################################################


# ==============================================================================
# 1. Packages and working directory
# ==============================================================================

setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

packages <- c(
  "here",
  "tidyverse",
  "scales",
  "patchwork",
  "cowplot"
)

installed <- packages %in% installed.packages()[, "Package"]

if (any(!installed)) {
  install.packages(packages[!installed])
}

library(here)
library(tidyverse)
library(scales)
library(patchwork)
library(cowplot)


# ==============================================================================
# 2. Paths
# ==============================================================================

metrics_dir <- here(
  "data", "derived", "spatial_metrics"
)

fig_dir <- here(
  "figures", "annual_disturbance_area"
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


# ==============================================================================
# 4. Filter disturbance categories and annual observations
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

disturbance_lookup <- tibble(
  disturbance = keep_disturbances,
  disturbance_label = c(
    "Wildfire",
    "Biotic",
    "Drought",
    "Wildfire + Biotic",
    "Wildfire + Drought",
    "Biotic + Drought",
    "Wildfire + Biotic + Drought"
  )
)


annual_dat <- bind_rows(
  any_dat,
  extreme_dat
) %>%
  filter(
    disturbance %in% keep_disturbances,
    period != "Total"
  ) %>%
  mutate(
    year = as.integer(period),
    mode = factor(
      mode,
      levels = c("Any", "Extreme")
    )
  ) %>%
  left_join(
    disturbance_lookup,
    by = "disturbance"
  ) %>%
  arrange(
    disturbance,
    mode,
    year
  )


# ==============================================================================
# 5. Check data
# ==============================================================================

print(
  annual_dat %>%
    group_by(mode, disturbance_label) %>%
    summarise(
      n_years = n(),
      min_year = min(year),
      max_year = max(year),
      .groups = "drop"
    ),
  n = Inf
)


# ==============================================================================
# 6. Colors
# ==============================================================================

threshold_colors <- c(
  "Any" = "#4D4D4D",
  "Extreme" = "#D95F02"
)


# ==============================================================================
# 7. Calculate period means and changes
#
# Period 1: 2000–2010 (11 years)
# Period 2: 2011–2020 (10 years)
#
# All area values represent mean annual disturbed area (km2/year).
# ==============================================================================

period_stats <- annual_dat %>%
  mutate(
    comparison_period = if_else(
      year <= 2010,
      "early",
      "late"
    )
  ) %>%
  group_by(
    disturbance_label,
    mode,
    comparison_period
  ) %>%
  summarise(
    mean_area = mean(area_km2),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = comparison_period,
    values_from = mean_area
  ) %>%
  mutate(
    change_km2 = late - early,
    
    percent_change = if_else(
      early > 0,
      100 * change_km2 / early,
      NA_real_
    ),
    
    # Keep all percentage changes, including large increases
    percent_label = if_else(
      is.na(percent_change),
      "NA",
      paste0(
        if_else(percent_change >= 0, "+", ""),
        comma(round(percent_change, 0)),
        "%"
      )
    ),
    
    # Preserve decimal precision for small area values
    early_label = comma(
      round(
        early,
        if_else(early < 10, 2, 0)
      )
    ),
    
    late_label = comma(
      round(
        late,
        if_else(late < 10, 2, 0)
      )
    ),
    
    change_label = paste0(
      if_else(change_km2 >= 0, "+", ""),
      comma(
        round(
          change_km2,
          if_else(abs(change_km2) < 10, 2, 0)
        )
      )
    ),
    
    summary_label = paste0(
      early_label,
      " → ",
      late_label,
      " (",
      change_label,
      "; ",
      percent_label,
      ")"
    )
  )


# Print summary values
print(
  period_stats %>%
    select(
      disturbance_label,
      mode,
      early,
      late,
      change_km2,
      percent_change
    ),
  n = Inf
)


# ==============================================================================
# 8. Function to create each disturbance panel
#
# Each panel contains:
#   - Annual bar graph
#   - Dashed line at 2010.5
#   - Any and Extreme summary annotations below graph
# ==============================================================================

make_disturbance_panel <- function(
    data,
    disturbance_name,
    show_x = FALSE,
    show_y_title = FALSE
) {
  
  plot_data <- data %>%
    filter(
      disturbance_label == disturbance_name
    )
  
  stats_data <- period_stats %>%
    filter(
      disturbance_label == disturbance_name
    ) %>%
    arrange(mode)
  
  
  # --------------------------------------------------------------------------
  # 8A. Annual bar graph
  # --------------------------------------------------------------------------
  
  p_bars <- ggplot(
    plot_data,
    aes(
      x = year,
      y = area_km2,
      fill = mode
    )
  ) +
    
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.75
    ) +
    
    geom_vline(
      xintercept = 2010.5,
      linetype = "dashed",
      linewidth = 0.5,
      color = "black"
    ) +
    
    scale_fill_manual(
      values = threshold_colors,
      name = "Threshold",
      drop = FALSE
    ) +
    
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 5)
    ) +
    
    scale_y_continuous(
      labels = label_comma(),
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
    
    theme_bw(base_size = 10) +
    
    theme(
      panel.grid.minor = element_blank(),
      
      plot.title = element_text(
        face = "bold",
        size = 10
      ),
      
      legend.position = "bottom",
      
      axis.title.x = element_blank()
    )
  
  
  if (!show_x) {
    p_bars <- p_bars +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }
  
  
  # --------------------------------------------------------------------------
  # 8B. Period comparison annotations
  #
  # Format:
  # Any: 4,509 → 7,265 (+2,757; +61%)
  # Extreme: 1,662 → 2,795 (+1,133; +68%)
  #
  # All areas in km2/year.
  # --------------------------------------------------------------------------
  
  annotation_data <- stats_data %>%
    mutate(
      y_position = if_else(
        mode == "Any",
        2,
        1
      ),
      
      label = paste0(
        as.character(mode),
        ": ",
        summary_label
      )
    )
  
  
  p_annotation <- ggplot(
    annotation_data,
    aes(
      x = 0,
      y = y_position,
      label = label,
      color = mode
    )
  ) +
    
    geom_text(
      hjust = 0,
      size = 2.5,
      show.legend = FALSE
    ) +
    
    scale_color_manual(
      values = threshold_colors
    ) +
    
    scale_x_continuous(
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    
    scale_y_continuous(
      limits = c(0.5, 2.5),
      expand = c(0, 0)
    ) +
    
    coord_cartesian(
      clip = "off"
    ) +
    
    theme_void() +
    
    theme(
      legend.position = "none",
      plot.margin = margin(0, 4, 0, 4)
    )
  
  
  # --------------------------------------------------------------------------
  # 8C. Combine bars and annotations
  # --------------------------------------------------------------------------
  
  p_combined <- p_bars / p_annotation +
    plot_layout(
      heights = c(4, 1)
    )
  
  return(p_combined)
}


# ==============================================================================
# 9. Generate seven panels
# ==============================================================================

# Row 1 — Single disturbances
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


# Row 2 — Two-way combinations
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


# Row 3 — Three-way combination
p_wf_bt_hd <- make_disturbance_panel(
  annual_dat,
  "Wildfire + Biotic + Drought",
  show_x = TRUE
)



# ==============================================================================
# 10. Assemble combined figure using patchwork
# ==============================================================================

# Layout:
# A B C
# D E F
# G . .

design <- "
ABC
DEF
G##
"

p_annual_both <- wrap_plots(
  A = p_wf,
  B = p_bt,
  C = p_hd,
  D = p_wf_bt,
  E = p_wf_hd,
  F = p_bt_hd,
  G = p_wf_bt_hd,
  design = design,
  guides = "collect"
) +
  
  plot_annotation(
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 12
      ),
      plot.subtitle = element_text(
        size = 10
      ),
      plot.caption = element_text(
        size = 8
      )
    )
  ) &
  
  theme(
    legend.position = "bottom"
  )


# ==============================================================================
# 11. Display figure
# ==============================================================================

p_annual_both


# ==============================================================================
# 12. Save figure
# ==============================================================================

ggsave(
  filename = file.path(
    fig_dir,
    "annual_disturbance_area_any_extreme_period_changes.png"
  ),
  plot = p_annual_both,
  width = 10,
  height = 7,
  dpi = 300
)
