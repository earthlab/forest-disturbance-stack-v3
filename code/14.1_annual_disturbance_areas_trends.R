
################################################################################
### Forest-disturbance-stack-v3
### Temporal trend analysis: annual disturbance area
###
### Analyses:
###   1. Lag-1 temporal autocorrelation
###   2. Mann-Kendall trend test
###   3. Theil-Sen slope estimation
###
### Study period: 2000–2020
################################################################################

# ==============================================================================
# 1. Packages
# ==============================================================================

setwd("/home/jovyan/data-store/forest-disturbance-stack-v3")
here::i_am("README.md")

packages <- c(
  "here", "tidyverse", "Kendall", "trend"
)

installed <- packages %in% installed.packages()[, "Package"]

if (any(!installed)) {
  install.packages(packages[!installed])
}

library(here)
library(tidyverse)
library(Kendall)
library(trend)


# ==============================================================================
# 2. Paths
# ==============================================================================

metrics_dir <- here(
  "data", "derived", "spatial_metrics"
)

output_dir <- here(
  "data", "derived", "temporal_trends"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 3. Read annual disturbance data
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


keep_disturbances <- c(
  "wf",
  "bt",
  "hd",
  "wf_bt",
  "wf_hd",
  "bt_hd",
  "wf_bt_hd"
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
    year = as.integer(period)
  ) %>%
  arrange(
    mode,
    disturbance,
    year
  )


# ==============================================================================
# 4. Validate annual time series
# ==============================================================================

series_check <- annual_dat %>%
  group_by(mode, disturbance) %>%
  summarise(
    n_years = n(),
    min_year = min(year),
    max_year = max(year),
    n_missing = sum(is.na(area_km2)),
    n_unique_years = n_distinct(year),
    .groups = "drop"
  )

print(series_check)

stopifnot(
  all(series_check$n_years == 21),
  all(series_check$n_unique_years == 21),
  all(series_check$min_year == 2000),
  all(series_check$max_year == 2020),
  all(series_check$n_missing == 0)
)


# ==============================================================================
# 5. Function: temporal trend analysis
# ==============================================================================

analyze_trend <- function(df) {
  
  df <- df %>%
    arrange(year)
  
  y <- df$area_km2
  x <- df$year
  
  # --------------------------------------------------------------------------
  # A. Lag-1 autocorrelation
  # --------------------------------------------------------------------------
  
  acf_result <- acf(
    y,
    lag.max = 1,
    plot = FALSE
  )
  
  lag1_acf <- as.numeric(
    acf_result$acf[2]
  )
  
  # Approximate white-noise significance threshold
  acf_threshold <- 1.96 / sqrt(length(y))
  
  lag1_flag <- abs(lag1_acf) > acf_threshold
  
  
  # --------------------------------------------------------------------------
  # B. Mann-Kendall trend test
  # --------------------------------------------------------------------------
  
  mk <- Kendall::MannKendall(y)
  
  mk_tau <- as.numeric(mk$tau)
  mk_p <- as.numeric(mk$sl)
  
  
  # --------------------------------------------------------------------------
  # C. Theil-Sen slope
  # --------------------------------------------------------------------------
  
  sen <- trend::sens.slope(
    y,
    conf.level = 0.95
  )
  
  sen_slope <- as.numeric(
    sen$estimates
  )
  
  sen_lower <- as.numeric(
    sen$conf.int[1]
  )
  
  sen_upper <- as.numeric(
    sen$conf.int[2]
  )
  
  
  # --------------------------------------------------------------------------
  # D. Additional descriptive statistics
  # --------------------------------------------------------------------------
  
  mean_area <- mean(y)
  
  first_decade_mean <- mean(
    y[x >= 2000 & x <= 2010]
  )
  
  second_decade_mean <- mean(
    y[x >= 2011 & x <= 2020]
  )
  
  percent_change <- 100 *
    (second_decade_mean - first_decade_mean) /
    first_decade_mean
  
  
  # --------------------------------------------------------------------------
  # E. Return results
  # --------------------------------------------------------------------------
  
  tibble(
    n_years = length(y),
    
    mean_area_km2 = mean_area,
    
    lag1_acf = lag1_acf,
    acf_threshold = acf_threshold,
    lag1_acf_flag = lag1_flag,
    
    mk_tau = mk_tau,
    mk_p = mk_p,
    
    sen_slope_km2_yr = sen_slope,
    sen_lower_95 = sen_lower,
    sen_upper_95 = sen_upper,
    
    mean_2000_2010 = first_decade_mean,
    mean_2011_2020 = second_decade_mean,
    percent_change_periods = percent_change
  )
}


# ==============================================================================
# 6. Run analyses for all 14 time series
# ==============================================================================

trend_results <- annual_dat %>%
  group_by(
    mode,
    disturbance
  ) %>%
  group_modify(
    ~ analyze_trend(.x)
  ) %>%
  ungroup() %>%
  mutate(
    mk_significant = mk_p < 0.05,
    
    trend_direction = case_when(
      mk_p >= 0.05 ~ "Not significant",
      sen_slope_km2_yr > 0 ~ "Increasing",
      sen_slope_km2_yr < 0 ~ "Decreasing",
      TRUE ~ "No change"
    )
  )

trend_results <- trend_results %>%
  mutate(
    mk_p_fdr = p.adjust(
      mk_p,
      method = "BH"
    ),
    mk_significant_fdr = mk_p_fdr < 0.05
  )


# ==============================================================================
# 7. Print results
# ==============================================================================

print(
  trend_results,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 8. Save results
# ==============================================================================

write_csv(
  trend_results,
  file.path(
    output_dir,
    "annual_disturbance_temporal_trends.csv"
  )
)


# ==============================================================================
# 9. Summary of significant trends
# ==============================================================================

significant_trends <- trend_results %>%
  filter(
    mk_significant
  ) %>%
  select(
    mode,
    disturbance,
    mk_tau,
    mk_p,
    sen_slope_km2_yr,
    sen_lower_95,
    sen_upper_95,
    lag1_acf,
    lag1_acf_flag
  )

print(
  significant_trends,
  n = Inf
)


# ==============================================================================
# 10. Identify series with possible autocorrelation
# ==============================================================================

autocorrelated_series <- trend_results %>%
  filter(
    lag1_acf_flag
  ) %>%
  select(
    mode,
    disturbance,
    lag1_acf,
    acf_threshold,
    mk_p
  )

print(
  autocorrelated_series,
  n = Inf
)



# ==============================================================================
# 11. Detrended temporal autocorrelation
# ==============================================================================

# Function to calculate lag-1 autocorrelation after removing
# the Theil-Sen trend.

analyze_detrended_acf <- function(df) {
  
  df <- df %>%
    arrange(year)
  
  x <- df$year
  y <- df$area_km2
  
  # Estimate Theil-Sen slope
  sen <- trend::sens.slope(y)
  
  slope <- as.numeric(sen$estimates)
  
  # Remove fitted trend
  detrended_y <- y - slope * (x - min(x))
  
  # Calculate lag-1 autocorrelation
  acf_result <- acf(
    detrended_y,
    lag.max = 1,
    plot = FALSE
  )
  
  lag1_detrended <- as.numeric(
    acf_result$acf[2]
  )
  
  # Approximate white-noise screening threshold
  threshold <- 1.96 / sqrt(length(y))
  
  tibble(
    lag1_acf_detrended = lag1_detrended,
    acf_threshold_detrended = threshold,
    detrended_acf_flag =
      abs(lag1_detrended) > threshold
  )
}


# Run for all disturbance time series
detrended_acf_results <- annual_dat %>%
  group_by(
    mode,
    disturbance
  ) %>%
  group_modify(
    ~ analyze_detrended_acf(.x)
  ) %>%
  ungroup()


# Join with existing trend results
trend_results <- trend_results %>%
  left_join(
    detrended_acf_results,
    by = c("mode", "disturbance")
  )


# Print comparison
print(
  trend_results %>%
    select(
      mode,
      disturbance,
      lag1_acf,
      lag1_acf_detrended,
      detrended_acf_flag
    ),
  n = Inf
)



# ==============================================================================
# 12. Permutation test: 2000–2010 vs. 2011–2020
# ==============================================================================

set.seed(123)

n_permutations <- 10000


permutation_period_test <- function(df, n_perm = 10000) {
  
  df <- df %>%
    arrange(year) %>%
    mutate(
      period_group = if_else(
        year <= 2010,
        "Early",
        "Late"
      )
    )
  
  y <- df$area_km2
  
  n_early <- sum(df$period_group == "Early")
  n_late <- sum(df$period_group == "Late")
  
  mean_early <- mean(
    y[df$period_group == "Early"]
  )
  
  mean_late <- mean(
    y[df$period_group == "Late"]
  )
  
  observed_difference <- mean_late - mean_early
  
  # Generate null distribution
  permuted_differences <- replicate(
    n_perm,
    {
      shuffled <- sample(y)
      
      mean(shuffled[(n_early + 1):length(y)]) -
        mean(shuffled[1:n_early])
    }
  )
  
  # Two-sided permutation p-value
  p_value <- (
    sum(
      abs(permuted_differences) >=
        abs(observed_difference)
    ) + 1
  ) / (n_perm + 1)
  
  percent_change <- if (mean_early == 0) {
    NA_real_
  } else {
    100 * observed_difference / mean_early
  }
  
  tibble(
    mean_early_km2 = mean_early,
    mean_late_km2 = mean_late,
    difference_km2 = observed_difference,
    percent_change = percent_change,
    permutation_p = p_value
  )
}


# Run permutation tests
permutation_results <- annual_dat %>%
  group_by(
    mode,
    disturbance
  ) %>%
  group_modify(
    ~ permutation_period_test(
      .x,
      n_perm = n_permutations
    )
  ) %>%
  ungroup() %>%
  mutate(
    permutation_p_fdr = p.adjust(
      permutation_p,
      method = "BH"
    )
  )


# Print results
print(
  permutation_results,
  n = Inf,
  width = Inf
)



# ==============================================================================
# 13. Pettitt change-point test
# ==============================================================================

analyze_pettitt <- function(df) {
  
  df <- df %>%
    arrange(year)
  
  y <- df$area_km2
  years <- df$year
  
  # Pettitt test
  result <- trend::pettitt.test(y)
  
  # Estimated change-point index
  change_index <- as.integer(
    result$estimate
  )
  
  # Convert index to calendar year
  change_year <- years[change_index]
  
  tibble(
    pettitt_change_index = change_index,
    pettitt_change_year = change_year,
    pettitt_statistic = as.numeric(
      result$statistic
    ),
    pettitt_p = as.numeric(
      result$p.value
    )
  )
}


# Run Pettitt tests
pettitt_results <- annual_dat %>%
  group_by(
    mode,
    disturbance
  ) %>%
  group_modify(
    ~ analyze_pettitt(.x)
  ) %>%
  ungroup() %>%
  mutate(
    pettitt_p_fdr = p.adjust(
      pettitt_p,
      method = "BH"
    )
  )


# Print results
print(
  pettitt_results,
  n = Inf,
  width = Inf
)



# ==============================================================================
# 14. Combine all temporal analysis results
# ==============================================================================

all_temporal_results <- trend_results %>%
  left_join(
    permutation_results,
    by = c("mode", "disturbance")
  ) %>%
  left_join(
    pettitt_results,
    by = c("mode", "disturbance")
  )


# Create a compact results table
summary_results <- all_temporal_results %>%
  select(
    mode,
    disturbance,
    
    lag1_acf,
    lag1_acf_detrended,
    detrended_acf_flag,
    
    mk_tau,
    mk_p,
    mk_p_fdr,
    
    sen_slope_km2_yr,
    sen_lower_95,
    sen_upper_95,
    
    mean_early_km2,
    mean_late_km2,
    difference_km2,
    percent_change,
    
    permutation_p,
    permutation_p_fdr,
    
    pettitt_change_year,
    pettitt_p,
    pettitt_p_fdr
  )


print(
  summary_results,
  n = Inf,
  width = Inf
)


# Save full results
write_csv(
  all_temporal_results,
  file.path(
    output_dir,
    "annual_disturbance_all_temporal_results.csv"
  )
)


# Save compact summary
write_csv(
  summary_results,
  file.path(
    output_dir,
    "annual_disturbance_temporal_summary.csv"
  )
)



# ==============================================================================
# 15. Figure: Mean annual disturbance area by period
#
# Compare 2000–2010 vs. 2011–2020
# Any and Extreme shown together
# Bootstrap 95% confidence intervals
# ==============================================================================

library(tidyverse)
library(scales)
library(patchwork)

# Output directory
period_fig_dir <- here(
  "figures",
  "annual_disturbance_area"
)

dir.create(
  period_fig_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Colors (same as annual disturbance bar figures)
threshold_colors <- c(
  "Any" = "#4D4D4D",
  "Extreme" = "#D95F02"
)

# Disturbance labels
disturbance_labels <- c(
  "wf" = "Wildfire",
  "bt" = "Biotic",
  "hd" = "Hotter drought",
  "wf_bt" = "Wildfire + Biotic",
  "wf_hd" = "Wildfire + Drought",
  "bt_hd" = "Biotic + Drought",
  "wf_bt_hd" = "Wildfire + Biotic + Drought"
)


# ==============================================================================
# 16. Bootstrap confidence intervals for period means
# ==============================================================================

set.seed(123)

n_boot <- 10000

bootstrap_mean_ci <- function(x, n_boot = 10000) {
  
  boot_means <- replicate(
    n_boot,
    mean(
      sample(
        x,
        size = length(x),
        replace = TRUE
      )
    )
  )
  
  tibble(
    mean_area = mean(x),
    ci_lower = quantile(
      boot_means,
      0.025,
      names = FALSE
    ),
    ci_upper = quantile(
      boot_means,
      0.975,
      names = FALSE
    )
  )
}


period_summary <- annual_dat %>%
  mutate(
    period_group = if_else(
      year <= 2010,
      "2000–2010",
      "2011–2020"
    )
  ) %>%
  group_by(
    mode,
    disturbance,
    period_group
  ) %>%
  group_modify(
    ~ bootstrap_mean_ci(
      .x$area_km2,
      n_boot = n_boot
    )
  ) %>%
  ungroup() %>%
  mutate(
    disturbance_label = factor(
      disturbance_labels[disturbance],
      levels = disturbance_labels
    ),
    mode = factor(
      mode,
      levels = c("Any", "Extreme")
    ),
    period_group = factor(
      period_group,
      levels = c(
        "2000–2010",
        "2011–2020"
      )
    )
  )


# ==============================================================================
# 17. Calculate absolute changes
# ==============================================================================

period_changes <- period_summary %>%
  select(
    mode,
    disturbance,
    period_group,
    mean_area
  ) %>%
  pivot_wider(
    names_from = period_group,
    values_from = mean_area
  ) %>%
  mutate(
    change_km2_yr = `2011–2020` - `2000–2010`,
    change_label = paste0(
      if_else(change_km2_yr >= 0, "+", ""),
      comma(round(change_km2_yr, 1)),
      " km²/yr"
    )
  )


# ==============================================================================
# 18. Build individual disturbance panels
# ==============================================================================

make_period_panel <- function(disturbance_code) {
  
  df <- period_summary %>%
    filter(
      disturbance == disturbance_code
    )
  
  changes <- period_changes %>%
    filter(
      disturbance == disturbance_code
    )
  
  # Put Any and Extreme on separate horizontal tracks
  df <- df %>%
    mutate(
      y_position = if_else(
        mode == "Any", 2, 1
      )
    )
  
  # Position changes between period means
  changes <- changes %>%
    mutate(
      y_position = if_else(
        mode == "Any", 2, 1
      ),
      label_x = pmax(
        `2000–2010`,
        `2011–2020`
      )
    )
  
  ggplot(
    df,
    aes(
      x = mean_area,
      y = y_position,
      color = mode
    )
  ) +
    
    # Connect early and late period means
    geom_line(
      aes(group = mode),
      linewidth = 0.8
    ) +
    
    # Bootstrap confidence intervals
    geom_errorbarh(
      aes(
        xmin = ci_lower,
        xmax = ci_upper
      ),
      height = 0.12,
      linewidth = 0.5
    ) +
    
    # Points for each period
    geom_point(
      aes(shape = period_group),
      size = 2.5
    ) +
    
    # Absolute change annotations
    geom_text(
      data = changes,
      aes(
        x = label_x,
        y = y_position,
        label = change_label,
        color = mode
      ),
      inherit.aes = FALSE,
      hjust = -0.1,
      vjust = -0.7,
      size = 2.7,
      show.legend = FALSE
    ) +
    
    scale_color_manual(
      values = threshold_colors,
      drop = FALSE
    ) +
    
    scale_shape_manual(
      values = c(
        "2000–2010" = 16,
        "2011–2020" = 17
      ),
      name = "Period"
    ) +
    
    scale_y_continuous(
      breaks = c(1, 2),
      labels = c("Extreme", "Any"),
      limits = c(0.5, 2.5)
    ) +
    
    scale_x_continuous(
      labels = label_comma(),
      expand = expansion(
        mult = c(0.05, 0.35)
      )
    ) +
    
    labs(
      title = disturbance_labels[[disturbance_code]],
      x = NULL,
      y = NULL
    ) +
    
    theme_bw(base_size = 10) +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 10
      ),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.text.y = element_text(
        size = 9
      )
    )
}


# ==============================================================================
# 19. Create and assemble seven panels
# ==============================================================================

p_wf <- make_period_panel("wf")
p_bt <- make_period_panel("bt")
p_hd <- make_period_panel("hd")

p_wf_bt <- make_period_panel("wf_bt")
p_wf_hd <- make_period_panel("wf_hd")
p_bt_hd <- make_period_panel("bt_hd")

p_triple <- make_period_panel("wf_bt_hd")


# Layout matching your original figures
design <- "
ABC
DEF
G##
"

p_period_comparison <-
  p_wf +
  p_bt +
  p_hd +
  p_wf_bt +
  p_wf_hd +
  p_bt_hd +
  p_triple +
  plot_layout(design = design) +
  plot_annotation(
    title = "Change in mean annual forest disturbance area",
    subtitle = "Western U.S. forests, 2000–2010 vs. 2011–2020",
    caption = paste(
      "Points represent period means;",
      "error bars represent bootstrap 95% confidence intervals.",
      "Annotations show changes in mean annual area."
    )
  )


# Display figure
p_period_comparison


# ==============================================================================
# 20. Save figure and summary data
# ==============================================================================

ggsave(
  filename = file.path(
    period_fig_dir,
    "disturbance_area_period_comparison.png"
  ),
  plot = p_period_comparison,
  width = 12,
  height = 8,
  dpi = 300
)

write_csv(
  period_summary,
  file.path(
    output_dir,
    "disturbance_period_means_bootstrap_ci.csv"
  )
)

write_csv(
  period_changes,
  file.path(
    output_dir,
    "disturbance_period_absolute_changes.csv"
  )
)
