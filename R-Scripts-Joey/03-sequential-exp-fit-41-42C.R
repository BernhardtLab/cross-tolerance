library(tidyverse)
library(cowplot)
library(minpack.lm)   # for nlsLM
library(broom)        # for tidy model output
theme_set(theme_cowplot())

edata <- read_csv("data-processed/all-blocks-tpc-experiment.csv") |> 
  filter(evolution_history %in% c("35 evolved", "40 evolved", "fRS585")) |>
  mutate(rep.id = paste(well, block, test_temperature, strain, evolution_history, sep = "_")) |> 
  rename(RFU = od,
         time_days = days)

# Filter to 41°C and 42°C wells only
edata_high_temp <- edata |>
  filter(test_temperature %in% c(41, 42))

# ══════════════════════════════════════════════════════════════════════════════
# SEQUENTIAL EXPONENTIAL FITTING FOR 41°C AND 42°C
#
# Strategy:
#   1. Start with n_min points (17 + 3 = 20)
#   2. Fit exponential model, record growth rate r
#   3. Add one more point, refit, record r
#   4. Continue until r declines by more than threshold (e.g., 10%)
#   5. Return the last "good" growth rate estimate and the data window used
#
# Rationale: pure exponential assumes RFU ~ N0 * exp(r*t). During true
# exponential phase, r is stable. Once you hit stationary phase (or slowdown
# due to resource limitation), r drops. The threshold controls how much 
# decline is acceptable.
# ══════════════════════════════════════════════════════════════════════════════

# Helper: fit pure exponential to a subset of data
fit_exponential_to_window <- function(df) {
  if (nrow(df) < 2) {
    return(list(converged = FALSE, r = NA, r_se = NA, N0 = NA, n_points = nrow(df)))
  }
  
  start <- list(
    N0 = min(df$RFU[df$time_days == min(df$time_days)], na.rm = TRUE),
    r  = 1.0
  )
  
  tryCatch(
    {
      fit <- nlsLM(
        RFU ~ N0 * exp(r * time_days),
        data  = df,
        start = start,
        lower = c(N0 = 0, r = 0),
        control = nls.lm.control(maxiter = 200)
      )
      # Extract parameters using tidy
      params <- tidy(fit)
      r_est <- params$estimate[params$term == "r"]
      r_se_est <- params$std.error[params$term == "r"]
      N0_est <- params$estimate[params$term == "N0"]
      
      list(
        converged = TRUE,
        r = r_est,
        r_se = r_se_est,
        N0 = N0_est,
        n_points = nrow(df),
        AIC = AIC(fit),
        fit = fit
      )
    },
    error = function(e) {
      list(converged = FALSE, r = NA, r_se = NA, N0 = NA, n_points = nrow(df))
    }
  )
}

# Main function: sequential fitting for one well
# Strategy: fit successively longer windows, tracking the growth rate r.
# The rate will initially climb, then peak, then decline as you move past
# the exponential phase. Stop when r has declined by decline_threshold
# from its peak value.
fit_sequential_exponential <- function(df, n_min = 20, decline_threshold = 0.05) {
  # Sort by time
  df <- df |> arrange(time_days)
  
  n_total <- nrow(df)
  if (n_total < n_min) {
    return(tibble(
      rep.id = unique(df$rep.id),
      n_min = n_min,
      status = "insufficient_data",
      n_points_selected = n_total,
      r_final = NA,
      r_se_final = NA,
      converged = FALSE,
      time_max_used = NA,
      note = paste("Only", n_total, "points available; need at least", n_min)
    ))
  }
  
  # Track fits as we go
  r_max <- -Inf      # Best (highest) r value seen so far
  r_max_at_n <- NA   # The n where r peaked
  last_good_fit <- NULL
  last_good_n <- n_min
  
  # Loop: fit with incrementally longer windows
  for (n in n_min:n_total) {
    df_window <- df |> slice(1:n)
    fit_result <- fit_exponential_to_window(df_window)
    
    if (!fit_result$converged) {
      # If a fit fails, stop here
      break
    }
    
    r_current <- fit_result$r
    
    # Track the peak
    if (r_current > r_max) {
      r_max <- r_current
      r_max_at_n <- n
    }
    
    # Once we've found a peak, check if we've declined enough
    if (!is.na(r_max_at_n) && n > r_max_at_n) {
      percent_decline <- (r_max - r_current) / r_max
      
      if (percent_decline > decline_threshold) {
        # We've declined enough; use the previous window
        break
      }
    }
    
    # This is a good fit; keep track of it
    last_good_fit <- fit_result
    last_good_n <- n
  }
  
  # Summary result
  if (is.null(last_good_fit)) {
    return(tibble(
      rep.id = unique(df$rep.id),
      n_min = n_min,
      status = "no_convergence",
      n_points_selected = NA,
      r_final = NA,
      r_se_final = NA,
      converged = FALSE,
      time_max_used = NA,
      note = "No successful fits in the range tried"
    ))
  }
  
  df_final <- df |> slice(1:last_good_n)
  
  return(tibble(
    rep.id = unique(df$rep.id),
    n_min = n_min,
    status = "success",
    n_points_selected = last_good_n,
    r_final = last_good_fit$r,
    r_se_final = last_good_fit$r_se,
    converged = TRUE,
    time_max_used = max(df_final$time_days),
    note = paste("Selected first", last_good_n, "of", n_total, "available points")
  ))
}

# ══════════════════════════════════════════════════════════════════════════════
# OPTION 2: STRICTER THRESHOLD ON EARLY DATA (n_min >= 15, threshold 1-2%)
# ══════════════════════════════════════════════════════════════════════════════

results_sequential_strict <- edata_high_temp |>
  group_by(rep.id) |>
  group_modify(~ fit_sequential_exponential(.x, n_min = 15, decline_threshold = 0.015)) |>
  ungroup()

print("Sequential exponential fits (STRICT: n_min=15, threshold=1.5%):")
print(results_sequential_strict)

# ══════════════════════════════════════════════════════════════════════════════
# OPTION 3: LOG-SCALE REGRESSION WITH RESIDUAL-BASED STOPPING
# ══════════════════════════════════════════════════════════════════════════════
# 
# Strategy: Fit linear model to log(RFU) ~ time_days, which should be perfectly
# linear during exponential phase. Stop adding points when residuals show 
# systematic upward bias (indicating curvature = leaving exponential phase).
#
# We'll use the absolute value of residuals as a stopping criterion:
# stop when mean(|recent residuals|) > threshold * mean(|early residuals|)

# Helper: fit linear model to log(RFU) vs time, return residuals too
fit_exponential_logscale <- function(df) {
  if (nrow(df) < 2) {
    return(list(converged = FALSE, r = NA, r_se = NA, N0 = NA, 
                n_points = nrow(df), residuals = NA_real_, mean_abs_resid = NA_real_))
  }
  
  df_clean <- df |> filter(RFU > 0)
  
  if (nrow(df_clean) < 2) {
    return(list(converged = FALSE, r = NA, r_se = NA, N0 = NA, 
                n_points = nrow(df), residuals = NA_real_, mean_abs_resid = NA_real_))
  }
  
  tryCatch(
    {
      fit <- lm(log(RFU) ~ time_days, data = df_clean)
      
      params <- tidy(fit)
      r_est <- params$estimate[params$term == "time_days"]
      r_se_est <- params$std.error[params$term == "time_days"]
      N0_est <- exp(params$estimate[params$term == "(Intercept)"])
      
      # Get residuals
      residuals_vec <- residuals(fit)
      mean_abs_resid <- mean(abs(residuals_vec))
      
      list(
        converged = TRUE,
        r = r_est,
        r_se = r_se_est,
        N0 = N0_est,
        n_points = nrow(df_clean),
        residuals = residuals_vec,
        mean_abs_resid = mean_abs_resid
      )
    },
    error = function(e) {
      list(converged = FALSE, r = NA, r_se = NA, N0 = NA, 
           n_points = nrow(df_clean), residuals = NA_real_, mean_abs_resid = NA_real_)
    }
  )
}

# Sequential fitting on log scale with residual-based stopping
fit_sequential_exponential_logscale <- function(df, n_min = 15, resid_threshold = 1.5) {
  df <- df |> arrange(time_days)
  
  n_total <- nrow(df)
  if (n_total < n_min) {
    return(tibble(
      rep.id = unique(df$rep.id),
      n_min = n_min,
      status = "insufficient_data",
      n_points_selected = n_total,
      r_final = NA,
      r_se_final = NA,
      converged = FALSE,
      time_max_used = NA,
      note = paste("Only", n_total, "points available; need at least", n_min)
    ))
  }
  
  early_mean_abs_resid <- NA_real_
  last_good_fit <- NULL
  last_good_n <- n_min
  
  for (n in n_min:n_total) {
    df_window <- df |> slice(1:n)
    fit_result <- fit_exponential_logscale(df_window)
    
    if (!fit_result$converged) {
      break
    }
    
    # On first iteration, establish baseline residual level
    if (is.na(early_mean_abs_resid)) {
      early_mean_abs_resid <- fit_result$mean_abs_resid
    }
    
    # Check if residuals have grown beyond threshold
    # (indicates we've left the exponential phase and entered non-exponential behavior)
    if (n > n_min) {
      resid_ratio <- fit_result$mean_abs_resid / early_mean_abs_resid
      
      if (resid_ratio > resid_threshold) {
        # Residuals have blown up; we've gone too far
        break
      }
    }
    
    last_good_fit <- fit_result
    last_good_n <- n
  }
  
  if (is.null(last_good_fit)) {
    return(tibble(
      rep.id = unique(df$rep.id),
      n_min = n_min,
      status = "no_convergence",
      n_points_selected = NA,
      r_final = NA,
      r_se_final = NA,
      converged = FALSE,
      time_max_used = NA,
      note = "No successful fits in the range tried"
    ))
  }
  
  df_final <- df |> slice(1:last_good_n)
  
  return(tibble(
    rep.id = unique(df$rep.id),
    n_min = n_min,
    status = "success",
    n_points_selected = last_good_n,
    r_final = last_good_fit$r,
    r_se_final = last_good_fit$r_se,
    converged = TRUE,
    time_max_used = max(df_final$time_days),
    note = paste("Selected first", last_good_n, "of", n_total, "available points")
  ))
}

# Apply log-scale fitting with residual-based stopping
# resid_threshold: stop when mean|residuals| exceeds this multiple of early residuals
# A value of 1.5 means stop when residuals grow by 50% above baseline
results_sequential_logscale <- edata_high_temp |>
  group_by(rep.id) |>
  group_modify(~ fit_sequential_exponential_logscale(.x, n_min = 15, resid_threshold = 1.2)) |>
  ungroup()

print("Sequential exponential fits on LOG SCALE (n_min=15, residual threshold=1.5x):")
print(results_sequential_logscale)

# Save results
write_csv(results_sequential_strict, 
          "data-processed/growth-rates-sequential-strict-41-42C.csv")
write_csv(results_sequential_logscale, 
          "data-processed/growth-rates-sequential-logscale-41-42C.csv")

# ══════════════════════════════════════════════════════════════════════════════
# VISUALIZATION: Show how growth rate changes with window size
# ══════════════════════════════════════════════════════════════════════════════

# Refit with detailed tracking for visualization
fit_sequential_exponential_detailed <- function(df, n_min = 20, decline_threshold = 0.05) {
  df <- df |> arrange(time_days)
  n_total <- nrow(df)
  
  if (n_total < n_min) {
    return(tibble(
      n = NA,
      r = NA,
      converged = NA,
      n_points = NA,
      time_max = NA
    ))
  }
  
  results_detail <- tibble(
    n = integer(),
    r = numeric(),
    converged = logical(),
    n_points = integer(),
    time_max = numeric()
  )
  
  r_max <- -Inf
  r_max_at_n <- NA
  
  for (n in n_min:n_total) {
    df_window <- df |> slice(1:n)
    fit_result <- fit_exponential_to_window(df_window)
    
    results_detail <- results_detail |>
      bind_rows(tibble(
        n = n,
        r = fit_result$r,
        converged = fit_result$converged,
        n_points = fit_result$n_points,
        time_max = max(df_window$time_days)
      ))
    
    if (!fit_result$converged) {
      break
    }
    
    r_current <- fit_result$r
    
    if (r_current > r_max) {
      r_max <- r_current
      r_max_at_n <- n
    }
    
    if (!is.na(r_max_at_n) && n > r_max_at_n) {
      percent_decline <- (r_max - r_current) / r_max
      if (percent_decline > decline_threshold) {
        break
      }
    }
  }
  
  return(results_detail)
}

# Generate detailed results for plotting
results_detailed <- edata_high_temp |>
  group_by(rep.id) |>
  group_modify(~ fit_sequential_exponential_detailed(.x, n_min = 20, decline_threshold = 0.05), 
               .keep = TRUE) |>
  ungroup()

# # Plot: growth rate vs. number of points used
# dir.create("figures/sequential-exp", showWarnings = FALSE, recursive = TRUE)
# 
# results_detailed |>
#   filter(converged) |>
#   ggplot(aes(x = n, y = r, color = rep.id)) +
#   geom_point(size = 2, alpha = 0.6) +
#   geom_line(alpha = 0.5) +
#   facet_wrap(~ rep.id) +
#   labs(
#     title = "Sequential exponential fitting for 41°C and 42°C wells",
#     x = "Number of time points included",
#     y = "Estimated growth rate (r, per day)",
#     color = "Well"
#   ) +
#   theme(legend.position = "bottom")

# ggsave("figures/sequential-exp/growth-rate-trajectory.png", width = 12, height = 8)

# Summary tables: which wells hit the decline threshold?
cat("\n=== STRICT APPROACH (n_min=15, 1.5% threshold) ===\n")
results_sequential_strict |>
  select(rep.id, n_points_selected, r_final, time_max_used, note) |>
  arrange(desc(n_points_selected)) |>
  print()

cat("\n=== LOG-SCALE APPROACH (n_min=15, 1.5% threshold) ===\n")
results_sequential_logscale |>
  select(rep.id, n_points_selected, r_final, time_max_used, note) |>
  arrange(desc(n_points_selected)) |>
  print()

# ══════════════════════════════════════════════════════════════════════════════
# PLOTTING: Overlay fitted exponential on raw OD data for each well
# Three versions: strict (OD scale), and log-scale
# ══════════════════════════════════════════════════════════════════════════════

# Helper function to generate and save plots for a results tibble
plot_sequential_fits <- function(results_df, output_dir, method_label) {
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  unique_wells <- unique(edata_high_temp$rep.id)
  
  for (well_id in unique_wells) {
    
    well_data <- edata_high_temp |> filter(rep.id == well_id) |> arrange(time_days)
    well_result <- results_df |> filter(rep.id == well_id)
    
    if (nrow(well_result) == 0 || !well_result$converged) {
      next
    }
    
    r_final <- well_result$r_final
    n_points_selected <- well_result$n_points_selected
    time_max <- well_result$time_max_used
    
    well_data_used <- well_data |> slice(1:n_points_selected)
    
    time_range <- seq(min(well_data_used$time_days), max(well_data_used$time_days), 
                      length.out = 200)
    N0_final <- min(well_data_used$RFU[well_data_used$time_days == min(well_data_used$time_days)], 
                    na.rm = TRUE)
    
    pred_data <- tibble(
      time_days = time_range,
      RFU_pred = N0_final * exp(r_final * time_days)
    )
    
    p <- ggplot() +
      geom_point(data = well_data,
                 aes(x = time_days, y = RFU),
                 size = 2, alpha = 0.5, color = "grey50") +
      geom_line(data = well_data,
                aes(x = time_days, y = RFU),
                alpha = 0.3, color = "grey50", linewidth = 0.7) +
      geom_point(data = well_data_used,
                 aes(x = time_days, y = RFU),
                 size = 2.5, alpha = 0.8, color = "#d6604d") +
      geom_line(data = pred_data,
                aes(x = time_days, y = RFU_pred, color = "Exponential fit"),
                linewidth = 1.2) +
      geom_vline(xintercept = time_max, linetype = "dashed", 
                 color = "firebrick", alpha = 0.5, linewidth = 0.8) +
      scale_color_manual(values = c("Exponential fit" = "#2166ac")) +
      labs(
        title = paste(method_label, "fit:", well_id),
        subtitle = paste("r =", format(r_final, digits = 3), "per day |",
                         "Used", n_points_selected, "of", nrow(well_data), 
                         "time points"),
        x = "Time (days)",
        y = "RFU (OD)",
        color = NULL
      ) +
      theme(
        legend.position = "bottom",
        plot.subtitle = element_text(size = 10)
      )
    
    ggsave(
      glue::glue("{output_dir}/well-{well_id}.png"),
      plot = p,
      width = 9,
      height = 6,
      dpi = 300
    )
  }
}

# Generate plots for strict threshold approach
plot_sequential_fits(results_sequential_strict, 
                     "figures/sequential-exp-fits-strict",
                     "Strict exponential fit (n_min=15, 1.5% threshold)")

# Generate plots for log-scale approach
plot_sequential_fits(results_sequential_logscale, 
                     "figures/sequential-exp-fits-logscale",
                     "Log-scale exponential fit (n_min=15, 1.5% threshold)")
