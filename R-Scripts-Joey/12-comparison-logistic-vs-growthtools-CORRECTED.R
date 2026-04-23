### Comparison of logistic vs. growthtools growth rate estimates (CORRECTED)
###
### Author: Joanna Bernhardt (Revision: Assistant)
### Date: April 22, 2026
###
### PURPOSE:
###   Compare two approaches to estimating exponential growth rates:
###   1. Logistic growth model (red solid line) - valid across full trajectory
###   2. Growthtools method (blue dashed line) - ONLY valid during exponential phase
###
### CORRECTION (April 22, 2026):
###   The previous version incorrectly plotted growthtools lines across the entire
###   time course. Growthtools estimates exponential growth rates, which are only
###   valid prior to saturation. This corrected version:
###   - Limits growthtools predictions to the exponential phase only
###   - Automatically detects where exponential growth ends
###   - Makes it clear visually that growthtools is not a full-trajectory model
###
### INPUT:
###   - data-processed/all-blocks-tpc-experiment.csv (growth curves)
###   - data-processed/growth-rates-logistic.csv (logistic fits)
###   - data-processed/growth-rates-growth-tools.csv (growthtools fits)
###
### OUTPUT:
###   - figures/comparison_logistic_vs_growthtools_42C_CORRECTED.pdf
###   - figures/comparison_logistic_vs_growthtools_42C_CORRECTED.png
###
### METHODS:
###   Exponential phase detection: fit log-linear model to progressively larger
###   windows of data. The exponential phase ends when R² drops significantly,
###   indicating onset of saturation phase.


# Load packages ---------------------------------------------------------------

library(tidyverse)
library(cowplot)


# Load data -------------------------------------------------------------------

edata <- read_csv("data-processed/all-blocks-tpc-experiment.csv") |>
  filter(evolution_history %in% c("35 evolved", "40 evolved", "fRS585"))

gr_logistic <- read_csv("data-processed/growth-rates-logistic.csv")
gr_growthtools <- read_csv("data-processed/growth-rates-growth-tools.csv")

# Filter to 42°C for demonstration (where saturation is most apparent)
edata_42 <- edata |> filter(test_temperature == 42)
gr_logistic_42 <- gr_logistic |> filter(test_temperature == 42)
gr_growthtools_42 <- gr_growthtools |> filter(test_temperature == 42)


# Helper functions -----------------------------------------------------------

# Function to detect end of exponential phase
# Strategy: fit log-linear model to progressively larger windows
# Stop when R² drops significantly (indicating saturation)
find_exponential_phase <- function(days, od, min_points = 5) {
  ln_od <- log(od)
  n <- length(days)
  
  # Try fits of increasing length
  r2_values <- numeric(n - min_points + 1)
  
  for (i in min_points:n) {
    fit <- lm(ln_od[1:i] ~ days[1:i])
    r2_values[i - min_points + 1] <- summary(fit)$r.squared
  }
  
  # Find where R² drops significantly, indicating saturation starting
  # Threshold: R² drop > 0.05 or R² < 0.90
  r2_diffs <- -diff(r2_values)
  threshold_idx <- which(r2_diffs > 0.05 | r2_values[-length(r2_values)] < 0.90)[1]
  
  if (is.na(threshold_idx)) {
    # If no clear transition, use the last point where R² is high
    last_good <- max(which(r2_values > 0.90))
    exp_phase_end_idx <- min_points + last_good - 1
  } else {
    exp_phase_end_idx <- min_points + threshold_idx - 1
  }
  
  exp_phase_end_idx <- min(exp_phase_end_idx, n)
  return(exp_phase_end_idx)
}

# Logistic fit function (from script 10)
fit_logistic_tpc <- function(time_days, od) {
  tryCatch({
    K_start <- max(od, na.rm = TRUE)
    r_start <- 0.5
    t0_start <- min(time_days)
    
    nls(od ~ K / (1 + exp(-r * (time_days - t0))),
        start = list(K = K_start, r = r_start, t0 = t0_start),
        control = nls.control(maxiter = 500, warnOnly = TRUE))
  }, error = function(e) NULL)
}


# Create comparison plots for 35 evolved at 42°C ------

strain_demo <- "35 evolved"
strain_data <- edata_42 |> filter(evolution_history == strain_demo)
well_list <- unique(strain_data$well)[1:6]

well_plots <- lapply(seq_along(well_list), function(j) {
  well_id <- well_list[j]
  data_well <- strain_data |>
    filter(well == well_id) |>
    arrange(days) |>
    select(days, od, evolution_history, well)
  
  if (nrow(data_well) < 3) return(NULL)
  
  # Get growth rates from both methods
  mu_log <- gr_logistic_42 |>
    filter(well == well_id) |>
    pull(mu) |>
    first()
  
  mu_gt <- gr_growthtools_42 |>
    filter(well == well_id) |>
    pull(mu) |>
    first()
  
  if (is.na(mu_log) || is.na(mu_gt)) return(NULL)
  
  # Fit logistic model across entire trajectory
  fit_log <- fit_logistic_tpc(data_well$days, data_well$od)
  time_seq <- seq(min(data_well$days), max(data_well$days), length.out = 150)
  
  if (!is.null(fit_log)) {
    pred_log_vec <- predict(fit_log, newdata = data.frame(time_days = time_seq))
  } else {
    K_est <- max(data_well$od) * 1.2
    t_mid_est <- median(data_well$days)
    pred_log_vec <- K_est / (1 + exp(-mu_log * (time_seq - t_mid_est)))
  }
  
  # Exponential growth: ONLY predict during exponential phase
  od0 <- min(data_well$od[data_well$od > 0], na.rm = TRUE)
  
  # KEY CORRECTION: Find end of exponential phase
  exp_end_idx <- find_exponential_phase(data_well$days, data_well$od)
  exp_end_time <- data_well$days[exp_end_idx]
  
  # Growthtools predictions ONLY up to end of exponential phase
  time_seq_exp <- seq(min(data_well$days), exp_end_time, length.out = 60)
  pred_gt_vec <- od0 * exp(mu_gt * (time_seq_exp - min(data_well$days)))
  
  # Combine prediction data
  pred_data <- bind_rows(
    tibble(time_days = time_seq, method = "Logistic", od_pred = pred_log_vec),
    tibble(time_days = time_seq_exp, method = "Growthtools (exp phase only)", od_pred = pred_gt_vec)
  )
  
  # Create plot
  ggplot(data_well, aes(x = days, y = od)) +
    geom_point(size = 2.5, alpha = 0.8, color = "black") +
    geom_line(data = pred_data |> filter(method == "Logistic"),
              aes(x = time_days, y = od_pred, color = method, linetype = method),
              size = 1.2, alpha = 0.85, inherit.aes = FALSE) +
    geom_line(data = pred_data |> filter(method == "Growthtools (exp phase only)"),
              aes(x = time_days, y = od_pred, color = method, linetype = method),
              size = 1.2, alpha = 0.85, inherit.aes = FALSE) +
    scale_color_manual(
      values = c("Logistic" = "#E41A1C", "Growthtools (exp phase only)" = "#377EB8"),
      guide = "none"
    ) +
    scale_linetype_manual(
      values = c("Logistic" = "solid", "Growthtools (exp phase only)" = "dashed"),
      guide = "none"
    ) +
    coord_cartesian(ylim = c(0, 1.6)) +
    labs(
      title = sprintf("Rep %s", well_id),
      x = "Days",
      y = "OD600",
      subtitle = sprintf("Log: %.3f | GT: %.3f | Exp phase ends: %.2f d",
                        mu_log, mu_gt, exp_end_time)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      plot.subtitle = element_text(size = 8.5, color = "gray50"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.minor = element_blank()
    )
})

well_plots_clean <- Filter(Negate(is.null), well_plots)

if (length(well_plots_clean) > 0) {
  combined <- cowplot::plot_grid(plotlist = well_plots_clean, ncol = 3, align = "hv")
  
  final_corrected_fig <- combined +
    ggtitle(sprintf("%s at 42°C (CORRECTED)", strain_demo)) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5, vjust = 2))
  
  print(final_corrected_fig)
}


# Save figures ----------------------------------------------------------------

ggsave(
  filename = "figures/comparison_logistic_vs_growthtools_42C_CORRECTED.pdf",
  plot = final_corrected_fig,
  width = 14,
  height = 10,
  dpi = 300,
  units = "in"
)

ggsave(
  filename = "figures/comparison_logistic_vs_growthtools_42C_CORRECTED.png",
  plot = final_corrected_fig,
  width = 14,
  height = 10,
  dpi = 300,
  units = "in"
)

cat("\n✓ Corrected comparison figures saved:\n")
cat("  PDF: figures/comparison_logistic_vs_growthtools_42C_CORRECTED.pdf\n")
cat("  PNG: figures/comparison_logistic_vs_growthtools_42C_CORRECTED.png\n\n")

cat("KEY POINTS:\n")
cat("  1. Red solid line (Logistic) - valid across full trajectory\n")
cat("  2. Blue dashed line (Growthtools) - only extends through exponential phase\n")
cat("  3. Exponential phase end point detected automatically per replicate\n")
cat("  4. This visualization correctly shows that growthtools estimates are\n")
cat("     exponential growth rates, NOT saturation-aware growth models\n\n")
