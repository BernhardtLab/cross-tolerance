library(tidyverse)
library(minpack.lm)

# Load and prepare data -------------------------------------------------------

edata <- read_csv("data-processed/all-blocks-tpc-experiment.csv") |> 
  filter(evolution_history %in% c("35 evolved", "40 evolved", "fRS585")) |>
  mutate(
    rep.id = paste(well, block, test_temperature, strain, evolution_history, sep = "_")
  )

# Helper function: 3-parameter logistic growth model --------------------------
# Based on nglabratus_tpc_analysis.R

fit_growth_logistic <- function(time_days, OD, min_points = 6) {
  # 3-parameter logistic: K / (1 + exp(-r*(t - t_mid)))
  logistic3 <- function(t, K, r, t_mid) {
    K / (1 + exp(-r * (t - t_mid)))
  }
  
  n <- length(time_days)
  if (n < min_points) return(c(mu = NA_real_, r2 = NA_real_))
  
  OD_max  <- max(OD)
  OD_min  <- min(OD)
  t_range <- tail(time_days, 1) - time_days[1]
  
  # Numerical gradient for initial guesses
  grad   <- diff(OD) / diff(time_days)
  grad_t <- (head(time_days, -1) + tail(time_days, -1)) / 2
  K0     <- OD_max * 1.05
  t_mid0 <- grad_t[which.max(grad)]
  r0     <- max(4 * max(grad) / K0, 0.05)
  
  tryCatch({
    fit <- nlsLM(
      OD ~ logistic3(time_days, K, r, t_mid),
      start  = list(K = K0, r = r0, t_mid = t_mid0),
      lower  = c(K = OD_min, r = 0.005, t_mid = -t_range),
      upper  = c(K = OD_max * 3, r = 20.0, t_mid = tail(time_days, 1) + t_range),
      control = nls.lm.control(maxiter = 200, maxfev = 10000)
    )
    
    K_fit   <- coef(fit)[["K"]]
    r_fit   <- coef(fit)[["r"]]
    t_mid_fit <- coef(fit)[["t_mid"]]
    
    if (K_fit < OD_min || r_fit <= 0) stop("Implausible params")
    
    pred   <- predict(fit)
    ss_res <- sum((OD - pred)^2)
    ss_tot <- sum((OD - mean(OD))^2)
    r2     <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
    
    # mu (day⁻¹), R²
    c(mu = r_fit * K_fit / 4,
      r2 = r2)
  }, error = function(e) c(mu = NA_real_, r2 = NA_real_))
}

# Create output directory structure for figures 
# Organize by block and temperature for easier navigation

growth_results_list <- list()

# Group by block and temperature, then fit within each group
unique_combos <- edata |>
  distinct(block, test_temperature)

for (i in seq_len(nrow(unique_combos))) {
  block_i <- unique_combos$block[i]
  temp_i <- unique_combos$test_temperature[i]
  
  # Create subdirectory for this block/temperature combination
  grow_fig_path <- glue::glue("figures/logistic-figures/block{block_i}_{temp_i}C")
  dir.create(grow_fig_path, recursive = TRUE, showWarnings = FALSE)
  
  # Filter to this block/temp combination
  data_subset <- edata |>
    filter(block == block_i, test_temperature == temp_i)
  
  # Fit growth rates for this subset
  growth_results_list[[i]] <- data_subset |>
    group_by(rep.id) |>
    summarise(
      rep.id = first(rep.id),
      well = first(well),
      block = first(block),
      test_temperature = first(test_temperature),
      strain = first(strain),
      evolution_history = first(evolution_history),
      gr_fit = list(fit_growth_logistic(days, od)),
      .groups = "drop"
    ) |>
    unnest_wider(gr_fit)
  
  cat(sprintf("Completed block %d, %dC (%d/%d)\n", 
              block_i, temp_i, i, nrow(unique_combos)))
}

# Combine all results
growth_results <- bind_rows(growth_results_list)

# Summary output ---------------------------------------------------------------

growth_summary <- growth_results |>
  select(rep.id, well, block, test_temperature, strain, evolution_history, mu, r2)

cat("\n=== Growth Rate Summary ===\n")
cat(sprintf("Total wells fit: %d\n", nrow(growth_summary)))
cat(sprintf("Successful fits (mu not NA): %d\n", sum(!is.na(growth_summary$mu))))
print(
  growth_summary |>
    group_by(test_temperature) |>
    summarise(
      n = n(),
      n_success = sum(!is.na(mu)),
      mean_mu = round(mean(mu, na.rm = TRUE), 3),
      sd_mu = round(sd(mu, na.rm = TRUE), 3),
      mean_r2 = round(mean(r2, na.rm = TRUE), 3)
    )
)

write_csv(growth_summary, "data-processed/growth-rates-logistic.csv")
cat("\nSaved to: data-processed/growth-rates-logistic.csv\n")

# Check how many figures were saved
total_figs <- length(list.files("figures/logistic-figures", recursive = TRUE))
cat("Figure count note: Set up for future figure generation\n")
