# =============================================================================
# 16-growth-rates-gcplyr.R
#
# Author: Joey Bernhardt
# Created: June 2026
#
# Estimates growth metrics from OD curves using the gcplyr package
# (model-free approach), then fits Sharpe-Schoolfield High (SSH) thermal
# performance curves per strain using:
#   - mu_max  (max per-capita growth rate) as the primary metric
#   - auc     (empirical AUC, trapezoid rule) for comparison
#
# Input:  data-processed/all-blocks-tpc-experiment.csv
# Output: data-processed/gcplyr/
#         figures/
#
# Sections:
#   1. Setup
#   2. SSH model functions
#   3. Load data
#   4. Estimate growth metrics per well with gcplyr
#   5. Fit SSH TPC per strain — mu_max
#   6. Fit SSH TPC per strain — AUC
#   7. Compare mu_max vs AUC TPC estimates
#   8. Plots
#   9. Export
# =============================================================================

# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)
library(gcplyr)
library(minpack.lm)
library(cowplot)
theme_set(theme_cowplot())

DATA_PATH <- "data-processed/all-blocks-tpc-experiment.csv"
OUT       <- "data-processed/gcplyr"
FIGS      <- "figures"
dir.create(OUT, showWarnings = FALSE)

TARGET_EVO <- c("35 evolved", "40 evolved", "fRS585")

EVO_COLORS <- c(
  "40 evolved" = "#FA3208",
  "35 evolved" = "#0E63FF",
  "fRS585"     = "#000000"
)

# SSH model constants
K_B    <- 8.617333e-5   # Boltzmann constant (eV K⁻¹)
TREF_K <- 288.15        # Reference temperature: 15°C in Kelvin

# SSH fitting
N_STARTS  <- 500
CTMAX_CAP <- 50.0

# gcplyr smoothing: moving-average window (number of time points)
# Data resolution is ~15 min; window of 5 = 75-min smoothing
SMOOTH_N <- 5

# TPC prediction range
T_PRED <- seq(15, 50, length.out = 500)

set.seed(7421)

# =============================================================================
# 2. SSH model functions
# =============================================================================

# Sharpe-Schoolfield High model
sharpeschoolhigh <- function(T_c, r_tref, e, eh, th) {
  T_k <- T_c + 273.15
  r_tref *
    exp((e  / K_B) * (1 / TREF_K - 1 / T_k)) /
    (1 + exp((eh / K_B) * (1 / th - 1 / T_k)))
}

# Extract Topt, CTmax, Rmax, B80 from SSH parameters
calc_tpc_traits <- function(r_tref, e, eh, th,
                             T_range = seq(0, 55, length.out = 10000)) {
  na_result <- c(topt = NA_real_, ctmax = NA_real_, rmax = NA_real_, b80 = NA_real_)
  preds <- sharpeschoolhigh(T_range, r_tref, e, eh, th)
  valid <- is.finite(preds)
  if (!any(valid)) return(na_result)

  topt_idx <- which.max(preds[valid])
  topt     <- T_range[valid][topt_idx]
  rmax     <- preds[valid][topt_idx]

  preds_right              <- preds
  preds_right[T_range <= topt] <- rmax
  sign_changes             <- which(diff(sign(preds_right)) != 0)

  if (length(sign_changes) > 0) {
    i     <- sign_changes[1]
    t1    <- T_range[i];     t2 <- T_range[i + 1]
    p1    <- preds_right[i]; p2 <- preds_right[i + 1]
    ctmax <- if (p2 != p1) t1 - p1 * (t2 - t1) / (p2 - p1) else t1
  } else {
    below <- which(T_range > topt & preds < 0.05 * rmax)
    ctmax <- if (length(below) > 0) T_range[below[1]] else CTMAX_CAP
  }

  above_80 <- T_range[preds >= 0.8 * rmax]
  b80      <- if (length(above_80) >= 2) max(above_80) - min(above_80) else NA_real_

  c(topt = topt, ctmax = ctmax, rmax = rmax, b80 = b80)
}

# Multistart SSH fitter — bounds tuned per metric (see calls below)
fit_ssh_multistart <- function(temps, rates, bounds_lo, bounds_hi,
                                start_lo, start_hi) {
  starts <- mapply(
    function(a, b, c, d) c(a, b, c, d),
    runif(N_STARTS, start_lo["r_tref"], start_hi["r_tref"]),
    runif(N_STARTS, start_lo["e"],      start_hi["e"]),
    runif(N_STARTS, start_lo["eh"],     start_hi["eh"]),
    runif(N_STARTS, start_lo["th"],     start_hi["th"]),
    SIMPLIFY = FALSE
  )

  best_popt <- NULL
  best_rss  <- Inf

  for (p0 in starts) {
    tryCatch({
      fit <- suppressWarnings(nlsLM(
        rates ~ sharpeschoolhigh(temps, r_tref, e, eh, th),
        start   = list(r_tref = p0[1], e = p0[2], eh = p0[3], th = p0[4]),
        lower   = bounds_lo,
        upper   = bounds_hi,
        control = nls.lm.control(maxiter = 200, maxfev = 5000)
      ))
      rss <- sum(residuals(fit)^2)
      if (rss < best_rss) { best_rss <- rss; best_popt <- coef(fit) }
    }, error = function(e) NULL)
  }

  if (is.null(best_popt)) return(NULL)

  ss_tot <- sum((rates - mean(rates))^2)
  r2     <- if (ss_tot > 0) 1 - best_rss / ss_tot else NA_real_
  traits <- calc_tpc_traits(best_popt["r_tref"], best_popt["e"],
                             best_popt["eh"],     best_popt["th"])

  list(params = best_popt, r2 = r2,
       topt  = traits["topt"],  ctmax = traits["ctmax"],
       rmax  = traits["rmax"],  b80   = traits["b80"])
}

# =============================================================================
# 3. Load data
# =============================================================================

cat("Loading data...\n")

od_data <- read_csv(DATA_PATH, show_col_types = FALSE) |>
  filter(evolution_history %in% TARGET_EVO)

cat(sprintf("  %d rows | %d strains | %d temperatures | %d blocks\n",
            nrow(od_data),
            n_distinct(od_data$strain),
            n_distinct(od_data$test_temperature),
            n_distinct(od_data$block)))

# Instrument blank: minimum OD across all wells (media + noise floor)
BLANK_OD <- min(od_data$od, na.rm = TRUE)
cat(sprintf("  Blank OD: %.4f\n", BLANK_OD))

# =============================================================================
# 4. Estimate growth metrics per well with gcplyr
# =============================================================================

cat("\nEstimating growth metrics with gcplyr...\n")

gcplyr_metrics <- od_data |>
  group_by(well, block, test_temperature, strain, evolution_history) |>
  arrange(days, .by_group = TRUE) |>
  mutate(
    # Smooth OD with moving average
    od_smooth    = smooth_data(
      x = days, y = od,
      sm_method      = "moving-average",
      window_width_n = SMOOTH_N
    ),
    # Per-capita growth rate from log-transformed smoothed data
    percap_deriv = calc_deriv(
      x              = days,
      y              = od_smooth,
      percapita      = TRUE,
      blank          = BLANK_OD,
      window_width_n = SMOOTH_N,
      trans_y        = "log"
    )
  ) |>
  summarise(
    mu_max    = max(percap_deriv, na.rm = TRUE),
    doub_time = doubling_time(y = max(percap_deriv, na.rm = TRUE)),
    lag       = tryCatch(
      lag_time(x = days, y = od_smooth, deriv = percap_deriv, blank = BLANK_OD),
      error   = function(e) NA_real_,
      warning = function(w) suppressWarnings(
        lag_time(x = days, y = od_smooth, deriv = percap_deriv, blank = BLANK_OD)
      )
    ),
    auc_gc    = auc(x = days, y = od_smooth, blank = BLANK_OD),
    n_obs     = n(),
    .groups   = "drop"
  )

cat(sprintf("  Wells processed: %d\n", nrow(gcplyr_metrics)))

cat("\nMean metrics by temperature:\n")
gcplyr_metrics |>
  group_by(test_temperature) |>
  summarise(
    n          = n(),
    mean_mu    = round(mean(mu_max,    na.rm = TRUE), 2),
    mean_doub  = round(mean(doub_time, na.rm = TRUE), 2),
    mean_lag   = round(mean(lag,       na.rm = TRUE), 3),
    mean_auc   = round(mean(auc_gc,    na.rm = TRUE), 4),
    .groups    = "drop"
  ) |>
  print()

# =============================================================================
# 5. Fit SSH TPC per strain — mu_max
# =============================================================================

cat("\nFitting SSH TPCs using mu_max...\n")

# mu_max bounds: per-capita rates in per-day units
# Typical doubling times 30–120 min → mu_max ~ 8–33 /day
BOUNDS_MU <- list(
  lo    = c(r_tref = 1e-4, e = 0.01, eh =  0.5, th = 303.0),
  hi    = c(r_tref = 60.0, e =  3.0, eh = 50.0, th = 335.0),
  s_lo  = c(r_tref = 0.1,  e =  0.1, eh =  0.5, th = 305.0),
  s_hi  = c(r_tref = 30.0, e =  2.0, eh = 20.0, th = 325.0)
)

d_mu <- gcplyr_metrics |>
  filter(!is.na(mu_max), is.finite(mu_max)) |>
  select(strain, evolution_history, test_temperature, mu_max)

strains_mu <- d_mu |>
  group_by(strain, evolution_history) |>
  summarise(n_temps = n_distinct(test_temperature), .groups = "drop") |>
  filter(n_temps >= 3)

cat(sprintf("  Strains with ≥3 temperatures: %d\n", nrow(strains_mu)))

params_mu_list <- vector("list", nrow(strains_mu))
preds_mu_list  <- vector("list", nrow(strains_mu))

for (i in seq_len(nrow(strains_mu))) {
  sid <- strains_mu$strain[i]
  evo <- strains_mu$evolution_history[i]
  d   <- d_mu |> filter(strain == sid)

  res <- fit_ssh_multistart(
    d$test_temperature, d$mu_max,
    BOUNDS_MU$lo, BOUNDS_MU$hi, BOUNDS_MU$s_lo, BOUNDS_MU$s_hi
  )

  if (is.null(res)) { cat(sprintf("  FAILED: %s\n", sid)); next }

  p <- res$params
  params_mu_list[[i]] <- tibble(
    strain = sid, evolution_history = evo,
    r_tref = p["r_tref"], e = p["e"], eh = p["eh"], th = p["th"],
    topt = res$topt, ctmax = res$ctmax, rmax = res$rmax, b80 = res$b80,
    r2 = res$r2, n_obs = nrow(d)
  )
  preds_mu_list[[i]] <- tibble(
    strain = sid, evolution_history = evo,
    temp = T_PRED,
    pred = sharpeschoolhigh(T_PRED, p["r_tref"], p["e"], p["eh"], p["th"])
  )
  if (i %% 10 == 0) cat(sprintf("  %d / %d\n", i, nrow(strains_mu)))
}

tpc_params_mu <- bind_rows(params_mu_list)
tpc_preds_mu  <- bind_rows(preds_mu_list)

cat("\n=== Topt, CTmax, B80 by evolution history (mu_max) ===\n")
tpc_params_mu |>
  group_by(evolution_history) |>
  summarise(
    n          = n(),
    mean_topt  = round(mean(topt,  na.rm = TRUE), 2),
    se_topt    = round(sd(topt,    na.rm = TRUE) / sqrt(n()), 2),
    mean_ctmax = round(mean(ctmax, na.rm = TRUE), 2),
    se_ctmax   = round(sd(ctmax,   na.rm = TRUE) / sqrt(n()), 2),
    .groups    = "drop"
  ) |>
  print()

# =============================================================================
# 6. Fit SSH TPC per strain — AUC
# =============================================================================

cat("\nFitting SSH TPCs using AUC (gcplyr)...\n")

BOUNDS_AUC <- list(
  lo   = c(r_tref = 1e-6, e = 0.01, eh =  0.5, th = 303.0),
  hi   = c(r_tref = 10.0, e =  3.0, eh = 50.0, th = 335.0),
  s_lo = c(r_tref = 0.01, e =  0.1, eh =  0.5, th = 305.0),
  s_hi = c(r_tref =  2.0, e =  2.0, eh = 20.0, th = 325.0)
)

d_auc <- gcplyr_metrics |>
  filter(!is.na(auc_gc), is.finite(auc_gc)) |>
  select(strain, evolution_history, test_temperature, auc_gc)

strains_auc <- d_auc |>
  group_by(strain, evolution_history) |>
  summarise(n_temps = n_distinct(test_temperature), .groups = "drop") |>
  filter(n_temps >= 3)

params_auc_list <- vector("list", nrow(strains_auc))
preds_auc_list  <- vector("list", nrow(strains_auc))

for (i in seq_len(nrow(strains_auc))) {
  sid <- strains_auc$strain[i]
  evo <- strains_auc$evolution_history[i]
  d   <- d_auc |> filter(strain == sid)

  res <- fit_ssh_multistart(
    d$test_temperature, d$auc_gc,
    BOUNDS_AUC$lo, BOUNDS_AUC$hi, BOUNDS_AUC$s_lo, BOUNDS_AUC$s_hi
  )

  if (is.null(res)) { cat(sprintf("  FAILED: %s\n", sid)); next }

  p <- res$params
  params_auc_list[[i]] <- tibble(
    strain = sid, evolution_history = evo,
    r_tref = p["r_tref"], e = p["e"], eh = p["eh"], th = p["th"],
    topt = res$topt, ctmax = res$ctmax, rmax = res$rmax, b80 = res$b80,
    r2 = res$r2, n_obs = nrow(d)
  )
  preds_auc_list[[i]] <- tibble(
    strain = sid, evolution_history = evo,
    temp = T_PRED,
    pred = sharpeschoolhigh(T_PRED, p["r_tref"], p["e"], p["eh"], p["th"])
  )
  if (i %% 10 == 0) cat(sprintf("  %d / %d\n", i, nrow(strains_auc)))
}

tpc_params_auc <- bind_rows(params_auc_list)
tpc_preds_auc  <- bind_rows(preds_auc_list)

cat("\n=== Topt, CTmax, B80 by evolution history (AUC) ===\n")
tpc_params_auc |>
  group_by(evolution_history) |>
  summarise(
    n          = n(),
    mean_topt  = round(mean(topt,  na.rm = TRUE), 2),
    se_topt    = round(sd(topt,    na.rm = TRUE) / sqrt(n()), 2),
    mean_ctmax = round(mean(ctmax, na.rm = TRUE), 2),
    se_ctmax   = round(sd(ctmax,   na.rm = TRUE) / sqrt(n()), 2),
    .groups    = "drop"
  ) |>
  print()

# =============================================================================
# 7. Compare mu_max vs AUC TPC estimates
# =============================================================================

cat("\nComparing Topt and CTmax across metrics...\n")

both_metrics <- tpc_params_mu |>
  select(strain, evolution_history, topt_mu = topt, ctmax_mu = ctmax) |>
  inner_join(
    tpc_params_auc |> select(strain, topt_auc = topt, ctmax_auc = ctmax),
    by = "strain"
  )

cat(sprintf(
  "  Topt  — r = %.3f, mean diff = %.3f°C\n",
  cor(both_metrics$topt_mu,  both_metrics$topt_auc,  use = "complete"),
  mean(both_metrics$topt_mu  - both_metrics$topt_auc,  na.rm = TRUE)
))
cat(sprintf(
  "  CTmax — r = %.3f, mean diff = %.3f°C\n",
  cor(both_metrics$ctmax_mu, both_metrics$ctmax_auc, use = "complete"),
  mean(both_metrics$ctmax_mu - both_metrics$ctmax_auc, na.rm = TRUE)
))

# =============================================================================
# 8. Plots
# =============================================================================

# ── TPC curves — mu_max ──────────────────────────────────────────────────────

mean_curves_mu <- tpc_params_mu |>
  group_by(evolution_history) |>
  summarise(across(c(r_tref, e, eh, th), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") |>
  rowwise() |>
  mutate(curve = list(tibble(
    temp = T_PRED,
    pred = sharpeschoolhigh(T_PRED, r_tref, e, eh, th)
  ))) |>
  unnest(curve)

obs_means_mu <- gcplyr_metrics |>
  filter(!is.na(mu_max)) |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(mu_max = mean(mu_max, na.rm = TRUE), .groups = "drop")

p_tpc_mu <- ggplot() +
  geom_line(
    data = tpc_preds_mu |> mutate(pred = ifelse(pred < 0, NA, pred)),
    aes(x = temp, y = pred, group = strain, color = evolution_history),
    alpha = 0.2, linewidth = 0.8
  ) +
  geom_line(
    data = mean_curves_mu,
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 2.0
  ) +
  geom_point(
    data = obs_means_mu,
    aes(x = test_temperature, y = mu_max, color = evolution_history),
    size = 1.8, alpha = 0.6
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  coord_cartesian(xlim = c(23, 48)) +
  labs(x = "Temperature (°C)", y = expression(mu[max]~(day^-1)),
       title = expression("Thermal Performance Curves —"~mu[max])) +
  theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1),
        legend.background = element_blank())

ggsave(file.path(FIGS, "tpc-mu_max.png"), p_tpc_mu, width = 9, height = 6, dpi = 300)
cat("\nSaved: tpc-mu_max.png\n")

# ── Thermal traits — mu_max ──────────────────────────────────────────────────

p_traits_mu <- tpc_params_mu |>
  pivot_longer(c(topt, ctmax, b80), names_to = "trait", values_to = "value") |>
  mutate(trait = factor(trait,
                        levels = c("topt", "ctmax", "b80"),
                        labels = c("Topt (°C)", "CTmax (°C)", "B80 — niche breadth (°C)"))) |>
  ggplot(aes(x = evolution_history, y = value, color = evolution_history)) +
  geom_jitter(width = 0.12, size = 2.5, alpha = 0.8) +
  stat_summary(fun = mean, geom = "crossbar",
               width = 0.35, linewidth = 0.7, fatten = 1.5) +
  scale_color_manual(values = EVO_COLORS, guide = "none") +
  facet_wrap(~ trait, scales = "free_y") +
  labs(x = NULL, y = NULL,
       title = expression("Thermal traits by evolution history ("~mu[max]~")"),
       subtitle = "Crossbar = mean") +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 11))

ggsave(file.path(FIGS, "thermal-traits-mu_max.png"), p_traits_mu,
       width = 11, height = 5, dpi = 300)
cat("Saved: thermal-traits-mu_max.png\n")

# ── mu_max vs AUC TPC comparison ─────────────────────────────────────────────

p_compare_topt <- ggplot(both_metrics,
                         aes(x = topt_auc, y = topt_mu, color = evolution_history)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(x = "Topt — AUC (°C)", y = expression("Topt —"~mu[max]~"(°C)"),
       title = "Topt") +
  theme(legend.position = "none")

p_compare_ctmax <- ggplot(both_metrics,
                          aes(x = ctmax_auc, y = ctmax_mu, color = evolution_history)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(x = "CTmax — AUC (°C)", y = expression("CTmax —"~mu[max]~"(°C)"),
       title = "CTmax") +
  theme(legend.position = "bottom")

p_compare <- plot_grid(p_compare_topt, p_compare_ctmax, ncol = 2,
                       labels = c("A", "B"), label_size = 13)
ggsave(file.path(FIGS, "tpc-metric-comparison-gcplyr.png"), p_compare,
       width = 8, height = 5, dpi = 300)
cat("Saved: tpc-metric-comparison-gcplyr.png\n")

# =============================================================================
# 9. Export
# =============================================================================

write_csv(gcplyr_metrics,  file.path(OUT, "gcplyr-metrics-per-well.csv"))
write_csv(tpc_params_mu,   file.path(OUT, "tpc-params-mu_max.csv"))
write_csv(tpc_preds_mu,    file.path(OUT, "tpc-predictions-mu_max.csv"))
write_csv(tpc_params_auc,  file.path(OUT, "tpc-params-auc-gcplyr.csv"))
write_csv(tpc_preds_auc,   file.path(OUT, "tpc-predictions-auc-gcplyr.csv"))

cat("\nAll outputs saved to", OUT, "and", FIGS, "\n")
cat("Done.\n")
