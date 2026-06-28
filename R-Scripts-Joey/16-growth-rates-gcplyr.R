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
#   5. Fit SSH TPC per strain — AUC
#   6. Plots
#   7. Export
# =============================================================================


# od_data_raw <- read_csv("data-processed/all-blocks-tpc-experiment.csv")
# unique(od_data_raw$evolution_history)


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

# Sharpeschoolfield High model constants
K_B    <- 8.617333e-5   # Boltzmann constant (eV K⁻¹)
TREF_K <- 288.15        # Reference temperature: 15°C in Kelvin

# SSH fitting
N_STARTS  <- 500
TMAX_CAP <- 50.0

# gcplyr smoothing: moving-average window (number of time points)
# Data resolution is ~15 min; window of 5 = 75-min smoothing; I tried windows of 3, 5 and 11... 5 seems to do best, but I can do a formal check of this by comparing within strain CV and finding which smoothing window minimizes this.
SMOOTH_N <- 5

# Minimum OD threshold for mu_max estimation: only maximise the per-capita
# derivative after the smoothed blank-corrected OD has reached this fraction
# of the well's peak OD. Prevents the early lag-phase artifact where OD ≈ 0
# after blank subtraction inflates the log-derivative to implausibly high values.
MU_THRESH <- 0.1

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

# Extract Topt, Tmax, Rmax, B80 from SSH parameters
calc_tpc_traits <- function(r_tref, e, eh, th,
                             T_range = seq(0, 55, length.out = 10000)) {
  na_result <- c(topt = NA_real_, tmax = NA_real_, rmax = NA_real_, b80 = NA_real_)
  preds <- sharpeschoolhigh(T_range, r_tref, e, eh, th)
  valid <- is.finite(preds)
  if (!any(valid)) return(na_result)

  # Numerical optimisation for Topt 
  opt <- optim(
    par    = T_range[which.max(preds)],   # grid max as starting point
    fn     = function(T) -sharpeschoolhigh(T, r_tref, e, eh, th),
    method = "Brent",
    lower  = min(T_range),
    upper  = max(T_range)
  )
  topt <- opt$par
  rmax <- -opt$value

  # Tmax: temperature above Topt where SSH rate drops to 5% of rmax
  thresh_fn <- function(T) sharpeschoolhigh(T, r_tref, e, eh, th) - 0.05 * rmax
  tmax <- tryCatch({
    uniroot(thresh_fn, lower = topt, upper = max(T_range), tol = 1e-8)$root
  }, error = function(e) TMAX_CAP)

  above_80 <- T_range[preds >= 0.8 * rmax]
  b80      <- if (length(above_80) >= 2) max(above_80) - min(above_80) else NA_real_

  c(topt = topt, tmax = tmax, rmax = rmax, b80 = b80)
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
       topt  = traits["topt"],  tmax = traits["tmax"],
       rmax  = traits["rmax"],  b80   = traits["b80"])
}

# =============================================================================
# 3. Load data
# =============================================================================


od_data <- read_csv(DATA_PATH, show_col_types = FALSE) |>
  filter(evolution_history %in% TARGET_EVO)

# Blank correction is applied per well (min OD within each well's time series)
# rather than a global minimum, so each well's own background is subtracted.


# =============================================================================
# 4. Estimate growth metrics per well with gcplyr
# =============================================================================


gcplyr_metrics <- od_data |>
  group_by(well, block, test_temperature, strain, evolution_history) |>
  arrange(days, .by_group = TRUE) |>
  mutate(
    # Smooth OD with moving average
    od_smooth      = smooth_data(
      x = days, y = od,
      sm_method      = "moving-average",
      window_width_n = SMOOTH_N
    ),
    # Blank-corrected smooth (floored at 0) — used for mu_max threshold
    od_smooth_corr = pmax(od_smooth - min(od, na.rm = TRUE), 0),
    # Per-capita growth rate from log-transformed smoothed data
    percap_deriv   = calc_deriv(
      x              = days,
      y              = od_smooth,
      percapita      = TRUE,
      blank          = min(od, na.rm = TRUE),
      window_width_n = SMOOTH_N,
      trans_y        = "log"
    )
  ) |>
  summarise(
    # Restrict to time points where OD has risen above MU_THRESH × peak OD,
    # avoiding the early-lag artifact where log-derivative is inflated near 0.
    mu_max    = max(percap_deriv[od_smooth_corr >= MU_THRESH * max(od_smooth_corr, na.rm = TRUE)],
                    na.rm = TRUE),
    doub_time = doubling_time(y = mu_max),
    lag       = tryCatch(
      lag_time(x = days, y = od_smooth, deriv = percap_deriv, blank = min(od, na.rm = TRUE)),
      error   = function(e) NA_real_,
      warning = function(w) suppressWarnings(
        lag_time(x = days, y = od_smooth, deriv = percap_deriv, blank = min(od, na.rm = TRUE))
      )
    ),
    auc_gc    = auc(x = days, y = od_smooth, blank = min(od, na.rm = TRUE)),
    n_obs     = n(),
    .groups   = "drop"
  )


# =============================================================================
# 5. Fit SSH TPC per strain — AUC
# =============================================================================


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
    topt = res$topt, tmax = res$tmax, rmax = res$rmax, b80 = res$b80,
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

cat("\n=== Topt, Tmax, B80 by evolution history (AUC) ===\n")
tpc_params_auc |>
  group_by(evolution_history) |>
  summarise(
    n          = n(),
    mean_topt  = round(mean(topt,  na.rm = TRUE), 2),
    se_topt    = round(sd(topt,    na.rm = TRUE) / sqrt(n()), 2),
    mean_tmax = round(mean(tmax, na.rm = TRUE), 2),
    se_tmax   = round(sd(tmax,   na.rm = TRUE) / sqrt(n()), 2),
    .groups    = "drop"
  ) |>
  print()

# =============================================================================
# 5b. Fit SSH TPC per strain — mu_max
# =============================================================================
#
# Note: mu_max fits may yield implausibly high Tmax for some strains because
# 42°C is the only temperature above Topt and mu_max there is very noisy.
# The deactivation slope (eh) is poorly constrained, causing the curve to
# decline slowly and extrapolate Tmax beyond the data range. AUC-derived
# thermal traits are the primary metric; mu_max results should be interpreted
# alongside those.

cat("\nFitting SSH TPCs for mu_max...\n")

BOUNDS_MUMAX <- list(
  lo   = c(r_tref = 1e-6, e = 0.01, eh =  0.5, th = 303.0),
  hi   = c(r_tref = 50.0, e =  3.0, eh = 50.0, th = 335.0),
  s_lo = c(r_tref = 0.10, e =  0.1, eh =  0.5, th = 305.0),
  s_hi = c(r_tref = 10.0, e =  2.0, eh = 20.0, th = 325.0)
)

d_mumax <- gcplyr_metrics |>
  filter(!is.na(mu_max), is.finite(mu_max), mu_max > 0) |>
  select(strain, evolution_history, test_temperature, mu_max)

strains_mumax <- d_mumax |>
  group_by(strain, evolution_history) |>
  summarise(n_temps = n_distinct(test_temperature), .groups = "drop") |>
  filter(n_temps >= 3)

cat(sprintf("  Strains with ≥3 temperatures: %d\n", nrow(strains_mumax)))

params_mumax_list <- vector("list", nrow(strains_mumax))
preds_mumax_list  <- vector("list", nrow(strains_mumax))

for (i in seq_len(nrow(strains_mumax))) {
  sid <- strains_mumax$strain[i]
  evo <- strains_mumax$evolution_history[i]
  d   <- d_mumax |> filter(strain == sid)

  res <- fit_ssh_multistart(
    d$test_temperature, d$mu_max,
    BOUNDS_MUMAX$lo, BOUNDS_MUMAX$hi, BOUNDS_MUMAX$s_lo, BOUNDS_MUMAX$s_hi
  )

  if (is.null(res)) { cat(sprintf("  FAILED: %s\n", sid)); next }

  p <- res$params
  params_mumax_list[[i]] <- tibble(
    strain = sid, evolution_history = evo,
    r_tref = p["r_tref"], e = p["e"], eh = p["eh"], th = p["th"],
    topt = res$topt, tmax = res$tmax, rmax = res$rmax, b80 = res$b80,
    r2 = res$r2, n_obs = nrow(d)
  )
  preds_mumax_list[[i]] <- tibble(
    strain = sid, evolution_history = evo,
    temp = T_PRED,
    pred = sharpeschoolhigh(T_PRED, p["r_tref"], p["e"], p["eh"], p["th"])
  )
  if (i %% 10 == 0) cat(sprintf("  %d / %d\n", i, nrow(strains_mumax)))
}

tpc_params_mumax <- bind_rows(params_mumax_list)
tpc_preds_mumax  <- bind_rows(preds_mumax_list)

cat("\n=== Topt, Tmax, B80 by evolution history (mu_max) ===\n")
tpc_params_mumax |>
  group_by(evolution_history) |>
  summarise(
    n         = n(),
    mean_topt = round(mean(topt, na.rm = TRUE), 2),
    se_topt   = round(sd(topt,   na.rm = TRUE) / sqrt(n()), 2),
    mean_tmax = round(mean(tmax, na.rm = TRUE), 2),
    se_tmax   = round(sd(tmax,   na.rm = TRUE) / sqrt(n()), 2),
    .groups   = "drop"
  ) |>
  print()


# =============================================================================
# 6. Plots
# =============================================================================

# ── TPC curves — AUC ─────────────────────────────────────────────────────────

mean_curves_auc <- tpc_preds_auc |>
  group_by(evolution_history, temp) |>
  summarise(pred = mean(pred, na.rm = TRUE), .groups = "drop")

obs_means_auc <- gcplyr_metrics |>
  filter(!is.na(auc_gc)) |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(auc_gc = mean(auc_gc, na.rm = TRUE), .groups = "drop")

p_tpc_auc <- ggplot() +
  geom_line(
    data = tpc_preds_auc |> mutate(pred = ifelse(pred < 0, NA, pred)),
    aes(x = temp, y = pred, group = strain, color = evolution_history),
    alpha = 0.2, linewidth = 0.8
  ) +
  geom_line(
    data = mean_curves_auc,
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 2.0
  ) +
  geom_point(
    data = obs_means_auc,
    aes(x = test_temperature, y = auc_gc, color = evolution_history),
    size = 1.8, alpha = 0.6
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  coord_cartesian(xlim = c(23, 48)) +
  labs(x = "Temperature (°C)", y = "AUC (empirical)",
       title = "Thermal Performance Curves — AUC (gcplyr)") +
  theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1),
        legend.background = element_blank())

ggsave(file.path(FIGS, "tpc-auc-gcplyr.png"), p_tpc_auc, width = 9, height = 6, dpi = 300)


# ── TPC curves — AUC, faceted by evolution history ───────────────────────────

p_tpc_auc_facet <- ggplot() +
  geom_line(
    data = tpc_preds_auc |> mutate(pred = ifelse(pred < 0, NA, pred)),
    aes(x = temp, y = pred, group = strain),
    color = "grey70", alpha = 0.6, linewidth = 0.5
  ) +
  geom_line(
    data = mean_curves_auc,
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 2.0
  ) +
  geom_point(
    data = obs_means_auc,
    aes(x = test_temperature, y = auc_gc, color = evolution_history),
    size = 1.8, alpha = 0.7
  ) +
  scale_color_manual(values = EVO_COLORS, guide = "none") +
  coord_cartesian(xlim = c(23, 48)) +
  facet_wrap(~ evolution_history) +
  labs(
    x = "Temperature (°C)", y = "AUC (empirical)",
    title = "Thermal Performance Curves — AUC (gcplyr)",
    subtitle = "Grey = individual strains  |  Bold = mean prediction across strains"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 12)
  )

ggsave(file.path(FIGS, "tpc-auc-gcplyr-faceted.png"), p_tpc_auc_facet,
       width = 12, height = 5, dpi = 300)


# ── Thermal traits — AUC ─────────────────────────────────────────────────────

p_traits_auc <- tpc_params_auc |>
  pivot_longer(c(topt, tmax, b80), names_to = "trait", values_to = "value") |>
  mutate(trait = factor(trait,
                        levels = c("topt", "tmax", "b80"),
                        labels = c("Topt (°C)", "Tmax (°C)", "B80 — niche breadth (°C)"))) |>
  ggplot(aes(x = evolution_history, y = value, color = evolution_history)) +
  geom_jitter(width = 0.12, size = 2.5, alpha = 0.8) +
  stat_summary(fun = mean, geom = "crossbar",
               width = 0.35, linewidth = 0.7, fatten = 1.5) +
  scale_color_manual(values = EVO_COLORS, guide = "none") +
  facet_wrap(~ trait, scales = "free_y") +
  labs(x = NULL, y = NULL,
       title = "Thermal traits by evolution history (AUC, gcplyr)",
       subtitle = "Crossbar = mean") +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 11))

ggsave(file.path(FIGS, "thermal-traits-auc-gcplyr.png"), p_traits_auc,
       width = 11, height = 5, dpi = 300)


# ── Thermal traits dot plot with ancestor reference line ─────────────────────

anc_topt <- tpc_params_auc |> filter(evolution_history == "fRS585") |> pull(topt)
anc_tmax <- tpc_params_auc |> filter(evolution_history == "fRS585") |> pull(tmax)

plot_data_dotplot <- tpc_params_auc |>
  filter(evolution_history != "fRS585") |>
  select(strain, evolution_history, topt, tmax) |>
  pivot_longer(c(topt, tmax), names_to = "trait", values_to = "value") |>
  mutate(trait = factor(trait, levels = c("topt", "tmax")))

anc_ref_dotplot <- tibble(
  trait   = factor(c("topt", "tmax"), levels = c("topt", "tmax")),
  anc_val = c(anc_topt, anc_tmax)
)

group_means_dotplot <- plot_data_dotplot |>
  summarise(mean_val = mean(value), se = sd(value) / sqrt(n()),
            .by = c(evolution_history, trait))

trait_labels_dotplot <- c(topt = "Topt (\u00b0C)", tmax = "Tmax (\u00b0C)")

p_traits_dotplot <- ggplot(plot_data_dotplot,
                           aes(x = evolution_history, y = value,
                               color = evolution_history)) +
  geom_hline(data = anc_ref_dotplot, aes(yintercept = anc_val),
             linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(width = 0.12, size = 1.8, alpha = 0.6) +
  geom_pointrange(
    data = group_means_dotplot,
    aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
    size = 0.7, linewidth = 1.1
  ) +
  facet_wrap(~ trait, scales = "free_y", labeller = as_labeller(trait_labels_dotplot)) +
  scale_color_manual(values = EVO_COLORS) +
  labs(x = NULL, y = "Temperature (\u00b0C)",
       caption = paste("Points: individual strains  |",
                       "Large point \u00b1 bar: mean \u00b1 SE  |",
                       "Dashed line: ancestor (fRS585)")) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        strip.text = element_text(size = 13),
        plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "thermal-traits-dotplot-auc-gcplyr.png"), p_traits_dotplot,
       width = 8, height = 5, dpi = 300)


# ── Statistical tests: evolved groups vs each other and vs ancestor ──────────

g35 <- tpc_params_auc |> filter(evolution_history == "35 evolved")
g40 <- tpc_params_auc |> filter(evolution_history == "40 evolved")

# Normality check (Shapiro-Wilk) on evolved groups
sw <- tibble(
  group  = rep(c("35 evolved", "40 evolved"), 2),
  trait  = c(rep("topt", 2), rep("tmax", 2)),
  W      = c(
    shapiro.test(g35$topt)$statistic, shapiro.test(g40$topt)$statistic,
    shapiro.test(g35$tmax)$statistic, shapiro.test(g40$tmax)$statistic
  ),
  p_sw   = c(
    shapiro.test(g35$topt)$p.value, shapiro.test(g40$topt)$p.value,
    shapiro.test(g35$tmax)$p.value, shapiro.test(g40$tmax)$p.value
  )
)

# Helper: run Welch two-sample or one-sample t-test + Wilcoxon, return tidy row
run_tests2 <- function(x, y = NULL, mu = NULL, label_x, label_y = "ancestor", trait) {
  if (!is.null(y)) {
    t_res <- tryCatch(t.test(x, y),                        error = function(e) NULL)
    w_res <- tryCatch(wilcox.test(x, y, exact = FALSE),    error = function(e) NULL)
    ref   <- mean(y)
  } else {
    t_res <- tryCatch(t.test(x, mu = mu),                  error = function(e) NULL)
    w_res <- tryCatch(wilcox.test(x, mu = mu, exact = FALSE), error = function(e) NULL)
    ref   <- mu
  }
  tibble(
    trait      = trait,
    comparison = paste(label_x, "vs", label_y),
    mean_x     = mean(x),
    ref        = ref,
    diff       = mean(x) - ref,
    ci_lo      = if (!is.null(t_res)) { if (!is.null(y)) t_res$conf.int[1] else t_res$conf.int[1] - mu } else NA_real_,
    ci_hi      = if (!is.null(t_res)) { if (!is.null(y)) t_res$conf.int[2] else t_res$conf.int[2] - mu } else NA_real_,
    p_welch    = if (!is.null(t_res)) t_res$p.value else NA_real_,
    p_wilcox   = if (!is.null(w_res)) w_res$p.value else NA_real_
  )
}

results2 <- bind_rows(
  run_tests2(g35$topt, g40$topt,      label_x = "35 evolved", label_y = "40 evolved", trait = "topt"),
  run_tests2(g35$topt, mu = anc_topt, label_x = "35 evolved", trait = "topt"),
  run_tests2(g40$topt, mu = anc_topt, label_x = "40 evolved", trait = "topt"),
  run_tests2(g35$tmax, g40$tmax,      label_x = "35 evolved", label_y = "40 evolved", trait = "tmax"),
  run_tests2(g35$tmax, mu = anc_tmax, label_x = "35 evolved", trait = "tmax"),
  run_tests2(g40$tmax, mu = anc_tmax, label_x = "40 evolved", trait = "tmax")
) |>
  group_by(trait) |>
  mutate(
    p_welch_holm  = p.adjust(p_welch,  method = "holm"),
    p_wilcox_holm = p.adjust(p_wilcox, method = "holm")
  ) |>
  ungroup()


# ── Dot plot with significance annotations ───────────────────────────────────

p_stars <- function(p) dplyr::case_when(
  p < 0.001 ~ "***",
  p < 0.01  ~ "**",
  p < 0.05  ~ "*",
  TRUE      ~ "ns"
)

# y-range per trait for annotation placement
y_range <- plot_data_dotplot |>
  summarise(y_max = max(value), y_span = diff(range(value)), .by = trait)

# Between-group bracket (35 evolved vs 40 evolved)
bracket_df <- results2 |>
  filter(comparison == "35 evolved vs 40 evolved") |>
  mutate(
    trait       = factor(trait, levels = c("topt", "tmax")),
    xmin        = "35 evolved",
    xmax        = "40 evolved",
    annotations = p_stars(p_welch_holm)
  ) |>
  left_join(y_range, by = "trait") |>
  mutate(y_position = y_max + y_span * 0.06)

# vs-ancestor stars placed to the right of each group's mean ± SE bar
anc_star_df <- results2 |>
  filter(stringr::str_detect(comparison, "vs ancestor")) |>
  mutate(
    trait             = factor(trait, levels = c("topt", "tmax")),
    evolution_history = stringr::str_remove(comparison, " vs ancestor"),
    label             = p_stars(p_welch_holm)
  ) |>
  left_join(group_means_dotplot, by = c("trait", "evolution_history")) |>
  left_join(y_range, by = "trait") |>
  mutate(y_pos = mean_val + se + y_span * 0.04)

p_traits_dotplot_sig <- ggplot(plot_data_dotplot,
                               aes(x = evolution_history, y = value,
                                   color = evolution_history)) +
  geom_hline(data = anc_ref_dotplot, aes(yintercept = anc_val),
             linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(width = 0.12, size = 1.8, alpha = 0.6) +
  geom_pointrange(
    data = group_means_dotplot,
    aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
    size = 0.7, linewidth = 1.1
  ) +
  suppressWarnings(ggsignif::geom_signif(
    data       = bracket_df,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual     = TRUE,
    tip_length = 0.02,
    textsize   = 4.5,
    color      = "black"
  )) +
  geom_text(
    data     = anc_star_df,
    aes(x = evolution_history, y = y_pos, label = label),
    color    = "black",
    size     = 4,
    fontface = "bold",
    nudge_x  = 0.3
  ) +
  facet_wrap(~ trait, scales = "free_y", labeller = as_labeller(trait_labels_dotplot)) +
  scale_color_manual(values = EVO_COLORS) +
  labs(x = NULL, y = "Temperature (\u00b0C)",
       caption = paste(
         "Points: individual strains  |  Large point \u00b1 bar: mean \u00b1 SE  |",
         "Dashed line: ancestor (fRS585)\n",
         "Brackets: Welch t-test (Holm-corrected)  |  Stars to right of mean: vs. ancestor (one-sample t-test)"
       )) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        strip.text       = element_text(size = 13),
        plot.caption     = element_text(size = 8, color = "grey40"))

ggsave(file.path(FIGS, "thermal-traits-dotplot-sig-auc-gcplyr.png"), p_traits_dotplot_sig,
       width = 8, height = 5, dpi = 300)


# ── TPC curves — mu_max ──────────────────────────────────────────────────────

mean_curves_mumax <- tpc_preds_mumax |>
  group_by(evolution_history, temp) |>
  summarise(pred = mean(pred, na.rm = TRUE), .groups = "drop")

obs_means_mumax <- gcplyr_metrics |>
  filter(!is.na(mu_max)) |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(mu_max = mean(mu_max, na.rm = TRUE), .groups = "drop")

p_tpc_mumax <- ggplot() +
  geom_line(
    data = tpc_preds_mumax |> mutate(pred = ifelse(pred < 0, NA, pred)),
    aes(x = temp, y = pred, group = strain, color = evolution_history),
    alpha = 0.2, linewidth = 0.8
  ) +
  geom_line(
    data = mean_curves_mumax,
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 2.0
  ) +
  geom_point(
    data = obs_means_mumax,
    aes(x = test_temperature, y = mu_max, color = evolution_history),
    size = 1.8, alpha = 0.6
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  coord_cartesian(xlim = c(23, 48)) +
  labs(x = "Temperature (\u00b0C)", y = "Max per-capita growth rate \u03bc_max (day\u207b\u00b9)",
       title = "Thermal Performance Curves \u2014 \u03bc_max (gcplyr)") +
  theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1),
        legend.background = element_blank())

p_tpc_mumax_facet <- ggplot() +
  geom_line(
    data = tpc_preds_mumax |> mutate(pred = ifelse(pred < 0, NA, pred)),
    aes(x = temp, y = pred, group = strain),
    color = "grey70", alpha = 0.6, linewidth = 0.5
  ) +
  geom_line(
    data = mean_curves_mumax,
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 2.0
  ) +
  geom_point(
    data = obs_means_mumax,
    aes(x = test_temperature, y = mu_max, color = evolution_history),
    size = 1.8, alpha = 0.7
  ) +
  scale_color_manual(values = EVO_COLORS, guide = "none") +
  coord_cartesian(xlim = c(23, 48)) +
  facet_wrap(~ evolution_history) +
  labs(
    x = "Temperature (\u00b0C)", y = "Max per-capita growth rate \u03bc_max (day\u207b\u00b9)",
    title    = "Thermal Performance Curves \u2014 \u03bc_max (gcplyr)",
    subtitle = "Grey = individual strains  |  Bold = mean prediction across strains"
  ) +
  theme(strip.background = element_blank(),
        strip.text       = element_text(face = "bold", size = 12))

ggsave(file.path(FIGS, "tpc-mumax-gcplyr.png"),        p_tpc_mumax,       width = 9,  height = 6, dpi = 300)
ggsave(file.path(FIGS, "tpc-mumax-gcplyr-faceted.png"), p_tpc_mumax_facet, width = 12, height = 5, dpi = 300)


# ── Statistical tests — mu_max ───────────────────────────────────────────────

anc_topt_mu <- tpc_params_mumax |> filter(evolution_history == "fRS585") |> pull(topt) |> unname()
anc_tmax_mu <- tpc_params_mumax |> filter(evolution_history == "fRS585") |> pull(tmax) |> unname()

g35_mu <- tpc_params_mumax |> filter(evolution_history == "35 evolved")
g40_mu <- tpc_params_mumax |> filter(evolution_history == "40 evolved")

results_mumax <- bind_rows(
  run_tests2(g35_mu$topt, g40_mu$topt,      label_x = "35 evolved", label_y = "40 evolved", trait = "topt"),
  run_tests2(g35_mu$topt, mu = anc_topt_mu, label_x = "35 evolved",                         trait = "topt"),
  run_tests2(g40_mu$topt, mu = anc_topt_mu, label_x = "40 evolved",                         trait = "topt"),
  run_tests2(g35_mu$tmax, g40_mu$tmax,      label_x = "35 evolved", label_y = "40 evolved", trait = "tmax"),
  run_tests2(g35_mu$tmax, mu = anc_tmax_mu, label_x = "35 evolved",                         trait = "tmax"),
  run_tests2(g40_mu$tmax, mu = anc_tmax_mu, label_x = "40 evolved",                         trait = "tmax")
) |>
  group_by(trait) |>
  mutate(
    p_welch_holm  = p.adjust(p_welch,  method = "holm"),
    p_wilcox_holm = p.adjust(p_wilcox, method = "holm")
  ) |>
  ungroup()


# ── Dotplot with significance annotations — mu_max ───────────────────────────

plot_data_mumax <- tpc_params_mumax |>
  filter(evolution_history != "fRS585") |>
  select(strain, evolution_history, topt, tmax) |>
  pivot_longer(c(topt, tmax), names_to = "trait", values_to = "value") |>
  mutate(trait = factor(trait, levels = c("topt", "tmax")))

anc_ref_mumax <- tibble(
  trait   = factor(c("topt", "tmax"), levels = c("topt", "tmax")),
  anc_val = c(anc_topt_mu, anc_tmax_mu)
)

gmeans_mumax <- plot_data_mumax |>
  summarise(mean_val = mean(value), se = sd(value) / sqrt(n()),
            .by = c(evolution_history, trait))

yrange_mumax <- plot_data_mumax |>
  summarise(y_max = max(value), y_span = diff(range(value)), .by = trait)

bracket_mumax <- results_mumax |>
  filter(comparison == "35 evolved vs 40 evolved") |>
  mutate(
    trait       = factor(trait, levels = c("topt", "tmax")),
    xmin        = "35 evolved", xmax = "40 evolved",
    annotations = p_stars(p_welch_holm)
  ) |>
  left_join(yrange_mumax, by = "trait") |>
  mutate(y_position = y_max + y_span * 0.06)

anc_star_mumax <- results_mumax |>
  filter(str_detect(comparison, "vs ancestor")) |>
  mutate(
    trait             = factor(trait, levels = c("topt", "tmax")),
    evolution_history = str_remove(comparison, " vs ancestor"),
    label             = p_stars(p_welch_holm)
  ) |>
  left_join(gmeans_mumax, by = c("trait", "evolution_history")) |>
  left_join(yrange_mumax, by = "trait") |>
  mutate(y_pos = mean_val + se + y_span * 0.04)

p_traits_mumax_sig <- ggplot(plot_data_mumax,
                              aes(x = evolution_history, y = value,
                                  color = evolution_history)) +
  geom_hline(data = anc_ref_mumax, aes(yintercept = anc_val),
             linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(width = 0.12, size = 1.8, alpha = 0.6) +
  geom_pointrange(
    data = gmeans_mumax,
    aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
    size = 0.7, linewidth = 1.1
  ) +
  suppressWarnings(ggsignif::geom_signif(
    data       = bracket_mumax,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual     = TRUE, tip_length = 0.02, textsize = 4.5, color = "black"
  )) +
  geom_text(
    data     = anc_star_mumax,
    aes(x = evolution_history, y = y_pos, label = label),
    color    = "black", size = 4, fontface = "bold", nudge_x = 0.3
  ) +
  facet_wrap(~ trait, scales = "free_y",
             labeller = as_labeller(trait_labels_dotplot)) +
  scale_color_manual(values = EVO_COLORS) +
  labs(x = NULL, y = "Temperature (\u00b0C)",
       caption = paste(
         "Points: individual strains  |  Large point \u00b1 bar: mean \u00b1 SE  |",
         "Dashed line: ancestor (fRS585)\n",
         "Brackets: Welch t-test (Holm-corrected)  |",
         "Stars to right of mean: vs. ancestor (one-sample t-test)"
       )) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        strip.text   = element_text(size = 13),
        plot.caption = element_text(size = 8, color = "grey40"))

ggsave(file.path(FIGS, "thermal-traits-dotplot-sig-mumax-gcplyr.png"),
       p_traits_mumax_sig, width = 8, height = 5, dpi = 300)


# ── AUC at 41°C and 42°C: predicted from TPC fit and observed ────────────────

# Method 1: predicted AUC at 41 and 42°C from fitted SSH params
pred_auc_temps <- tpc_params_auc |>
  mutate(
    auc_pred_41 = sharpeschoolhigh(41, r_tref, e, eh, th),
    auc_pred_42 = sharpeschoolhigh(42, r_tref, e, eh, th)
  ) |>
  select(strain, evolution_history, auc_pred_41, auc_pred_42)

# Method 2: observed AUC per strain (mean across blocks) at 41 and 42°C
obs_auc_temps <- obs_means_auc |>
  filter(test_temperature %in% c(41, 42)) |>
  mutate(col = paste0("auc_obs_", test_temperature)) |>
  select(strain, evolution_history, col, auc_gc) |>
  pivot_wider(names_from = col, values_from = auc_gc)

# Ancestor reference scalars (unnamed to avoid compound names in c())
anc_pred_41 <- pred_auc_temps |> filter(evolution_history == "fRS585") |> pull(auc_pred_41) |> unname()
anc_pred_42 <- pred_auc_temps |> filter(evolution_history == "fRS585") |> pull(auc_pred_42) |> unname()
anc_obs_41  <- obs_auc_temps  |> filter(evolution_history == "fRS585") |> pull(auc_obs_41)  |> unname()
anc_obs_42  <- obs_auc_temps  |> filter(evolution_history == "fRS585") |> pull(auc_obs_42)  |> unname()

# Statistical tests (reuses run_tests2 and p_stars defined above)
run_auc_tests <- function(data, trait_col, trait_label, anc_val) {
  g35 <- data |> filter(evolution_history == "35 evolved") |> pull({{ trait_col }})
  g40 <- data |> filter(evolution_history == "40 evolved") |> pull({{ trait_col }})
  bind_rows(
    run_tests2(g35, g40,          label_x = "35 evolved", label_y = "40 evolved", trait = trait_label),
    run_tests2(g35, mu = anc_val, label_x = "35 evolved",                         trait = trait_label),
    run_tests2(g40, mu = anc_val, label_x = "40 evolved",                         trait = trait_label)
  ) |>
    mutate(
      p_welch_holm  = p.adjust(p_welch,  method = "holm"),
      p_wilcox_holm = p.adjust(p_wilcox, method = "holm")
    )
}

results_pred <- bind_rows(
  run_auc_tests(pred_auc_temps, auc_pred_41, "AUC at 41\u00b0C", anc_pred_41),
  run_auc_tests(pred_auc_temps, auc_pred_42, "AUC at 42\u00b0C", anc_pred_42)
) |> mutate(method = "Predicted (TPC)")

results_obs <- bind_rows(
  run_auc_tests(obs_auc_temps, auc_obs_41, "AUC at 41\u00b0C", anc_obs_41),
  run_auc_tests(obs_auc_temps, auc_obs_42, "AUC at 42\u00b0C", anc_obs_42)
) |> mutate(method = "Observed")

# Dot plot helper: facets = 41°C and 42°C; bracket = group vs group;
# stars = each group vs ancestor reference line
make_auc_dotplot <- function(auc_data, results_df, anc_vals, method_label) {
  trait_map <- c("AUC at 41\u00b0C", "AUC at 42\u00b0C")
  names(trait_map) <- names(anc_vals)

  plot_df <- auc_data |>
    filter(evolution_history != "fRS585") |>
    pivot_longer(names(anc_vals), names_to = "trait_col", values_to = "value") |>
    mutate(trait = factor(trait_map[trait_col], levels = trait_map))

  anc_ref <- tibble(
    trait   = factor(trait_map, levels = trait_map),
    anc_val = unname(anc_vals)
  )

  gmeans <- plot_df |>
    summarise(mean_val = mean(value), se = sd(value) / sqrt(n()),
              .by = c(evolution_history, trait))

  yrange <- plot_df |>
    summarise(y_max = max(value), y_span = diff(range(value)), .by = trait)

  bdf <- results_df |>
    filter(comparison == "35 evolved vs 40 evolved") |>
    mutate(
      trait       = factor(trait, levels = trait_map),
      xmin        = "35 evolved",
      xmax        = "40 evolved",
      annotations = p_stars(p_welch_holm)
    ) |>
    left_join(yrange, by = "trait") |>
    mutate(y_position = y_max + y_span * 0.06)

  sdf <- results_df |>
    filter(str_detect(comparison, "vs ancestor")) |>
    mutate(
      trait             = factor(trait, levels = trait_map),
      evolution_history = str_remove(comparison, " vs ancestor"),
      label             = p_stars(p_welch_holm)
    ) |>
    left_join(gmeans, by = c("trait", "evolution_history")) |>
    left_join(yrange, by = "trait") |>
    mutate(y_pos = mean_val + se + y_span * 0.04)

  ggplot(plot_df, aes(x = evolution_history, y = value, color = evolution_history)) +
    geom_hline(data = anc_ref, aes(yintercept = anc_val),
               linetype = "dashed", color = "#000000", linewidth = 0.6) +
    geom_jitter(width = 0.12, size = 1.8, alpha = 0.6) +
    geom_pointrange(
      data = gmeans,
      aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
      size = 0.7, linewidth = 1.1
    ) +
    suppressWarnings(ggsignif::geom_signif(
      data       = bdf,
      aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
      manual     = TRUE, tip_length = 0.02, textsize = 4.5, color = "black"
    )) +
    geom_text(
      data    = sdf,
      aes(x = evolution_history, y = y_pos, label = label),
      color   = "black", size = 4, fontface = "bold", nudge_x = 0.3
    ) +
    facet_wrap(~ trait, scales = "free_y") +
    scale_color_manual(values = EVO_COLORS) +
    labs(
      x       = NULL,
      y       = "AUC (OD \u00d7 time)",
      title   = method_label,
      caption = paste(
        "Points: individual strains  |  Large point \u00b1 bar: mean \u00b1 SE  |",
        "Dashed line: ancestor (fRS585)\n",
        "Brackets: Welch t-test (Holm-corrected)  |",
        "Stars to right of mean: vs. ancestor (one-sample t-test)"
      )
    ) +
    theme_bw(base_size = 13) +
    theme(legend.position = "none",
          strip.text   = element_text(size = 13),
          plot.title   = element_text(size = 13, face = "bold"),
          plot.caption = element_text(size = 8, color = "grey40"))
}

anc_pred_vals <- c(auc_pred_41 = anc_pred_41, auc_pred_42 = anc_pred_42)
anc_obs_vals  <- c(auc_obs_41  = anc_obs_41,  auc_obs_42  = anc_obs_42)

p_auc_pred <- make_auc_dotplot(
  pred_auc_temps, results_pred, anc_pred_vals,
  "AUC at 41\u00b0C and 42\u00b0C \u2014 Predicted from TPC fit"
)
p_auc_obs <- make_auc_dotplot(
  obs_auc_temps, results_obs, anc_obs_vals,
  "AUC at 41\u00b0C and 42\u00b0C \u2014 Observed (mean across blocks)"
)

ggsave(file.path(FIGS, "auc-dotplot-predicted-gcplyr.png"), p_auc_pred,
       width = 8, height = 5, dpi = 300)
ggsave(file.path(FIGS, "auc-dotplot-observed-gcplyr.png"), p_auc_obs,
       width = 8, height = 5, dpi = 300)


# ── Per-strain TPCs with raw data, PDF ───────────────────────────────────────

# Strip labels include R² for quick fit assessment
strain_labels <- tpc_params_auc |>
  arrange(evolution_history, strain) |>
  mutate(label = sprintf("%s  (R²=%.2f)", strain, r2))

tpc_preds_labeled <- tpc_preds_auc |>
  left_join(select(strain_labels, strain, label), by = "strain") |>
  mutate(
    pred  = ifelse(pred < 0, NA, pred),
    label = factor(label, levels = strain_labels$label)
  )

data_labeled <- d_auc |>
  left_join(select(strain_labels, strain, label), by = "strain") |>
  mutate(label = factor(label, levels = strain_labels$label))

vlines <- strain_labels |>
  pivot_longer(c(topt, tmax), names_to = "trait", values_to = "temp_val") |>
  mutate(label = factor(label, levels = strain_labels$label))

p_per_strain <- ggplot() +
  geom_line(
    data = tpc_preds_labeled,
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 0.7
  ) +
  geom_point(
    data = data_labeled,
    aes(x = test_temperature, y = auc_gc, color = evolution_history),
    size = 1.5, alpha = 0.7
  ) +
  geom_vline(
    data = vlines,
    aes(xintercept = temp_val, linetype = trait),
    color = "grey40", linewidth = 0.4
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_linetype_manual(values = c(topt = "dashed", tmax = "dotted"),
                        labels = c(topt = "Topt", tmax = "Tmax")) +
  coord_cartesian(xlim = c(23, 45)) +
  facet_wrap(~ label, ncol = 6) +
  labs(x = "Temperature (°C)", y = "AUC", linetype = NULL) +
  theme(
    legend.position  = "bottom",
    strip.background = element_blank(),
    strip.text       = element_text(size = 7),
    axis.text        = element_text(size = 6)
  )

ggsave(file.path(FIGS, "tpc-per-strain-gcplyr.pdf"), p_per_strain,
       width = 16, height = 18)


# ── Per-well mu_max diagnostic PDF ───────────────────────────────────────────
#
# Two-panel layout per well:
#   Top:    smoothed blank-corrected OD over time; vertical line = time of mu_max
#   Bottom: per-capita growth rate over time; horizontal line = mu_max value
# Shared x-axis (time) via facet_grid(panel ~ well), making it easy to see
# where in the growth curve the peak rate was estimated.

# Recompute smoothed OD and per-capita derivative per well
mumax_plot_raw <- od_data |>
  group_by(well, block, test_temperature, strain, evolution_history) |>
  arrange(days, .by_group = TRUE) |>
  mutate(
    blank_od       = min(od, na.rm = TRUE),
    od_smooth      = smooth_data(
      x = days, y = od,
      sm_method = "moving-average", window_width_n = SMOOTH_N
    ),
    od_smooth_corr = pmax(od_smooth - blank_od, 0),
    percap_deriv   = calc_deriv(
      x              = days,
      y              = od_smooth,
      percapita      = TRUE,
      blank          = min(od, na.rm = TRUE),
      window_width_n = SMOOTH_N,
      trans_y        = "log"
    )
  ) |>
  ungroup() |>
  left_join(
    gcplyr_metrics |>
      select(well, block, test_temperature, strain, evolution_history, mu_max),
    by = c("well", "block", "test_temperature", "strain", "evolution_history")
  )

# Time at which mu_max occurs per well (for vertical reference line in OD panel).
# Apply the same MU_THRESH used in section 4 so the marker is consistent with
# how mu_max was estimated.
mumax_times <- mumax_plot_raw |>
  group_by(well, block, test_temperature, strain, evolution_history, mu_max) |>
  filter(od_smooth_corr >= MU_THRESH * max(od_smooth_corr, na.rm = TRUE)) |>
  slice_max(percap_deriv, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(well, block, test_temperature, strain, evolution_history, mu_max, t_mumax = days)

# Build strip labels and well ordering
mumax_plot_raw <- mumax_plot_raw |>
  left_join(
    mumax_times |> select(well, block, test_temperature, strain, evolution_history, t_mumax),
    by = c("well", "block", "test_temperature", "strain", "evolution_history")
  ) |>
  mutate(
    well_id   = paste(strain, test_temperature, block, well, sep = "_"),
    strip_lab = sprintf("%s | %d\u00b0C B%d [%s]\n\u03bc_max=%.2f",
                        strain, test_temperature, block, well, mu_max)
  )

mu_well_order <- mumax_plot_raw |>
  distinct(well_id, strip_lab, evolution_history, strain, test_temperature, block, well) |>
  arrange(evolution_history, strain, test_temperature, block, well)

mumax_plot_raw <- mumax_plot_raw |>
  mutate(strip_lab = factor(strip_lab, levels = unique(mu_well_order$strip_lab)))

# Build long-format combined dataframe for facet_grid
mumax_od_panel <- mumax_plot_raw |>
  select(days, value = od_smooth_corr, strip_lab, well_id, evolution_history,
         t_mumax, mu_max) |>
  mutate(panel = "OD (smoothed, blank-corrected)")

mumax_deriv_panel <- mumax_plot_raw |>
  select(days, value = percap_deriv, strip_lab, well_id, evolution_history,
         t_mumax, mu_max) |>
  mutate(panel = "Per-capita growth rate (day\u207b\u00b9)")

mumax_combined <- bind_rows(mumax_od_panel, mumax_deriv_panel) |>
  mutate(panel = factor(panel,
                        levels = c("OD (smoothed, blank-corrected)",
                                   "Per-capita growth rate (day\u207b\u00b9)")))

# Reference lines: vertical (OD panel, time of mu_max) + horizontal (deriv panel, mu_max value)
ref_vline <- mumax_times |>
  left_join(
    mumax_plot_raw |> distinct(well, block, test_temperature, strain, evolution_history,
                                strip_lab),
    by = c("well", "block", "test_temperature", "strain", "evolution_history")
  ) |>
  mutate(panel = factor("OD (smoothed, blank-corrected)",
                        levels = levels(mumax_combined$panel)))

ref_hline <- mumax_times |>
  left_join(
    mumax_plot_raw |> distinct(well, block, test_temperature, strain, evolution_history,
                                strip_lab),
    by = c("well", "block", "test_temperature", "strain", "evolution_history")
  ) |>
  mutate(panel = factor("Per-capita growth rate (day\u207b\u00b9)",
                        levels = levels(mumax_combined$panel)))

# Paginate: 6 wells per page (2-row panels per well)
MU_N_COLS     <- 6
mu_all_labs   <- levels(mumax_plot_raw$strip_lab)
mu_n_pages    <- ceiling(length(mu_all_labs) / MU_N_COLS)

pdf(file.path(FIGS, "mumax-per-well-gcplyr.pdf"), width = 16, height = 7)
for (pg in seq_len(mu_n_pages)) {
  idx  <- seq((pg - 1) * MU_N_COLS + 1, min(pg * MU_N_COLS, length(mu_all_labs)))
  labs <- mu_all_labs[idx]

  df_pg     <- mumax_combined |> filter(strip_lab %in% labs) |>
    mutate(strip_lab = factor(strip_lab, levels = labs))
  vline_pg  <- ref_vline  |> filter(strip_lab %in% labs) |>
    mutate(strip_lab = factor(strip_lab, levels = labs))
  hline_pg  <- ref_hline  |> filter(strip_lab %in% labs) |>
    mutate(strip_lab = factor(strip_lab, levels = labs))

  p_pg <- ggplot(df_pg, aes(x = days, y = value,
                             color = evolution_history, group = well_id)) +
    geom_line(linewidth = 0.6, na.rm = TRUE) +
    geom_vline(
      data     = vline_pg,
      aes(xintercept = t_mumax),
      linetype = "dashed", color = "grey40", linewidth = 0.5
    ) +
    geom_hline(
      data     = hline_pg,
      aes(yintercept = mu_max),
      linetype = "dashed", color = "grey40", linewidth = 0.5
    ) +
    facet_grid(panel ~ strip_lab, scales = "free_y") +
    scale_color_manual(values = EVO_COLORS, name = NULL) +
    labs(x = "Time (days)", y = NULL) +
    theme(
      strip.background  = element_blank(),
      strip.text.x      = element_text(size = 6.5),
      strip.text.y      = element_text(size = 7, angle = 0, hjust = 0),
      axis.text         = element_text(size = 5),
      axis.title        = element_text(size = 9),
      legend.position   = "bottom",
      panel.spacing.y   = unit(0.4, "lines")
    )

  print(p_pg)
}
dev.off()
cat(sprintf("Saved: mumax-per-well-gcplyr.pdf (%d pages)\n", mu_n_pages))


# ── Per-well AUC diagnostic PDF ──────────────────────────────────────────────
#
# For each well: raw blank-corrected OD (grey points), smoothed OD curve
# (colored line), and the AUC region shaded beneath it. Shows exactly what
# is being integrated and flags wells where the smooth diverges from raw data.

# Recompute smoothed OD for all wells (same parameters as section 4)
auc_plot_data <- od_data |>
  group_by(well, block, test_temperature, strain, evolution_history) |>
  arrange(days, .by_group = TRUE) |>
  mutate(
    blank_od       = min(od, na.rm = TRUE),
    od_corr        = od - blank_od,
    od_smooth      = smooth_data(
      x = days, y = od,
      sm_method = "moving-average", window_width_n = SMOOTH_N
    ),
    od_smooth_corr = pmax(od_smooth - blank_od, 0)  # blank-correct; floor at 0
  ) |>
  ungroup() |>
  left_join(
    gcplyr_metrics |>
      select(well, block, test_temperature, strain, evolution_history, auc_gc),
    by = c("well", "block", "test_temperature", "strain", "evolution_history")
  ) |>
  mutate(
    well_id   = paste(strain, test_temperature, block, well, sep = "_"),
    strip_lab = sprintf("%s | %d\u00b0C B%d [%s]\nAUC=%.3f",
                        strain, test_temperature, block, well, auc_gc)
  )

# Order: evolution history > strain > temperature > block > well
well_order_auc <- auc_plot_data |>
  distinct(well_id, strip_lab, evolution_history, strain, test_temperature, block, well) |>
  arrange(evolution_history, strain, test_temperature, block, well)

auc_plot_data <- auc_plot_data |>
  mutate(strip_lab = factor(strip_lab, levels = unique(well_order_auc$strip_lab)))

# Paginate: 6 columns × 5 rows = 30 wells per page
AUC_N_COLS     <- 6
AUC_N_ROWS     <- 5
AUC_N_PER_PAGE <- AUC_N_COLS * AUC_N_ROWS
auc_all_labs   <- levels(auc_plot_data$strip_lab)
auc_n_pages    <- ceiling(length(auc_all_labs) / AUC_N_PER_PAGE)

pdf(file.path(FIGS, "auc-per-well-gcplyr.pdf"), width = 16, height = 12)
for (pg in seq_len(auc_n_pages)) {
  idx  <- seq((pg - 1) * AUC_N_PER_PAGE + 1, min(pg * AUC_N_PER_PAGE, length(auc_all_labs)))
  labs <- auc_all_labs[idx]

  df_pg <- auc_plot_data |>
    filter(strip_lab %in% labs) |>
    mutate(strip_lab = factor(strip_lab, levels = labs))

  p_pg <- ggplot(df_pg, aes(x = days)) +
    geom_point(
      aes(y = od_corr),
      size = 0.3, alpha = 0.2, color = "grey50"
    ) +
    geom_ribbon(
      aes(ymin = 0, ymax = od_smooth_corr, fill = evolution_history,
          group = well_id),
      alpha = 0.3
    ) +
    geom_line(
      aes(y = od_smooth_corr, color = evolution_history, group = well_id),
      linewidth = 0.6
    ) +
    facet_wrap(~ strip_lab, ncol = AUC_N_COLS, scales = "free_y") +
    scale_color_manual(values = EVO_COLORS, name = NULL) +
    scale_fill_manual(values = EVO_COLORS, name = NULL) +
    labs(x = "Time (days)", y = "OD600 (blank-corrected)") +
    theme(
      strip.background = element_blank(),
      strip.text       = element_text(size = 6),
      axis.text        = element_text(size = 5),
      axis.title       = element_text(size = 9),
      legend.position  = "bottom"
    )

  print(p_pg)
}
dev.off()
cat(sprintf("Saved: auc-per-well-gcplyr.pdf (%d pages)\n", auc_n_pages))


# =============================================================================
# 7. Export
# =============================================================================

write_csv(gcplyr_metrics,   file.path(OUT, "gcplyr-metrics-per-well.csv"))
write_csv(tpc_params_auc,  file.path(OUT, "tpc-params-auc-gcplyr.csv"))
write_csv(tpc_preds_auc,   file.path(OUT, "tpc-predictions-auc-gcplyr.csv"))
write_csv(tpc_params_mumax, file.path(OUT, "tpc-params-mumax-gcplyr.csv"))
write_csv(tpc_preds_mumax,  file.path(OUT, "tpc-predictions-mumax-gcplyr.csv"))
write_csv(results_mumax,    file.path(OUT, "stats-results-mumax-gcplyr.csv"))


