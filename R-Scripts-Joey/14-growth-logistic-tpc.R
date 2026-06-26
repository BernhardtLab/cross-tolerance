# =============================================================================
# 14-growth-logistic-tpc.R
#
# Author: Joey Bernhardt
# Created: June 2026
#
# Estimates growth rates from OD curves using a 3-parameter logistic model,
# then fits Sharpe-Schoolfield High (SSH) thermal performance curves per strain.
#
# Input:  data-processed/all-blocks-tpc-experiment.csv
# Output: data-processed/14-logistic-tpc/
#
# Sections:
#   1. Setup
#   2. Logistic fit function
#   3. SSH model functions (Topt, CTmax, B80)
#   4. Load data
#   5. Fit logistic per well → growth rate estimates
#   6. Fit SSH TPC per strain
#   7. Export
# =============================================================================

# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)
library(minpack.lm)   # nlsLM
library(cowplot)
theme_set(theme_cowplot())

# Paths
DATA_PATH <- "data-processed/all-blocks-tpc-experiment.csv"
OUT       <- "data-processed/14-logistic-tpc"
dir.create(OUT, showWarnings = FALSE)

# Logistic fitting
R2_FLAG   <- 0.90     # Wells below this R² are flagged (not removed)
R_MAX_DAY <- 60       # Upper bound on r (day⁻¹); ~25 min doubling — wells hitting this are flagged

# SSH model constants
K_B    <- 8.617333e-5   # Boltzmann constant (eV K⁻¹)
TREF_K <- 288.15        # Reference temperature: 15°C in Kelvin

# SSH fitting
N_STARTS  <- 500        # Random starts for multistart fitting
CTMAX_CAP <- 50.0       # Fallback CTmax if SSH doesn't cross 5% Rmax threshold

# TPC prediction range
T_PRED <- seq(15, 50, length.out = 500)

# Evolution history groups to retain
TARGET_EVO <- c("35 evolved", "40 evolved", "fRS585")

# Colors
EVO_COLORS <- c(
  "40 evolved" = "#FA3208",
  "35 evolved" = "#0E63FF",
  "fRS585"     = "#000000"
)

set.seed(42)

# =============================================================================
# 2. Logistic fit function
# =============================================================================

# Fits a 4-parameter logistic model to a single well's OD time series:
#
#   OD(t) = A + (K - A) / (1 + exp(-r * (t - t_mid)))
#
# Parameters:
#   A     = lower asymptote (baseline OD at t = -∞)
#   K     = upper asymptote (carrying capacity)
#   r     = intrinsic growth rate (day⁻¹)
#   t_mid = inflection point (days); time of fastest growth
#
# Using 4 parameters (vs 3) allows the curve to start at a non-zero baseline,
# which avoids inflated r estimates when cultures don't begin near OD = 0.
#
# Returns a named vector with:
#   r     = logistic rate parameter (day⁻¹)
#   rk4   = r * (K - A) / 4  (max absolute rate of OD increase, OD·day⁻¹)
#   K     = upper asymptote
#   A     = lower asymptote
#   t_mid = inflection point (days)
#   r2    = R² of the fit
#
# Returns NAs if fit fails (no fallback method).

fit_logistic <- function(days, od, min_points = 6) {

  na_result <- c(r = NA_real_, rk4 = NA_real_, K = NA_real_,
                 A = NA_real_, t_mid = NA_real_, r2 = NA_real_)

  if (length(days) < min_points || sum(!is.na(od)) < min_points) return(na_result)

  days <- as.numeric(days)
  od   <- as.numeric(od)

  OD_max  <- max(od, na.rm = TRUE)
  OD_min  <- min(od, na.rm = TRUE)
  t_range <- diff(range(days))

  # Initial guesses
  A0     <- OD_min                          # baseline = observed minimum OD
  K0     <- OD_max * 1.05
  grad   <- diff(od) / diff(days)
  grad_t <- (head(days, -1) + tail(days, -1)) / 2
  t_mid0 <- grad_t[which.max(grad)]
  r0     <- max(4 * max(grad) / (K0 - A0), 0.05)

  fit_result <- tryCatch({
    fit <- suppressWarnings(nlsLM(
      od ~ A + (K - A) / (1 + exp(-r * (days - t_mid))),
      start   = list(A = A0, K = K0, r = r0, t_mid = t_mid0),
      lower   = c(A = 0,       K = OD_min, r = 1e-3,      t_mid = days[1] - t_range),
      upper   = c(A = OD_min * 2, K = OD_max * 3, r = R_MAX_DAY, t_mid = tail(days, 1) + t_range),
      control = nls.lm.control(maxiter = 5000, maxfev = 20000)
    ))

    A_fit     <- coef(fit)[["A"]]
    K_fit     <- coef(fit)[["K"]]
    r_fit     <- coef(fit)[["r"]]
    t_mid_fit <- coef(fit)[["t_mid"]]

    if (K_fit <= A_fit || r_fit <= 0) stop("Implausible parameters")

    pred   <- predict(fit)
    ss_res <- sum((od - pred)^2)
    ss_tot <- sum((od - mean(od))^2)
    r2     <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_

    c(r     = r_fit,
      rk4   = r_fit * (K_fit - A_fit) / 4,
      K     = K_fit,
      A     = A_fit,
      t_mid = t_mid_fit,
      r2    = r2)

  }, error = function(e) NULL)

  if (is.null(fit_result)) return(na_result)
  fit_result
}

# =============================================================================
# 3. SSH model functions
# =============================================================================

# Sharpe-Schoolfield High model:
#
#   r(T) = r_tref * exp((e/kB)  * (1/Tref - 1/T))
#                / (1 + exp((eh/kB) * (1/th   - 1/T)))
#
# Parameters:
#   r_tref = rate at reference temperature Tref (15°C)
#   e      = activation energy (eV); controls the rising left side
#   eh     = deactivation energy (eV); controls the falling right side
#   th     = temperature of half-deactivation (K)

sharpeschoolhigh <- function(T_c, r_tref, e, eh, th) {
  T_k <- T_c + 273.15
  r_tref *
    exp((e  / K_B) * (1 / TREF_K - 1 / T_k)) /
    (1 + exp((eh / K_B) * (1 / th - 1 / T_k)))
}

# Extract Topt, CTmax, and B80 from SSH parameters.
#
# Topt  = temperature at peak performance
# CTmax = temperature above Topt where performance drops to 5% of Rmax
#         (interpolated zero-crossing if curve reaches zero, otherwise 5% threshold)
# B80   = thermal breadth at 80% of Rmax: width of temperature range
#         where predicted rate >= 0.8 * Rmax

calc_tpc_traits <- function(r_tref, e, eh, th,
                             T_range = seq(0, 55, length.out = 10000)) {

  na_result <- c(topt = NA_real_, ctmax = NA_real_, rmax = NA_real_, b80 = NA_real_)

  preds <- sharpeschoolhigh(T_range, r_tref, e, eh, th)
  valid <- is.finite(preds)
  if (!any(valid)) return(na_result)

  # Topt and Rmax
  topt_idx <- which.max(preds[valid])
  topt     <- T_range[valid][topt_idx]
  rmax     <- preds[valid][topt_idx]

  # CTmax: interpolated crossing above Topt
  # Mask everything at or below Topt so we only look at the descending right side
  preds_right              <- preds
  preds_right[T_range <= topt] <- rmax
  sign_changes             <- which(diff(sign(preds_right)) != 0)

  if (length(sign_changes) > 0) {
    # Curve crosses zero — interpolate
    i     <- sign_changes[1]
    t1    <- T_range[i];       t2 <- T_range[i + 1]
    p1    <- preds_right[i];   p2 <- preds_right[i + 1]
    ctmax <- if (p2 != p1) t1 - p1 * (t2 - t1) / (p2 - p1) else t1
  } else {
    # No zero crossing — use 5% of Rmax as operational CTmax
    below <- which(T_range > topt & preds < 0.05 * rmax)
    ctmax <- if (length(below) > 0) T_range[below[1]] else CTMAX_CAP
  }

  # B80: temperature range where performance >= 80% of Rmax
  above_80    <- T_range[preds >= 0.8 * rmax]
  b80 <- if (length(above_80) >= 2) max(above_80) - min(above_80) else NA_real_

  c(topt = topt, ctmax = ctmax, rmax = rmax, b80 = b80)
}

# =============================================================================
# 4. Load data
# =============================================================================

cat("Loading data...\n")

edata <- read_csv(DATA_PATH, show_col_types = FALSE) |>
  filter(evolution_history %in% TARGET_EVO)

cat(sprintf("  %d rows\n", nrow(edata)))
cat(sprintf("  %d strains\n", n_distinct(edata$strain)))
cat(sprintf("  %d temperatures: %s\n",
            n_distinct(edata$test_temperature),
            paste(sort(unique(edata$test_temperature)), collapse = ", ")))
cat(sprintf("  %d blocks\n", n_distinct(edata$block)))

# =============================================================================
# 5. Fit logistic per well
# =============================================================================

cat("\nFitting 3-parameter logistic to each well...\n")

growth_rates <- edata |>
  group_by(well, block, test_temperature, strain, evolution_history) |>
  arrange(days, .by_group = TRUE) |>
  summarise(
    fit = list(fit_logistic(days, od)),
    .groups = "drop"
  ) |>
  mutate(
    r     = map_dbl(fit, "r"),
    rk4   = map_dbl(fit, "rk4"),
    K     = map_dbl(fit, "K"),
    A     = map_dbl(fit, "A"),
    t_mid = map_dbl(fit, "t_mid"),
    r2    = map_dbl(fit, "r2")
  ) |>
  select(-fit) |>
  mutate(low_r2       = !is.na(r2) & r2 < R2_FLAG,
         fit_fail     = is.na(r),
         neg_t_mid    = !is.na(t_mid) & t_mid < 0,          # inflection before experiment start; r is extrapolated
         r_at_bound   = !is.na(r) & r >= R_MAX_DAY * 0.99)  # r hit the upper bound; estimate is not trustworthy

# Summary
n_total  <- nrow(growth_rates)
n_fail   <- sum(growth_rates$fit_fail)
n_low_r2 <- sum(growth_rates$low_r2, na.rm = TRUE)

n_neg_tmid  <- sum(growth_rates$neg_t_mid,  na.rm = TRUE)
n_at_bound  <- sum(growth_rates$r_at_bound, na.rm = TRUE)

cat(sprintf("  Wells total:        %d\n", n_total))
cat(sprintf("  Fit failures:       %d (%.1f%%)\n", n_fail,     100 * n_fail     / n_total))
cat(sprintf("  Flagged (R²<%.2f):  %d (%.1f%%)\n", R2_FLAG,
            n_low_r2,   100 * n_low_r2   / n_total))
cat(sprintf("  Negative t_mid:     %d (%.1f%%)\n", n_neg_tmid,  100 * n_neg_tmid  / n_total))
cat(sprintf("  r at bound (≥%.0f):  %d (%.1f%%)\n", R_MAX_DAY,
            n_at_bound,  100 * n_at_bound  / n_total))

cat("\nGrowth rate summary by temperature:\n")
growth_rates |>
  group_by(test_temperature) |>
  summarise(
    n          = n(),
    n_failed   = sum(fit_fail),
    n_low_r2   = sum(low_r2, na.rm = TRUE),
    mean_r     = round(mean(r,   na.rm = TRUE), 2),
    mean_rk4   = round(mean(rk4, na.rm = TRUE), 3),
    .groups    = "drop"
  ) |>
  print()

write_csv(growth_rates, file.path(OUT, "growth-rates-per-well.csv"))
cat(sprintf("\nSaved: growth-rates-per-well.csv\n"))

# ── Logistic fit plots (one PNG per well) ------------------------------------

FITS_DIR <- file.path(OUT, "logistic-fits")
dir.create(FITS_DIR, showWarnings = FALSE)

cat(sprintf("\nSaving logistic fit plots to %s/...\n", FITS_DIR))

# Join fitted parameters back to raw OD data
plot_data <- edata |>
  left_join(
    growth_rates |>
      select(well, block, test_temperature, strain, r, K, t_mid, r2, low_r2, fit_fail),
    by = c("well", "block", "test_temperature", "strain")
  )

wells <- growth_rates |>
  distinct(well, block, test_temperature, strain, evolution_history,
           r, K, A, t_mid, r2, low_r2, fit_fail, neg_t_mid, r_at_bound)

for (i in seq_len(nrow(wells))) {
  w <- wells[i, ]

  raw <- plot_data |>
    filter(well            == w$well,
           block           == w$block,
           test_temperature == w$test_temperature,
           strain          == w$strain) |>
    arrange(days)

  # Build fitted curve if parameters exist
  if (!w$fit_fail) {
    t_seq   <- seq(min(raw$days), max(raw$days), length.out = 200)
    od_pred <- w$A + (w$K - w$A) / (1 + exp(-w$r * (t_seq - w$t_mid)))
    curve   <- tibble(days = t_seq, od = od_pred)
  }

  # Title and border color
  r2_label  <- if (!is.na(w$r2)) sprintf("R²=%.3f", w$r2) else "fit failed"
  plot_title <- sprintf("%s | block %s | %s°C | %s",
                        w$strain, w$block, w$test_temperature, w$well)
  subtitle   <- if (!w$fit_fail) {
    flags <- paste0(
      if (w$neg_t_mid)  "  [t_mid<0: r extrapolated]" else "",
      if (w$r_at_bound) "  [r at bound: unreliable]"  else ""
    )
    sprintf("r=%.2f day⁻¹  K=%.3f  t_mid=%.2f  %s%s",
            w$r, w$K, w$t_mid, r2_label, flags)
  } else {
    "fit failed"
  }
  border_col <- if (w$low_r2 || w$fit_fail) "red" else if (w$r_at_bound) "purple" else if (w$neg_t_mid) "orange" else "grey80"

  p <- ggplot(raw, aes(x = days, y = od)) +
    geom_point(size = 1.5, alpha = 0.7) +
    labs(title = plot_title, subtitle = subtitle,
         x = "Days", y = "OD") +
    theme(
      plot.title    = element_text(size = 9),
      plot.subtitle = element_text(size = 8,
                                   color = if (w$low_r2 || w$fit_fail) "red" else "grey30"),
      panel.border  = element_rect(color = border_col, fill = NA, linewidth = 1.5)
    )

  if (!w$fit_fail) {
    p <- p + geom_line(data = curve, aes(x = days, y = od),
                       color = "steelblue", linewidth = 1)
  }

  fname <- sprintf("block%s_%s_%sC_%s.png",
                   w$block, w$strain, w$test_temperature, w$well)
  ggsave(file.path(FITS_DIR, fname), p, width = 4, height = 3, dpi = 150)
}

cat(sprintf("  Saved %d plots\n", nrow(wells)))

# =============================================================================
# 6. Fit SSH TPC per strain
# =============================================================================

# ── Multistart SSH fitter ────────────────────────────────────────────────────
# Fits SSH to vectors of temperatures and rates using N_STARTS random starts.
# Returns list(params, topt, ctmax, rmax, b80, r2); NULL if all starts fail.

fit_ssh_multistart <- function(temps, rates) {

  BOUNDS_LO <- c(r_tref = 1e-3, e = 0.01, eh =  0.5, th = 303.0)
  BOUNDS_HI <- c(r_tref = 50.0, e =  3.0, eh = 50.0, th = 335.0)
  START_LO  <- c(r_tref =  0.5, e =  0.1, eh =  0.5, th = 305.0)
  START_HI  <- c(r_tref = 20.0, e =  2.0, eh = 20.0, th = 325.0)

  starts <- mapply(
    function(a, b, c, d) c(a, b, c, d),
    runif(N_STARTS, START_LO["r_tref"], START_HI["r_tref"]),
    runif(N_STARTS, START_LO["e"],      START_HI["e"]),
    runif(N_STARTS, START_LO["eh"],     START_HI["eh"]),
    runif(N_STARTS, START_LO["th"],     START_HI["th"]),
    SIMPLIFY = FALSE
  )

  best_popt <- NULL
  best_rss  <- Inf

  for (p0 in starts) {
    tryCatch({
      fit <- suppressWarnings(nlsLM(
        rates ~ sharpeschoolhigh(temps, r_tref, e, eh, th),
        start   = list(r_tref = p0[1], e = p0[2], eh = p0[3], th = p0[4]),
        lower   = BOUNDS_LO,
        upper   = BOUNDS_HI,
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
       topt = traits["topt"], ctmax = traits["ctmax"],
       rmax = traits["rmax"], b80  = traits["b80"])
}

# ── Fit for both r and rk4 ───────────────────────────────────────────────────

fit_tpcs <- function(growth_rates, rate_col) {
  cat(sprintf("\nFitting SSH TPCs using '%s'...\n", rate_col))

  d_all <- growth_rates |>
    filter(!fit_fail, !low_r2, !r_at_bound, !is.na(.data[[rate_col]])) |>
    select(strain, evolution_history, test_temperature, all_of(rate_col))

  n_excluded <- sum(growth_rates$fit_fail | growth_rates$low_r2 | growth_rates$r_at_bound, na.rm = TRUE)
  cat(sprintf("  Wells excluded (fit_fail | low_r2 | r_at_bound): %d of %d\n",
              n_excluded, nrow(growth_rates)))

  strains <- d_all |>
    group_by(strain, evolution_history) |>
    summarise(n_temps = n_distinct(test_temperature), .groups = "drop") |>
    filter(n_temps >= 3)

  cat(sprintf("  Strains with ≥3 temperatures: %d\n", nrow(strains)))

  params_list <- vector("list", nrow(strains))
  preds_list  <- vector("list", nrow(strains))

  for (i in seq_len(nrow(strains))) {
    sid <- strains$strain[i]
    evo <- strains$evolution_history[i]
    d   <- d_all |> filter(strain == sid)

    res <- fit_ssh_multistart(d$test_temperature, d[[rate_col]])

    if (is.null(res)) {
      cat(sprintf("  FAILED: %s\n", sid))
      next
    }

    p <- res$params
    params_list[[i]] <- tibble(
      strain            = sid,
      evolution_history = evo,
      rate_metric       = rate_col,
      r_tref = p["r_tref"], e = p["e"], eh = p["eh"], th = p["th"],
      topt  = res$topt,  ctmax = res$ctmax,
      rmax  = res$rmax,  b80   = res$b80,
      r2    = res$r2,    n_obs = nrow(d)
    )

    preds_list[[i]] <- tibble(
      strain            = sid,
      evolution_history = evo,
      rate_metric       = rate_col,
      temp = T_PRED,
      pred = sharpeschoolhigh(T_PRED, p["r_tref"], p["e"], p["eh"], p["th"])
    )

    if (i %% 10 == 0) cat(sprintf("  %d / %d\n", i, nrow(strains)))
  }

  list(params = bind_rows(params_list), preds = bind_rows(preds_list))
}

res_r   <- fit_tpcs(growth_rates, "r")
res_rk4 <- fit_tpcs(growth_rates, "rk4")

tpc_params <- bind_rows(res_r$params, res_rk4$params)
tpc_preds  <- bind_rows(res_r$preds,  res_rk4$preds)

# Summary
cat("\n=== Topt, CTmax, B80 by rate metric and evolution history ===\n")
tpc_params |>
  group_by(rate_metric, evolution_history) |>
  summarise(
    n          = n(),
    mean_topt  = round(mean(topt,  na.rm = TRUE), 2),
    se_topt    = round(sd(topt,    na.rm = TRUE) / sqrt(n()), 2),
    mean_ctmax = round(mean(ctmax, na.rm = TRUE), 2),
    se_ctmax   = round(sd(ctmax,   na.rm = TRUE) / sqrt(n()), 2),
    mean_b80   = round(mean(b80,   na.rm = TRUE), 2),
    .groups    = "drop"
  ) |>
  arrange(rate_metric, evolution_history) |>
  print()

# ── TPC plots ────────────────────────────────────────────────────────────────

make_tpc_plot <- function(rate_col, rate_label) {

  preds   <- tpc_preds  |> filter(rate_metric == rate_col,
                                   evolution_history %in% TARGET_EVO) |>
    mutate(pred = ifelse(pred < 0, NA_real_, pred))

  # Mean SSH curve per evolution history group
  mean_curves <- tpc_params |>
    filter(rate_metric == rate_col, evolution_history %in% TARGET_EVO) |>
    group_by(evolution_history) |>
    summarise(across(c(r_tref, e, eh, th), \(x) mean(x, na.rm = TRUE)),
              .groups = "drop") |>
    rowwise() |>
    mutate(curve = list(tibble(
      temp = T_PRED,
      pred = sharpeschoolhigh(T_PRED, r_tref, e, eh, th)
    ))) |>
    unnest(curve)

  # Observed mean per strain × temperature (for data points)
  obs <- growth_rates |>
    filter(!fit_fail, !is.na(.data[[rate_col]]),
           evolution_history %in% TARGET_EVO) |>
    group_by(strain, evolution_history, test_temperature) |>
    summarise(rate = mean(.data[[rate_col]], na.rm = TRUE), .groups = "drop")

  ggplot() +
    geom_line(
      data = preds,
      aes(x = temp, y = pred, group = strain, color = evolution_history),
      alpha = 0.2, linewidth = 0.8
    ) +
    geom_line(
      data = mean_curves,
      aes(x = temp, y = pred, color = evolution_history),
      linewidth = 2.0
    ) +
    geom_point(
      data = obs,
      aes(x = test_temperature, y = rate, color = evolution_history),
      size = 1.8, alpha = 0.6
    ) +
    scale_color_manual(values = EVO_COLORS, name = NULL) +
    coord_cartesian(xlim = c(23, 48)) +
    labs(
      x     = "Temperature (°C)",
      y     = rate_label,
      title = sprintf("Thermal Performance Curves — %s", rate_label)
    ) +
    theme(
      legend.position      = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background    = element_blank()
    )
}

p_r   <- make_tpc_plot("r",   "r (day⁻¹)")
p_rk4 <- make_tpc_plot("rk4", "r·K/4 (OD·day⁻¹)")

ggsave(file.path(OUT, "tpc-r.png"),   p_r,   width = 9, height = 6, dpi = 300)
ggsave(file.path(OUT, "tpc-rk4.png"), p_rk4, width = 9, height = 6, dpi = 300)
cat("\nSaved: tpc-r.png, tpc-rk4.png\n")

# ── Thermal traits plot ──────────────────────────────────────────────────────

p_traits <- tpc_params |>
  filter(evolution_history %in% TARGET_EVO) |>
  mutate(rate_metric = factor(rate_metric, levels = c("r", "rk4"),
                              labels = c("r (day⁻¹)", "r·K/4"))) |>
  pivot_longer(c(topt, ctmax, b80),
               names_to  = "trait",
               values_to = "value") |>
  mutate(trait = factor(trait,
                        levels = c("topt", "ctmax", "b80"),
                        labels = c("Topt (°C)", "CTmax (°C)", "B80 (°C)"))) |>
  ggplot(aes(x = evolution_history, y = value, color = evolution_history)) +
  geom_jitter(aes(shape = rate_metric), width = 0.12, size = 2.5, alpha = 0.8) +
  stat_summary(aes(group = rate_metric, linetype = rate_metric),
               fun = mean, geom = "crossbar",
               width = 0.35, linewidth = 0.7, fatten = 1.5) +
  scale_color_manual(values = EVO_COLORS, guide = "none") +
  scale_shape_manual(values = c("r (day⁻¹)" = 16, "r·K/4" = 17), name = "Rate metric") +
  scale_linetype_manual(values = c("r (day⁻¹)" = "solid", "r·K/4" = "dashed"), name = "Rate metric") +
  facet_wrap(~ trait, scales = "free_y") +
  labs(x = NULL, y = NULL,
       title = "Thermal traits by evolution history",
       subtitle = "Circles = r (day⁻¹), triangles = r·K/4  |  Crossbar = mean") +
  theme(
    legend.position  = "bottom",
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 12)
  )

ggsave(file.path(OUT, "thermal-traits.png"), p_traits,
       width = 11, height = 5, dpi = 300)
cat("Saved: thermal-traits.png\n")

# ── Per-strain TPC plots (one PNG per strain, two panels: r and rk4) ---------

TPC_DIR <- file.path(OUT, "tpc-per-strain")
dir.create(TPC_DIR, showWarnings = FALSE)
cat(sprintf("\nSaving per-strain TPC plots to %s/...\n", TPC_DIR))

# Observed data points: all individual replicate wells
obs_all <- growth_rates |>
  filter(!fit_fail, evolution_history %in% TARGET_EVO) |>
  select(strain, evolution_history, test_temperature, r, rk4) |>
  pivot_longer(c(r, rk4), names_to = "rate_metric", values_to = "rate") |>
  mutate(rate_metric = factor(rate_metric, levels = c("r", "rk4"),
                              labels = c("r (day⁻¹)", "r·K/4")))

# Predicted curves from SSH fits
preds_all <- tpc_preds |>
  filter(evolution_history %in% TARGET_EVO) |>
  mutate(rate_metric = factor(rate_metric, levels = c("r", "rk4"),
                              labels = c("r (day⁻¹)", "r·K/4")),
         pred = ifelse(pred < 0, NA_real_, pred))

all_strains <- unique(tpc_params$strain)

for (sid in all_strains) {
  evo <- tpc_params$evolution_history[tpc_params$strain == sid][1]

  obs   <- obs_all  |> filter(strain == sid)
  preds <- preds_all |> filter(strain == sid)

  # Topt / CTmax labels per panel
  trait_labels <- tpc_params |>
    filter(strain == sid) |>
    mutate(rate_metric = factor(rate_metric, levels = c("r", "rk4"),
                                labels = c("r (day⁻¹)", "r·K/4")),
           label = sprintf("Topt=%.1f°C  CTmax=%.1f°C  B80=%.1f°C",
                           topt, ctmax, b80))

  p <- ggplot() +
    geom_line(
      data = preds,
      aes(x = temp, y = pred),
      color = EVO_COLORS[evo], linewidth = 1.2
    ) +
    geom_point(
      data = obs,
      aes(x = test_temperature, y = rate),
      color = EVO_COLORS[evo], size = 2, alpha = 0.6
    ) +
    geom_text(
      data = trait_labels,
      aes(label = label),
      x = -Inf, y = Inf, hjust = -0.05, vjust = 1.4,
      size = 3, color = "grey30"
    ) +
    facet_wrap(~ rate_metric, scales = "free_y") +
    coord_cartesian(xlim = c(23, 48)) +
    labs(
      title    = sprintf("%s  (%s)", sid, evo),
      x        = "Temperature (°C)",
      y        = "Growth rate"
    ) +
    theme(
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold"),
      plot.title       = element_text(size = 11)
    )

  ggsave(file.path(TPC_DIR, sprintf("%s.png", sid)),
         p, width = 8, height = 4, dpi = 200)
}

cat(sprintf("  Saved %d strain plots\n", length(all_strains)))

# =============================================================================
# 7. Export
# =============================================================================

# TPC parameters per strain (both rate metrics)
write_csv(tpc_params, file.path(OUT, "tpc-params-per-strain.csv"))

# TPC predictions (for re-plotting without re-fitting)
write_csv(tpc_preds, file.path(OUT, "tpc-predictions.csv"))

cat("\nAll outputs saved to", OUT, "\n")
cat("Files:\n")
for (f in sort(list.files(OUT, recursive = FALSE))) {
  cat(sprintf("  %s\n", f))
}
cat(sprintf("  logistic-fits/  (%d well plots)\n", nrow(wells)))
cat("\nDone.\n")
