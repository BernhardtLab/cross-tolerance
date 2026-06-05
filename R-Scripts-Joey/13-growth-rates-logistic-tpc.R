# =============================================================================
# 13-growth-rates-logistic-tpc.R
#
# Author: Joey Bernhardt
# Created: June 2026
#
# Pipeline:
#   1. Load raw OD data from all-blocks-tpc-experiment.csv
#   2. Fit 3-parameter logistic model to each well → growth rate (mu, day⁻¹)
#   3. Flag low-quality fits (R² < R2_FLAG_THRESHOLD) rather than removing them
#   4. Fit Sharpe-Schoolfield High (SSH) TPC per strain, using all replicate
#      wells as individual data points (not pre-averaged)
#   5. Extract Topt and CTmax; export results and figures
#
# Input:  data-processed/all-blocks-tpc-experiment.csv
# Output: data-processed/logistic-tpc/
# =============================================================================

library(tidyverse)
library(minpack.lm)   # nlsLM for robust nonlinear fitting
library(cowplot)
theme_set(theme_cowplot())

# =============================================================================
# Configuration
# =============================================================================

DATA_PATH       <- "data-processed/all-blocks-tpc-experiment.csv"
OUT             <- "data-processed/logistic-tpc"
dir.create(OUT, showWarnings = FALSE)

R2_FLAG_THRESHOLD <- 0.95   # Wells below this R² are flagged (not removed)
N_STARTS          <- 500    # Random starts for SSH multistart fitting
CTMAX_CAP         <- 50.0   # Fallback CTmax if SSH doesn't cross threshold
T_PRED            <- seq(15, 50, length.out = 500)  # Temperatures for TPC predictions

# Boltzmann constant and reference temperature for SSH model
K_B    <- 8.617333e-5   # eV K⁻¹
TREF_K <- 288.15        # 15°C in Kelvin

set.seed(42)

# Evolution history groups to carry through analysis
TARGET_EVO <- c("35 evolved", "40 evolved", "fRS585")

EVO_COLORS <- c(
  "40 evolved" = "#FA3208",
  "35 evolved" = "#0E63FF",
  "fRS585"     = "#000000"
)

# =============================================================================
# Helper functions
# =============================================================================

# ── 3-parameter logistic fit ─────────────────────────────────────────────────
#
# Model: OD(t) = K / (1 + exp(-r * (t - t_mid)))
#
# mu (growth rate) is returned as r in day⁻¹ — the logistic rate parameter,
# which equals the specific growth rate at the inflection point × 2, and is
# the standard quantity used in TPC analyses.
#
# Returns a named vector: mu, r2, K, t_mid (all NA on failure)

fit_logistic <- function(days, od, min_points = 6) {

  logistic3 <- function(t, K, r, t_mid) K / (1 + exp(-r * (t - t_mid)))

  n <- length(days)
  if (n < min_points || all(is.na(od))) {
    return(c(mu = NA_real_, r2 = NA_real_, K = NA_real_, t_mid = NA_real_))
  }

  od      <- as.numeric(od)
  days    <- as.numeric(days)
  OD_max  <- max(od, na.rm = TRUE)
  OD_min  <- min(od, na.rm = TRUE)
  t_range <- tail(days, 1) - days[1]

  # Initial guesses from numerical gradient
  grad   <- diff(od) / diff(days)
  grad_t <- (head(days, -1) + tail(days, -1)) / 2
  K0     <- OD_max * 1.05
  t_mid0 <- grad_t[which.max(grad)]
  r0     <- max(4 * max(grad) / K0, 0.05)

  result <- tryCatch({
    fit <- nlsLM(
      od ~ logistic3(days, K, r, t_mid),
      start   = list(K = K0, r = r0, t_mid = t_mid0),
      lower   = c(K = OD_min, r = 1e-3, t_mid = days[1] - t_range),
      upper   = c(K = OD_max * 3, r = 50.0, t_mid = tail(days, 1) + t_range),
      control = nls.lm.control(maxiter = 200, maxfev = 10000)
    )
    K_fit     <- coef(fit)[["K"]]
    r_fit     <- coef(fit)[["r"]]
    t_mid_fit <- coef(fit)[["t_mid"]]

    if (K_fit < OD_min || r_fit <= 0) stop("Implausible parameters")

    pred   <- predict(fit)
    ss_res <- sum((od - pred)^2)
    ss_tot <- sum((od - mean(od))^2)
    r2     <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_

    c(mu = r_fit, r2 = r2, K = K_fit, t_mid = t_mid_fit)
  }, error = function(e) NULL)

  if (!is.null(result)) return(result)

  # Fallback: return NAs (don't silently substitute a different method)
  c(mu = NA_real_, r2 = NA_real_, K = NA_real_, t_mid = NA_real_)
}


# ── Sharpe-Schoolfield High model ─────────────────────────────────────────────
#
# r(T) = r_tref * exp((e/kB) * (1/Tref - 1/T)) / (1 + exp((eh/kB) * (1/th - 1/T)))
#
# T_c:   temperature in °C
# r_tref: rate at reference temperature (15°C)
# e:     activation energy (eV)
# eh:    deactivation energy (eV)
# th:    temperature at half-deactivation (K)

sharpeschoolhigh <- function(T_c, r_tref, e, eh, th) {
  T_k         <- T_c + 273.15
  boltz_act   <- (e  / K_B) * (1 / TREF_K - 1 / T_k)
  boltz_deact <- (eh / K_B) * (1 / th     - 1 / T_k)
  r_tref * exp(boltz_act) / (1 + exp(boltz_deact))
}


# ── Extract Topt and CTmax from SSH parameters ─────────────────────────────

calc_topt_ctmax <- function(r_tref, e, eh, th,
                             T_range = seq(5, 55, length.out = 5000)) {
  preds <- sharpeschoolhigh(T_range, r_tref, e, eh, th)
  valid <- is.finite(preds)
  if (!any(valid)) return(c(topt = NA_real_, ctmax = NA_real_, rmax = NA_real_))

  topt_idx <- which.max(preds[valid])
  topt     <- T_range[valid][topt_idx]
  rmax     <- preds[valid][topt_idx]

  # CTmax: linear interpolation of zero-crossing above Topt
  # (for SSH this is rare; fallback is 5% rmax threshold)
  pa               <- preds
  pa[T_range <= topt] <- rmax
  sign_chg         <- which(diff(sign(pa)) != 0)

  if (length(sign_chg) > 0) {
    i     <- sign_chg[1]
    t1    <- T_range[i];   t2 <- T_range[i + 1]
    p1    <- pa[i];        p2 <- pa[i + 1]
    ctmax <- if (p2 != p1) t1 - p1 * (t2 - t1) / (p2 - p1) else t1
  } else {
    below <- which(T_range > topt & preds < 0.05 * rmax)
    ctmax <- if (length(below) > 0) T_range[below[1]] else CTMAX_CAP
  }

  c(topt = topt, ctmax = ctmax, rmax = rmax)
}


# ── SSH multistart fitter ─────────────────────────────────────────────────────
#
# Fits SSH to a vector of temperatures and rates using N_STARTS random starts.
# Returns a list with params, topt, ctmax, rmax, r2; NULL if all starts fail.

fit_ssh_multistart <- function(temps, rates) {

  # Parameter bounds
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
      if (rss < best_rss) {
        best_rss  <- rss
        best_popt <- coef(fit)
      }
    }, error = function(e) NULL)
  }

  if (is.null(best_popt)) return(NULL)

  ss_tot <- sum((rates - mean(rates))^2)
  r2     <- if (ss_tot > 0) 1 - best_rss / ss_tot else NA_real_

  derived <- calc_topt_ctmax(best_popt["r_tref"], best_popt["e"],
                              best_popt["eh"],     best_popt["th"])

  list(params = best_popt,
       topt   = derived["topt"],
       ctmax  = derived["ctmax"],
       rmax   = derived["rmax"],
       r2     = r2)
}


# =============================================================================
# 1. Load data
# =============================================================================

cat("Loading data...\n")

edata <- read_csv(DATA_PATH, show_col_types = FALSE) |>
  filter(evolution_history %in% TARGET_EVO)

cat(sprintf("  %d rows, %d unique wells\n",
            nrow(edata),
            n_distinct(paste(edata$well, edata$block, edata$test_temperature))))


# =============================================================================
# 2. Fit 3-parameter logistic per well
# =============================================================================

cat("\nFitting logistic model to each well...\n")

growth_rates <- edata |>
  group_by(well, block, test_temperature, strain, evolution_history) |>
  arrange(days, .by_group = TRUE) |>
  summarise(
    fit = list(fit_logistic(days, od)),
    .groups = "drop"
  ) |>
  mutate(
    mu    = map_dbl(fit, "mu"),
    r2    = map_dbl(fit, "r2"),
    K     = map_dbl(fit, "K"),
    t_mid = map_dbl(fit, "t_mid")
  ) |>
  select(-fit) |>
  mutate(
    low_r2   = !is.na(r2) & r2 < R2_FLAG_THRESHOLD,
    fit_fail = is.na(mu)
  )

n_total   <- nrow(growth_rates)
n_fail    <- sum(growth_rates$fit_fail)
n_low_r2  <- sum(growth_rates$low_r2, na.rm = TRUE)

cat(sprintf("  Wells fit:        %d\n", n_total))
cat(sprintf("  Fit failures:     %d (%.1f%%)\n", n_fail,  100 * n_fail  / n_total))
cat(sprintf("  Low R² (< %.2f): %d (%.1f%%)\n", R2_FLAG_THRESHOLD,
            n_low_r2, 100 * n_low_r2 / n_total))

# Growth rate summary by temperature
cat("\nGrowth rate summary by temperature (all fits including flagged):\n")
growth_rates |>
  group_by(test_temperature) |>
  summarise(
    n        = n(),
    n_failed = sum(fit_fail),
    n_low_r2 = sum(low_r2, na.rm = TRUE),
    mean_mu  = round(mean(mu, na.rm = TRUE), 3),
    sd_mu    = round(sd(mu,   na.rm = TRUE), 3),
    .groups  = "drop"
  ) |>
  print()

write_csv(growth_rates, file.path(OUT, "growth-rates-per-well.csv"))
cat(sprintf("\nSaved: %s/growth-rates-per-well.csv\n", OUT))


# =============================================================================
# 3. Fit SSH TPC per strain (all replicate wells as individual data points)
# =============================================================================

cat("\n=== Fitting Sharpe-Schoolfield High TPC per strain ===\n")
cat("Using all replicate wells as individual data points (not pre-averaged)\n\n")

# Prepare: one row per well, with temp and mu
# Exclude failed fits; keep flagged fits (they'll contribute as-is)
gr_for_tpc <- growth_rates |>
  filter(!fit_fail) |>
  select(strain, evolution_history, test_temperature, well, block, mu, r2, low_r2)

strains <- gr_for_tpc |>
  group_by(strain, evolution_history) |>
  summarise(
    n_obs   = sum(!is.na(mu)),
    n_temps = n_distinct(test_temperature[!is.na(mu)]),
    .groups = "drop"
  ) |>
  filter(n_temps >= 3)   # Need at least 3 temperatures for SSH

cat(sprintf("Strains with ≥3 temperature points: %d\n\n", nrow(strains)))

tpc_params_list <- vector("list", nrow(strains))
tpc_preds_list  <- vector("list", nrow(strains))

for (i in seq_len(nrow(strains))) {
  sid  <- strains$strain[i]
  evo  <- strains$evolution_history[i]

  d      <- gr_for_tpc |> filter(strain == sid, !is.na(mu))
  temps  <- d$test_temperature
  rates  <- d$mu

  result <- fit_ssh_multistart(temps, rates)

  if (is.null(result)) {
    cat(sprintf("  FAILED: %s\n", sid))
    next
  }

  tpc_params_list[[i]] <- tibble(
    strain          = sid,
    evolution_history = evo,
    r_tref          = result$params["r_tref"],
    e               = result$params["e"],
    eh              = result$params["eh"],
    th              = result$params["th"],
    topt            = result$topt,
    ctmax           = result$ctmax,
    rmax            = result$rmax,
    r2_tpc          = result$r2,
    n_obs           = nrow(d)
  )

  tpc_preds_list[[i]] <- tibble(
    strain            = sid,
    evolution_history = evo,
    temp              = T_PRED,
    pred              = sharpeschoolhigh(
      T_PRED,
      result$params["r_tref"], result$params["e"],
      result$params["eh"],     result$params["th"]
    )
  )

  if (i %% 10 == 0 || i == nrow(strains)) {
    cat(sprintf("  %d / %d strains done\n", i, nrow(strains)))
  }
}

tpc_params <- bind_rows(tpc_params_list)
tpc_preds  <- bind_rows(tpc_preds_list)

cat(sprintf("\nSuccessfully fit: %d / %d strains\n", nrow(tpc_params), nrow(strains)))


# =============================================================================
# 4. Summary statistics
# =============================================================================

cat("\n=== Topt and CTmax by evolution history ===\n")

tpc_params |>
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
# 5. Figures
# =============================================================================

cat("\nGenerating figures...\n")

# ── Fig 1: TPC overlay (individual strains + mean curves) ─────────────────

indiv_curves <- tpc_preds |>
  filter(evolution_history %in% TARGET_EVO) |>
  mutate(pred = ifelse(pred < 0, NA_real_, pred))

mean_params <- tpc_params |>
  filter(evolution_history %in% TARGET_EVO) |>
  group_by(evolution_history) |>
  summarise(across(c(r_tref, e, eh, th), mean, na.rm = TRUE), .groups = "drop")

mean_curves <- mean_params |>
  rowwise() |>
  mutate(curve = list(tibble(
    temp = T_PRED,
    pred = sharpeschoolhigh(T_PRED, r_tref, e, eh, th)
  ))) |>
  unnest(curve)

# Observed mean growth rates per strain × temperature (for plotting points)
obs_means <- gr_for_tpc |>
  filter(evolution_history %in% TARGET_EVO) |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(mu = mean(mu, na.rm = TRUE), .groups = "drop")

p_tpc <- ggplot() +
  geom_line(
    data = indiv_curves,
    aes(x = temp, y = pred, group = strain, color = evolution_history),
    alpha = 0.2, linewidth = 0.9
  ) +
  geom_line(
    data = mean_curves,
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 2.2
  ) +
  geom_point(
    data = obs_means,
    aes(x = test_temperature, y = mu, color = evolution_history),
    size = 2, alpha = 0.6
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  coord_cartesian(xlim = c(23, 48), ylim = c(0, NA)) +
  labs(
    x     = "Temperature (°C)",
    y     = "Growth Rate (day⁻¹)",
    title = "Thermal Performance Curves — Logistic Estimation"
  ) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background    = element_blank()
  )

ggsave(file.path(OUT, "01-tpc-overlay.png"), p_tpc,
       width = 9, height = 6, dpi = 300)


# ── Fig 2: Topt by evolution history ──────────────────────────────────────

p_topt <- tpc_params |>
  filter(evolution_history %in% TARGET_EVO) |>
  ggplot(aes(x = evolution_history, y = topt, color = evolution_history)) +
  geom_jitter(width = 0.08, size = 2.5, alpha = 0.8) +
  stat_summary(fun = mean, geom = "crossbar",
               width = 0.35, linewidth = 0.8, fatten = 2) +
  scale_color_manual(values = EVO_COLORS) +
  labs(x = NULL, y = "Topt (°C)", title = "Thermal Optimum") +
  theme(legend.position = "none")

ggsave(file.path(OUT, "02-topt.png"), p_topt,
       width = 5, height = 5, dpi = 300)


# ── Fig 3: CTmax by evolution history ─────────────────────────────────────

p_ctmax <- tpc_params |>
  filter(evolution_history %in% TARGET_EVO) |>
  ggplot(aes(x = evolution_history, y = ctmax, color = evolution_history)) +
  geom_jitter(width = 0.08, size = 2.5, alpha = 0.8) +
  stat_summary(fun = mean, geom = "crossbar",
               width = 0.35, linewidth = 0.8, fatten = 2) +
  scale_color_manual(values = EVO_COLORS) +
  labs(x = NULL, y = "CTmax (°C)", title = "Critical Thermal Maximum") +
  theme(legend.position = "none")

ggsave(file.path(OUT, "03-ctmax.png"), p_ctmax,
       width = 5, height = 5, dpi = 300)


# ── Fig 4: R² distribution of logistic fits ───────────────────────────────

p_r2 <- growth_rates |>
  filter(!fit_fail) |>
  ggplot(aes(x = r2, fill = low_r2)) +
  geom_histogram(binwidth = 0.01, boundary = 0) +
  geom_vline(xintercept = R2_FLAG_THRESHOLD, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "tomato"),
                    labels = c("FALSE" = "Good fit", "TRUE" = "Low R²"),
                    name   = NULL) +
  facet_wrap(~ test_temperature, labeller = label_both) +
  labs(
    x     = "R² (logistic fit)",
    y     = "Count",
    title = sprintf("Logistic fit quality (flagged: R² < %.2f)", R2_FLAG_THRESHOLD)
  )

ggsave(file.path(OUT, "04-r2-distribution.png"), p_r2,
       width = 10, height = 6, dpi = 300)

cat("  Figures saved to", OUT, "\n")


# =============================================================================
# 6. Export
# =============================================================================

write_csv(tpc_params, file.path(OUT, "tpc-params-per-strain.csv"))
write_csv(tpc_preds,  file.path(OUT, "tpc-predictions.csv"))

cat("\nOutputs:\n")
for (f in sort(list.files(OUT))) cat(sprintf("  %s\n", f))
cat("\nDone.\n")
