# =============================================================================
# 40-tpc-window-bootstrap-york.R
# Replicate-based x error for the window metrics, and York regression using it.
#
# Why: script 39 estimated the x error as the mean of the bootstrap band widths
# over the window, which treats errors at different temperatures as fully
# correlated and is probably too large. Here the error is the SD of each
# bootstrap replicate's own window mean, i.e. the sampling variability of the
# metric itself.
#
# Steps
#   1. Residual bootstrap of each evolved strain's SSH TPC, as in Script 19
#      (same model, bounds, refit and validity rules), starting from the
#      point-estimate parameters saved by Script 17. 1000 valid replicates per
#      strain are kept, not discarded.
#   2. Per replicate: ancestor-window mean (fixed window, 39.4-42.2 C), and the
#      strain's own-window mean and AUC (T_opt to T_max of that replicate).
#   3. x error = SD of the replicate values (fixed window, and own-window metrics).
#   4. York regression within each drug x treatment, with the same y error as
#      Script 39 (pooled plate SD / sqrt(plates)), using the new x error.
#   5. Comparison with the Script 39 York results (which are left unchanged).
#
# Outputs (new; existing outputs are not overwritten):
#   data-processed/tpc-window-boot-40.csv        per-strain replicate summaries
#   data-processed/york-regression-40.csv        York with replicate-based x error
#   data-processed/york-comparison-40.csv        Script 39 vs Script 40 x error
#   figures/york-scatter-40.png
# =============================================================================

library(tidyverse)
library(minpack.lm)

set.seed(5531)

OUT  <- "data-processed"
FIGS <- "figures"
EVO_LEVELS  <- c("35 evolved", "40 evolved")
EVO_COLORS  <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF")
DRUG_LABELS <- c(fluconazole = "Fluconazole", caspofungin = "Caspofungin",
                 amphotericin = "Amphotericin B")

# SSH constants and bounds (identical to Script 19)
K_B      <- 8.617333e-5
TREF_K   <- 288.15
TMAX_CAP <- 50.0
BOUNDS_LO <- c(r_tref = 1e-6, e = 0.01, eh =  0.5, th = 303.0)
BOUNDS_HI <- c(r_tref = 10.0, e =  3.0, eh = 50.0, th = 335.0)
START_LO  <- c(r_tref = 0.01, e =  0.1, eh =  0.5, th = 305.0)
START_HI  <- c(r_tref =  2.0, e =  2.0, eh = 20.0, th = 325.0)
N_BOOT        <- 1000
N_STARTS_BOOT <- 20


# =============================================================================
# 1. SSH model and bootstrap helpers (as in Script 19)
# =============================================================================

sharpeschoolhigh <- function(T_c, r_tref, e, eh, th) {
  T_k <- T_c + 273.15
  r_tref *
    exp((e / K_B) * (1 / TREF_K - 1 / T_k)) /
    (1 + exp((eh / K_B) * (1 / th - 1 / T_k)))
}

calc_tpc_traits <- function(r_tref, e, eh, th,
                            T_range = seq(0, 55, length.out = 10000)) {
  na_result <- c(topt = NA_real_, tmax = NA_real_, rmax = NA_real_, b80 = NA_real_)
  preds <- sharpeschoolhigh(T_range, r_tref, e, eh, th)
  if (!any(is.finite(preds))) return(na_result)
  opt <- optim(
    par    = T_range[which.max(preds)],
    fn     = function(T) -sharpeschoolhigh(T, r_tref, e, eh, th),
    method = "Brent", lower = min(T_range), upper = max(T_range)
  )
  topt <- opt$par
  rmax <- -opt$value
  thresh_fn <- function(T) sharpeschoolhigh(T, r_tref, e, eh, th) - 0.05 * rmax
  tmax <- tryCatch(
    uniroot(thresh_fn, lower = topt, upper = max(T_range), tol = 1e-8)$root,
    error = function(e) TMAX_CAP
  )
  above_80 <- T_range[preds >= 0.8 * rmax]
  b80 <- if (length(above_80) >= 2) max(above_80) - min(above_80) else NA_real_
  c(topt = topt, tmax = tmax, rmax = rmax, b80 = b80)
}

fit_ssh_boot_refit <- function(temps, rates, init_params) {
  all_starts <- c(
    list(init_params),
    lapply(seq_len(N_STARTS_BOOT), function(i)
      c(runif(1, START_LO["r_tref"], START_HI["r_tref"]),
        runif(1, START_LO["e"],      START_HI["e"]),
        runif(1, START_LO["eh"],     START_HI["eh"]),
        runif(1, START_LO["th"],     START_HI["th"]))
    )
  )
  best_popt <- NULL
  best_rss  <- Inf
  for (p0 in all_starts) {
    tryCatch({
      fit <- suppressWarnings(nlsLM(
        rates ~ sharpeschoolhigh(temps, r_tref, e, eh, th),
        start   = list(r_tref = p0[1], e = p0[2], eh = p0[3], th = p0[4]),
        lower   = BOUNDS_LO, upper = BOUNDS_HI,
        control = nls.lm.control(maxiter = 200, maxfev = 5000)
      ))
      rss <- sum(residuals(fit)^2)
      if (rss < best_rss) { best_rss <- rss; best_popt <- coef(fit) }
    }, error = function(e) NULL)
    if (!is.null(best_popt) && identical(p0, init_params) && best_rss < 1e-8) break
  }
  best_popt
}

params_at_bound <- function(params, lo, hi, rel_tol = 1e-3) {
  if (any(!is.finite(params))) return(TRUE)
  range_w <- hi - lo
  any((params - lo) / range_w < rel_tol | (hi - params) / range_w < rel_tol)
}

boot_residual <- function(fitted_vals, resids, refit_fn, R = N_BOOT, max_iter = 10L * R) {
  first_result <- NULL
  samples <- vector("list", R)
  n_valid <- 0L
  n_iter  <- 0L
  while (n_valid < R && n_iter < max_iter) {
    n_iter <- n_iter + 1L
    boot_y <- fitted_vals + sample(resids, size = length(resids), replace = TRUE)
    result <- refit_fn(boot_y)
    if (!is.null(result) && all(is.finite(result))) {
      n_valid <- n_valid + 1L
      samples[[n_valid]] <- result
      if (is.null(first_result)) first_result <- result
    }
  }
  n_stats <- if (!is.null(first_result)) length(first_result) else 0L
  stat_nm <- if (!is.null(first_result)) names(first_result) else character(0L)
  mat <- matrix(NA_real_, nrow = R, ncol = n_stats, dimnames = list(NULL, stat_nm))
  if (n_valid > 0)
    mat[seq_len(n_valid), ] <- matrix(unlist(samples[seq_len(n_valid)]),
                                      nrow = n_valid, ncol = n_stats, byrow = TRUE)
  list(samples = mat, n_valid = n_valid, n_total_iter = n_iter)
}

# Mean of an SSH curve over [lo, hi] on a fine grid
curve_mean <- function(lo, hi, r_tref, e, eh, th, n = 501) {
  g <- seq(lo, hi, length.out = n)
  mean(sharpeschoolhigh(g, r_tref, e, eh, th))
}


# =============================================================================
# 2. Point estimates and data
# =============================================================================

tpc_par <- read_csv(file.path(OUT, "gcplyr/tpc-params-auc-gcplyr-17.csv"), show_col_types = FALSE)
tpc_pred <- read_csv(file.path(OUT, "gcplyr/tpc-predictions-auc-gcplyr-17.csv"), show_col_types = FALSE)
auc_data <- read_csv(file.path(OUT, "gcplyr/gcplyr-metrics-per-well-17.csv"), show_col_types = FALSE) |>
  filter(evolution_history %in% c(EVO_LEVELS, "fRS585"), !is.na(auc_gc), is.finite(auc_gc))

anc_par <- tpc_par |> filter(evolution_history == "fRS585")
stopifnot(nrow(anc_par) == 1)
anc_topt <- anc_par$topt
anc_tmax <- anc_par$tmax

evo_par <- tpc_par |> filter(evolution_history %in% EVO_LEVELS)
stopifnot(nrow(evo_par) == 36)


# =============================================================================
# 3. Residual bootstrap per strain, keeping every valid replicate
# =============================================================================

boot_rows <- vector("list", nrow(evo_par))

for (i in seq_len(nrow(evo_par))) {
  sid <- evo_par$strain[i]
  d   <- auc_data |> filter(strain == sid)
  p   <- c(r_tref = evo_par$r_tref[i], e = evo_par$e[i],
           eh = evo_par$eh[i], th = evo_par$th[i])

  fitted <- sharpeschoolhigh(d$test_temperature, p["r_tref"], p["e"], p["eh"], p["th"])
  resids <- d$auc_gc - fitted

  refit_fn <- (function(temps_, p_, blo_, bhi_) {
    function(boot_y) {
      popt <- fit_ssh_boot_refit(temps_, boot_y, p_)
      if (is.null(popt)) return(NULL)
      if (params_at_bound(popt[names(blo_)], blo_, bhi_)) return(NULL)
      tr <- calc_tpc_traits(popt["r_tref"], popt["e"], popt["eh"], popt["th"])
      if (any(is.na(tr))) return(NULL)
      c(tr, r_tref = unname(popt["r_tref"]), e = unname(popt["e"]),
        eh = unname(popt["eh"]), th = unname(popt["th"]))
    }
  })(d$test_temperature, p, BOUNDS_LO, BOUNDS_HI)

  br <- boot_residual(fitted, resids, refit_fn)
  s  <- br$samples[seq_len(br$n_valid), , drop = FALSE]

  fixed_b   <- apply(s, 1, \(r) curve_mean(anc_topt, anc_tmax,
                                           r["r_tref"], r["e"], r["eh"], r["th"]))
  own_mean_b <- apply(s, 1, \(r) curve_mean(r["topt"], r["tmax"],
                                            r["r_tref"], r["e"], r["eh"], r["th"]))
  own_auc_b  <- own_mean_b * (s[, "tmax"] - s[, "topt"])

  boot_rows[[i]] <- tibble(
    strain = sid, evolution_history = evo_par$evolution_history[i],
    n_valid = br$n_valid, n_total_iter = br$n_total_iter,
    fixed_mean = mean(fixed_b), se_fixed = sd(fixed_b),
    own_mean = mean(own_mean_b), se_own_mean = sd(own_mean_b),
    own_auc = mean(own_auc_b), se_own_auc = sd(own_auc_b)
  )

  cat(sprintf("%2d / %d %s: %d valid replicates (%d iterations)\n",
              i, nrow(evo_par), sid, br$n_valid, br$n_total_iter))
}

boot_summary <- bind_rows(boot_rows)
write_csv(boot_summary, file.path(OUT, "tpc-window-boot-40.csv"))


# =============================================================================
# 4. Point-estimate x values (as in Scripts 37 and 39) and the two x-error versions
# =============================================================================

integrate_window <- function(temp, pred, lo, hi) {
  x <- sort(unique(c(lo, hi, temp[temp > lo & temp < hi])))
  y <- approx(temp, pred, xout = x)$y
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

x_point <- tpc_pred |>
  filter(evolution_history %in% EVO_LEVELS) |>
  group_by(strain, evolution_history) |>
  summarise(x = integrate_window(temp, pred, anc_topt, anc_tmax) / (anc_tmax - anc_topt),
            .groups = "drop")

# Script 39 x error, for comparison: mean of bootstrap band widths over the window
ribbon <- read_csv(file.path(OUT, "gcplyr/tpc-ci-ribbon-19.csv"), show_col_types = FALSE)
x_se_old <- ribbon |>
  filter(evolution_history %in% EVO_LEVELS,
         temperature >= anc_topt, temperature <= anc_tmax) |>
  mutate(se_t = (ci_hi - ci_lo) / (2 * 1.96)) |>
  group_by(strain, evolution_history) |>
  summarise(sx_old = mean(se_t), .groups = "drop")

x_dat <- x_point |>
  inner_join(boot_summary |> select(strain, evolution_history, se_fixed, n_valid),
             by = c("strain", "evolution_history")) |>
  inner_join(x_se_old, by = c("strain", "evolution_history")) |>
  mutate(sx = se_fixed)

cat(sprintf("\nMedian x error: old (band-width bound) %.3f, new (replicate SD) %.3f\n",
            median(x_dat$sx_old), median(x_dat$sx)))


# =============================================================================
# 5. y data (as in Script 39: pooled plate SD / sqrt(plates))
# =============================================================================

ic50_plate <- read_csv(file.path(OUT, "normalised-ic50-per-plate.csv"), show_col_types = FALSE) |>
  filter(evolution_history %in% EVO_LEVELS)

pooled_sd <- ic50_plate |>
  group_by(population, drug) |>
  mutate(dev = log_ratio - mean(log_ratio)) |>
  ungroup() |>
  group_by(drug) |>
  summarise(pool_sd = sqrt(sum(dev^2) / (n() - n_distinct(population))), .groups = "drop")

y_dat <- ic50_plate |>
  group_by(strain = population, evolution_history, drug) |>
  summarise(y = mean(log_ratio), n_plates = n(), .groups = "drop") |>
  left_join(pooled_sd, by = "drug") |>
  mutate(sy = pool_sd / sqrt(n_plates))

dat <- y_dat |> inner_join(x_dat, by = c("strain", "evolution_history"))
stopifnot(nrow(dat) == 36 * 3)


# =============================================================================
# 6. York regression (Script 39 algorithm), with both x-error versions
# =============================================================================

york_fit <- function(x, y, sx, sy, tol = 1e-12, maxit = 1000) {
  wx <- 1 / sx^2
  wy <- 1 / sy^2
  b  <- cov(x, y) / var(x)
  for (i in seq_len(maxit)) {
    W    <- wx * wy / (wx + b^2 * wy)
    xbar <- sum(W * x) / sum(W)
    ybar <- sum(W * y) / sum(W)
    U    <- x - xbar
    V    <- y - ybar
    beta <- W * (U / wy + b * V / wx)
    b_new <- sum(W * beta * V) / sum(W * beta * U)
    if (abs(b_new - b) < tol) { b <- b_new; break }
    b <- b_new
  }
  W    <- wx * wy / (wx + b^2 * wy)
  xbar <- sum(W * x) / sum(W)
  U    <- x - xbar
  se_b <- sqrt(1 / sum(W * U^2))
  tcrit <- qt(0.975, df = length(x) - 2)
  tibble(slope = b, se = se_b, lo = b - tcrit * se_b, hi = b + tcrit * se_b,
         intercept = sum(W * y) / sum(W) - b * xbar,
         pivot_x = xbar, pivot_y = sum(W * y) / sum(W), iterations = i)
}

york_run <- function(d, sx_col) {
  yk <- york_fit(d$x, d$y, d[[sx_col]], d$sy)
  tibble(york_slope = yk$slope, york_lo = yk$lo, york_hi = yk$hi,
         york_intercept = yk$intercept, york_pivot_x = yk$pivot_x,
         york_pivot_y = yk$pivot_y, iterations = yk$iterations)
}

york_new <- dat |>
  group_by(drug, evolution_history) |>
  group_modify(\(d, k) york_run(d, "sx")) |>
  ungroup() |>
  mutate(x_error = "replicate SD (Script 40)")

york_old <- dat |>
  group_by(drug, evolution_history) |>
  group_modify(\(d, k) york_run(d, "sx_old")) |>
  ungroup() |>
  mutate(x_error = "band-width bound (Script 39)")

results40 <- bind_rows(york_new, york_old) |>
  mutate(drug = DRUG_LABELS[drug]) |>
  arrange(drug, evolution_history, x_error)

comparison <- results40 |>
  transmute(drug, evolution_history, x_error,
            york = sprintf("%.2f [%.2f, %.2f]", york_slope, york_lo, york_hi))

cat("\nYork slope [analytic 95% CI] by x-error version:\n")
print(comparison, n = Inf)

cat("\nReplicate-based x error by group (median across strains):\n")
print(dat |>
        group_by(drug, evolution_history) |>
        summarise(sx_old = round(median(sx_old), 3), sx_new = round(median(sx), 3),
                  sd_x = round(sd(x), 3), .groups = "drop"))

write_csv(results40, file.path(OUT, "york-regression-40.csv"))
write_csv(comparison, file.path(OUT, "york-comparison-40.csv"))


# =============================================================================
# 7. Figure: scatter with York lines and the replicate-based x error bars
# =============================================================================

rng <- dat |>
  group_by(drug, evolution_history) |>
  summarise(x1 = min(x), x2 = max(x), .groups = "drop")

band_dat <- results40 |>
  filter(x_error == "replicate SD (Script 40)") |>
  left_join(rng |> mutate(drug = DRUG_LABELS[drug]), by = c("drug", "evolution_history")) |>
  rowwise() |>
  reframe(
    drug = drug, evolution_history = evolution_history,
    x = seq(x1, x2, length.out = 60),
    y = york_pivot_y + york_slope * (x - york_pivot_x),
    lo = y - (york_hi - york_lo) / 2 * abs(x - york_pivot_x),
    hi = y + (york_hi - york_lo) / 2 * abs(x - york_pivot_x)
  ) |>
  ungroup()

seg40 <- results40 |>
  filter(x_error == "replicate SD (Script 40)") |>
  left_join(rng |> mutate(drug = DRUG_LABELS[drug]), by = c("drug", "evolution_history")) |>
  transmute(drug, evolution_history, x1, x2,
            y1 = york_intercept + york_slope * x1,
            y2 = york_intercept + york_slope * x2)

ggplot(dat |> mutate(drug = DRUG_LABELS[drug]),
       aes(x = x, y = y, color = evolution_history)) +
  geom_ribbon(data = band_dat,
              aes(x = x, ymin = lo, ymax = hi, fill = evolution_history,
                  group = evolution_history),
              alpha = 0.2, color = NA, inherit.aes = FALSE) +
  geom_errorbar(aes(ymin = y - sy, ymax = y + sy), width = 0, alpha = 0.35) +
  geom_errorbar(aes(xmin = x - sx, xmax = x + sx), orientation = "y", width = 0, alpha = 0.35) +
  geom_point(size = 2) +
  geom_segment(data = seg40,
               aes(x = x1, xend = x2, y = y1, yend = y2, color = evolution_history),
               linewidth = 0.8, inherit.aes = FALSE) +
  facet_wrap(~ drug, scales = "free") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_fill_manual(values = EVO_COLORS, guide = "none") +
  labs(
    x = "Ancestor-window mean growth (error bars: SE across TPC bootstrap replicates)",
    y = expression(paste(log[2], " IC"[50], " / same-plate ancestor (error bars: SE of strain mean)")),
    caption = paste(
      "Lines: York regression within each treatment. Shaded: 95% CI of the slope (analytic York SE).",
      "x error = SE of each strain's ancestor-window mean, from the SD of its bootstrap replicates (Script 40).",
      "y error = pooled plate SD / sqrt(plates).",
      sep = "\n")
  ) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), legend.position = "bottom",
        plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "york-scatter-40.png"), width = 10, height = 4.2, dpi = 300, bg = "white")
