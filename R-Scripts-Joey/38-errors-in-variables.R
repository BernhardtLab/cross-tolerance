# =============================================================================
# 38-errors-in-variables.R
# IC50 vs ancestor-window mean growth, accounting for measurement error and
# separating the treatments.
#
# Both variables are estimates:
#   y = strain mean of plate-level log2 IC50 ratios (vs same-plate ancestor).
#       Error variance = var(plate values) / n_plates.
#   x = mean fitted growth over the ancestor window (39.4-42.2 C).
#       Error from the TPC bootstrap (Script 19): pointwise 95% bands in
#       tpc-ci-ribbon-19.csv give SE(t) = (ci_hi - ci_lo) / (2 * 1.96).
#       The window mean's SE is the mean SE over the window's temperatures.
#       This is a conservative bound, since it treats errors across
#       temperatures as fully correlated.
#
# The ancestor window is the same for every strain, so the reference curve's
# own error does not add variation between strains.
#
# Analyses (within each treatment, so no between-treatment comparison):
#   1. OLS slope, 95% CI and r (reference).
#   2. Reliability ratio lambda = 1 - mean(var_x) / var(x); attenuation-corrected
#      OLS slope = slope / lambda (approximate).
#   3. SMA (= RMA) slope, sign(r) * sd(y) / sd(x); slope CI by resampling strains
#      (2000 bootstrap replicates). SMA/RMA do not correct for measurement error.
#   4. Check: mean of the bootstrap ribbon over the window equals
#      mean_fixed_window from Script 32.
#
# Limits: the strain-specific window metrics also depend on T_opt and T_max
# errors, which this script does not propagate. The ancestor-window metric is
# used because its error comes only from the curve bands.
#
# Inputs:  data-processed/normalised-ic50-per-plate.csv
#          data-processed/gcplyr/tpc-ci-ribbon-19.csv
#          data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv
#          data-processed/gcplyr/tpc-predictions-auc-gcplyr-17.csv
# Outputs: data-processed/errors-in-variables-38.csv
# =============================================================================

library(tidyverse)

OUT <- "data-processed"
EVO_LEVELS <- c("35 evolved", "40 evolved")
DRUG_LABELS <- c(fluconazole = "Fluconazole", caspofungin = "Caspofungin",
                 amphotericin = "Amphotericin B")
N_BOOT <- 2000
set.seed(4826)


# =============================================================================
# 1. Strain means, their error variance, and the window mean with its SE
# =============================================================================

ic50_plate <- read_csv(file.path(OUT, "normalised-ic50-per-plate.csv"), show_col_types = FALSE) |>
  filter(evolution_history %in% EVO_LEVELS)

y_dat <- ic50_plate |>
  group_by(strain = population, evolution_history, drug) |>
  summarise(y = mean(log_ratio), var_y = var(log_ratio) / n(), n_plates = n(), .groups = "drop")

stopifnot(min(y_dat$n_plates) >= 2)

tpc_par  <- read_csv(file.path(OUT, "gcplyr/tpc-params-auc-gcplyr-17.csv"), show_col_types = FALSE)
tpc_pred <- read_csv(file.path(OUT, "gcplyr/tpc-predictions-auc-gcplyr-17.csv"), show_col_types = FALSE)
ribbon   <- read_csv(file.path(OUT, "gcplyr/tpc-ci-ribbon-19.csv"), show_col_types = FALSE)

anc <- tpc_par |> filter(evolution_history == "fRS585") |> select(topt, tmax)
stopifnot(nrow(anc) == 1)

# Window mean from the ancestor-window fitted curve, and its SE from the bootstrap bands
x_dat <- ribbon |>
  filter(evolution_history %in% EVO_LEVELS,
         temperature >= anc$topt, temperature <= anc$tmax) |>
  mutate(se_t = (ci_hi - ci_lo) / (2 * 1.96)) |>
  group_by(strain, evolution_history) |>
  summarise(x = mean(fitted), se_x = mean(se_t), .groups = "drop") |>
  mutate(var_x = se_x^2)

# Sanity check: the ribbon's fitted values reproduce Script 32's mean_fixed_window
mfw_ref <- tpc_pred |>
  filter(evolution_history %in% EVO_LEVELS) |>
  group_by(strain, evolution_history) |>
  summarise(mfw = {
    x <- seq(anc$topt, anc$tmax, length.out = 2001)
    mean(approx(temp, pred, xout = x)$y)
  }, .groups = "drop")

cat("Max |ribbon mean - pred mean| over window:",
    format(x_dat |> inner_join(mfw_ref, by = c("strain", "evolution_history")) |>
             summarise(max(abs(x - mfw))) |> pull(), digits = 3), "\n")

dat <- y_dat |>
  inner_join(x_dat |> select(strain, evolution_history, x, se_x, var_x),
             by = c("strain", "evolution_history"))

stopifnot(nrow(dat) == 36 * 3)


# =============================================================================
# 2. Within-treatment estimates
# =============================================================================

# Standardized major axis (SMA) = reduced major axis (RMA) slope:
# sign(r) * sd(y) / sd(x). Symmetric in x and y; it does not use error variances,
# so it is not an attenuation correction.
sma_slope <- function(x, y) {
  sign(cor(x, y)) * sd(y) / sd(x)
}

within_est <- function(d) {
  m  <- lm(y ~ x, data = d)
  co <- summary(m)$coefficients["x", ]
  ci <- confint(m)["x", ]
  lambda <- 1 - mean(d$var_x) / var(d$x)
  b_sma  <- sma_slope(d$x, d$y)
  boot_b <- replicate(N_BOOT, {
    s <- d[sample(nrow(d), replace = TRUE), ]
    sma_slope(s$x, s$y)
  })
  tibble(
    n            = nrow(d),
    ols_slope    = co[["Estimate"]],
    ols_lo       = ci[[1]],
    ols_hi       = ci[[2]],
    ols_r        = cor(d$x, d$y),
    ols_p        = co[["Pr(>|t|)"]],
    reliability  = lambda,
    slope_corr   = if_else(lambda > 0, co[["Estimate"]] / lambda, NA_real_),
    sma_slope    = b_sma,
    sma_lo       = unname(quantile(boot_b, 0.025)),
    sma_hi       = unname(quantile(boot_b, 0.975))
  )
}

results <- dat |>
  group_by(drug, evolution_history) |>
  group_modify(\(d, k) within_est(d)) |>
  ungroup() |>
  mutate(drug = DRUG_LABELS[drug])

print(results |> mutate(across(where(is.double), \(v) round(v, 3))), n = Inf)

# Pooled Deming across treatments (for reference only; treatments barely overlap in x)
pooled <- dat |>
  group_by(drug) |>
  group_modify(\(d, k) {
    tibble(sma_pooled = sma_slope(d$x, d$y))
  }) |>
  ungroup()

print(pooled |> mutate(across(where(is.double), \(v) round(v, 3))))

# =============================================================================
# 4. Strain-specific window metrics with propagated endpoint errors
# =============================================================================
# Each strain's window is [T_opt, T_max] from its own fit. The bootstrap gives
# SE(T_opt) and SE(T_max) (Script 19, tpc-boot-se-19.csv). Delta method:
#   M = I / W,  I = integral of f over [lo, hi],  W = hi - lo
#   dI/dhi = f(hi),   dI/dlo = -f(lo)
#   dM/dhi = (f(hi) - M) / W,   dM/dlo = -(f(lo) - M) / W
# Curve error over the window: mean band SE (fully correlated bound), as before.
# Endpoint errors are treated as independent (the bootstrap covariance is not saved).

boot_se <- read_csv(file.path(OUT, "gcplyr/tpc-boot-se-19.csv"), show_col_types = FALSE) |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(strain, evolution_history, se_topt, se_tmax)

end_dat <- tpc_par |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(strain, evolution_history, topt, tmax) |>
  inner_join(boot_se, by = c("strain", "evolution_history"))

stopifnot(nrow(end_dat) == 36)

strain_metrics <- pmap_dfr(
  list(end_dat$strain, end_dat$evolution_history, end_dat$topt, end_dat$tmax,
       end_dat$se_topt, end_dat$se_tmax),
  \(s, g, lo, hi, sl, sh) {
    cv <- tpc_pred |> filter(strain == s) |> arrange(temp)
    W  <- hi - lo
    I  <- integrate_window(cv$temp, cv$pred, lo, hi)
    M  <- I / W
    f_lo <- approx(cv$temp, cv$pred, xout = lo)$y
    f_hi <- approx(cv$temp, cv$pred, xout = hi)$y
    se_curve <- ribbon |>
      filter(strain == s, temperature >= lo, temperature <= hi) |>
      mutate(se_t = (ci_hi - ci_lo) / (2 * 1.96)) |>
      summarise(v = mean(se_t)) |> pull(v)
    se_M <- sqrt(((-(f_lo - M) / W) * sl)^2 + (((f_hi - M) / W) * sh)^2 + se_curve^2)
    se_I <- sqrt((-f_lo * sl)^2 + (f_hi * sh)^2 + (se_curve * W)^2)
    tibble(strain = s, evolution_history = g,
           mean_above_topt = M, se_mean_above_topt = se_M,
           auc_above_topt = I, se_auc_above_topt = se_I)
  })

strain_dat2 <- y_dat |> inner_join(strain_metrics, by = c("strain", "evolution_history"))
stopifnot(nrow(strain_dat2) == 36 * 3)

specific_results <- map_dfr(c("mean_above_topt", "auc_above_topt"), \(m) {
  se_col <- paste0("se_", m)
  strain_dat2 |>
    mutate(x = .data[[m]], var_x = .data[[se_col]]^2) |>
    group_by(drug, evolution_history) |>
    group_modify(\(d, k) within_est(d)) |>
    ungroup() |>
    mutate(metric = m, .before = 1)
}) |>
  mutate(drug = DRUG_LABELS[drug])

cat("\nStrain-specific window metrics, within treatment:\n")
print(specific_results |>
        transmute(metric, drug, evolution_history,
                  ols = sprintf("%.2f [%.2f, %.2f]", ols_slope, ols_lo, ols_hi),
                  ols_r = round(ols_r, 2), ols_p = round(ols_p, 3),
                  reliab = round(reliability, 2), attn_corr = round(slope_corr, 2)),
      n = Inf)

cat("\nMean SE of strain-specific metrics vs between-strain SD:\n")
print(strain_dat2 |>
        group_by(evolution_history) |>
        summarise(across(c(mean_above_topt, se_mean_above_topt, auc_above_topt, se_auc_above_topt),
                         \(v) round(mean(v), 3)), .groups = "drop"))

write_csv(bind_rows(results |> mutate(metric = "mean_fixed_window"), specific_results),
          file.path(OUT, "errors-in-variables-38.csv"))
