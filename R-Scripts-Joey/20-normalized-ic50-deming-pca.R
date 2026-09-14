# =============================================================================
# 20-normalized-ic50-deming-pca.R
#
# Deming regressions and PCA using plate-normalized IC50s (log-ratios from
# script 11d), with bootstrap SEs estimated by plate-level resampling.
#
# Bootstrap approach: per strain × drug, the per-plate log-ratio values
# (log(strain IC50 / ancestor IC50) from script 11d) are treated as
# observations and resampled with replacement N_BOOT times. SE = SD of
# bootstrap means. Strains with only 1 matched plate retain a point estimate
# but no SE and are excluded from Deming regression.
#
# Deming regression: th_c (TPC deactivation temperature, bootstrap SE from
#   script 19) vs mean log-ratio (plate-level bootstrap SE from this script).
#   y-axis is log-ratio rather than absolute log(IC50) because the
#   plate-level normalization already removes batch effects, and the
#   scientific question is whether higher Th predicts cross-tolerance
#   relative to the ancestral baseline.
#   Pooled, per-group, and within-group-centered versions all fitted.
#
# PCA: evolved strains only (ancestor log-ratio = 0 by definition, carries
#   no information). Variables: topt, tmax, th_c, b80 + log-ratio for
#   fluconazole, caspofungin, amphotericin. Pooled and within-group-centered.
#
# Inputs:
#   data-processed/normalised-ic50-per-plate.csv   — script 11d
#   data-processed/gcplyr/tpc-boot-se-19.csv       — script 19
#
# Outputs (data-processed/):
#   norm-ic50-boot-20.csv
#   deming-norm-pooled-20.csv          — th_c, pooled
#   deming-norm-by-group-20.csv        — th_c, per evolution history
#   deming-norm-within-20.csv          — th_c, within-group centered
#   deming-norm-tmax-pooled-20.csv     — tmax, pooled
#   deming-norm-tmax-by-group-20.csv   — tmax, per evolution history
#   deming-norm-tmax-within-20.csv     — tmax, within-group centered
#   deming-norm-dtmax-pooled-20.csv    — ΔTmax vs ancestor, pooled
#   deming-norm-dtmax-by-group-20.csv  — ΔTmax vs ancestor, per evolution history
#   deming-norm-dtmax-within-20.csv    — ΔTmax vs ancestor, within-group centered
#   ols-norm-dtmax-by-group-20.csv     — OLS ΔTmax by group
#   deming-norm-perm-20.csv            — permutation p-values (th_c + tmax)
#   pca-scores-20.csv
#   pca-loadings-20.csv
#   pca-scores-within-20.csv
#   pca-loadings-within-20.csv
#
# Outputs (figures/):
#   deming-norm-20.png                 — th_c pooled
#   deming-norm-within-20.png          — th_c within-group centered
#   deming-norm-tmax-20.png            — tmax pooled
#   deming-norm-tmax-within-20.png     — tmax within-group centered
#   deming-norm-dtmax-20.png           — ΔTmax relative to ancestor, pooled
#   pca-biplot-20.png
#   pca-biplot-within-20.png
# =============================================================================


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)
library(deming)
library(quantreg)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")

theme_evo <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(
      panel.background = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA),
      plot.background  = element_rect(fill = "transparent", colour = NA)
    )
}

N_BOOT        <- 1000   # bootstrap iterations for per-strain SE (section 3)
N_PERM_DEMING <- 5000   # permutation iterations for within-group Deming (section 5e)
set.seed(7241)

p_stars <- function(p) case_when(
  p < 0.001 ~ "***",
  p < 0.01  ~ "**",
  p < 0.05  ~ "*",
  TRUE      ~ "ns"
)


# =============================================================================
# 2. Load data
# =============================================================================

plate_norm <- read_csv("data-processed/normalised-ic50-per-plate.csv",
                       show_col_types = FALSE)

tpc_se <- read_csv("data-processed/gcplyr/tpc-boot-se-19.csv",
                   show_col_types = FALSE)

cat("Plate-norm rows:", nrow(plate_norm), "\n")
cat("TPC strains:", nrow(tpc_se), "\n")


# =============================================================================
# 3. Bootstrap per-strain mean log-ratio (plate-level resampling)
# =============================================================================

norm_boot <- plate_norm |>
  group_by(population, evolution_history, drug) |>
  group_modify(function(d, key) {
    n <- nrow(d)
    if (n < 2) {
      return(tibble(
        mean_log_ratio = mean(d$log_ratio),
        se_log_ratio   = NA_real_,
        n_plates       = n
      ))
    }
    boot_means <- replicate(
      N_BOOT,
      mean(sample(d$log_ratio, size = n, replace = TRUE))
    )
    tibble(
      mean_log_ratio = mean(d$log_ratio),
      se_log_ratio   = sd(boot_means),
      n_plates       = n
    )
  }) |>
  ungroup()

cat("\nPer-strain bootstrap complete.\n")
norm_boot |>
  count(drug, evolution_history, is.na(se_log_ratio)) |>
  rename(se_is_na = `is.na(se_log_ratio)`) |>
  print()


# =============================================================================
# 4. Join TPC and IC50 data for Deming regression
# =============================================================================
# Strains with NA se_log_ratio (only 1 plate) are excluded from Deming fits.

deming_dat <- tpc_se |>
  select(strain, evolution_history, th_c, se_th, tmax, se_tmax) |>
  inner_join(
    norm_boot |>
      filter(evolution_history %in% c("35 evolved", "40 evolved")) |>
      select(population, drug, mean_log_ratio, se_log_ratio),
    by = c("strain" = "population")
  ) |>
  filter(!is.na(se_log_ratio), !is.na(se_th), !is.na(se_tmax))

cat(sprintf(
  "\nDeming input: %d strain × drug obs across %d strains and %d drugs\n",
  nrow(deming_dat),
  n_distinct(deming_dat$strain),
  n_distinct(deming_dat$drug)
))


# =============================================================================
# 5. Deming regression: th_c vs mean log-ratio
# =============================================================================

fit_deming <- function(d, x, y, xstd, ystd) {
  tryCatch(
    deming(
      reformulate(x, y),
      data = d,
      xstd = d[[xstd]],
      ystd = d[[ystd]]
    ),
    error = function(e) { message("  Deming failed: ", e$message); NULL }
  )
}

summarise_deming <- function(fit, slope_var, n) {
  if (is.null(fit)) return(tibble())
  cf <- coef(fit)
  se <- fit$se
  t_s <- cf[slope_var] / se[2]
  tibble(
    n            = n,
    intercept    = cf["(Intercept)"],
    se_intercept = se[1],
    slope        = cf[slope_var],
    se_slope     = se[2],
    t_slope      = t_s,
    p_slope      = 2 * pt(abs(t_s), df = n - 2, lower.tail = FALSE)
  )
}

# ── 5a. Pooled across evolution histories ─────────────────────────────────────

deming_pooled <- deming_dat |>
  group_by(drug) |>
  group_modify(function(d, key) {
    if (nrow(d) < 5) return(tibble())
    fit <- fit_deming(d, "th_c", "mean_log_ratio", "se_th", "se_log_ratio")
    summarise_deming(fit, "th_c", nrow(d))
  }) |>
  ungroup()

cat("\n=== Deming: th_c vs log-ratio (pooled) ===\n")
print(deming_pooled |> select(drug, n, slope, se_slope, t_slope, p_slope))

# ── 5b. Per evolution history ─────────────────────────────────────────────────

deming_by_group <- deming_dat |>
  group_by(drug, evolution_history) |>
  group_modify(function(d, key) {
    if (nrow(d) < 4) return(tibble())
    fit <- fit_deming(d, "th_c", "mean_log_ratio", "se_th", "se_log_ratio")
    summarise_deming(fit, "th_c", nrow(d))
  }) |>
  ungroup()

cat("\n=== Deming: th_c vs log-ratio (by evolution history) ===\n")
print(deming_by_group |> select(drug, evolution_history, n, slope, se_slope, p_slope))

# ── 5c. Within-group centered Deming ─────────────────────────────────────────

deming_centered <- deming_dat |>
  group_by(drug, evolution_history) |>
  mutate(
    th_c_c      = th_c          - mean(th_c),
    log_ratio_c = mean_log_ratio - mean(mean_log_ratio)
  ) |>
  ungroup()

deming_within <- deming_centered |>
  group_by(drug) |>
  group_modify(function(d, key) {
    if (nrow(d) < 5) return(tibble())
    fit <- fit_deming(d, "th_c_c", "log_ratio_c", "se_th", "se_log_ratio")
    summarise_deming(fit, "th_c_c", nrow(d))
  }) |>
  ungroup()

cat("\n=== Deming: within-group centered ===\n")
print(deming_within |> select(drug, n, slope, se_slope, t_slope, p_slope))


# =============================================================================
# 5d. Deming regression: tmax vs mean log-ratio
# =============================================================================

tmax_centered <- deming_dat |>
  group_by(drug, evolution_history) |>
  mutate(
    tmax_c      = tmax         - mean(tmax),
    log_ratio_c = mean_log_ratio - mean(mean_log_ratio)
  ) |>
  ungroup()

# ── Pooled ────────────────────────────────────────────────────────────────────

deming_pooled_tmax <- deming_dat |>
  group_by(drug) |>
  group_modify(function(d, key) {
    if (nrow(d) < 5) return(tibble())
    fit <- fit_deming(d, "tmax", "mean_log_ratio", "se_tmax", "se_log_ratio")
    summarise_deming(fit, "tmax", nrow(d))
  }) |>
  ungroup()

cat("\n=== Deming: tmax vs log-ratio (pooled) ===\n")
print(deming_pooled_tmax |> select(drug, n, slope, se_slope, t_slope, p_slope))

# ── Per evolution history ──────────────────────────────────────────────────────

deming_by_group_tmax <- deming_dat |>
  group_by(drug, evolution_history) |>
  group_modify(function(d, key) {
    if (nrow(d) < 4) return(tibble())
    fit <- fit_deming(d, "tmax", "mean_log_ratio", "se_tmax", "se_log_ratio")
    summarise_deming(fit, "tmax", nrow(d))
  }) |>
  ungroup()

cat("\n=== Deming: tmax vs log-ratio (by evolution history) ===\n")
print(deming_by_group_tmax |> select(drug, evolution_history, n, slope, se_slope, p_slope))

# ── OLS by evolution history (more stable than Deming at n = 18) ──────────────
# Deming overcorrects for attenuation when error is large relative to signal;
# OLS gives honest effect sizes and standard errors here.

ols_tmax_by_group <- deming_dat |>
  group_by(drug, evolution_history) |>
  group_modify(function(d, key) {
    fit <- lm(mean_log_ratio ~ tmax, data = d)
    s   <- summary(fit)$coefficients
    tibble(
      n        = nrow(d),
      r        = cor(d$tmax, d$mean_log_ratio),
      slope    = s["tmax", "Estimate"],
      se_slope = s["tmax", "Std. Error"],
      t_slope  = s["tmax", "t value"],
      p_slope  = s["tmax", "Pr(>|t|)"]
    )
  }) |>
  ungroup()

cat("\n=== OLS: tmax vs log-ratio (by evolution history) ===\n")
print(ols_tmax_by_group |> mutate(across(where(is.numeric), ~round(., 3))),
      n = Inf, width = Inf)

# ── Within-group centered ──────────────────────────────────────────────────────

deming_within_tmax <- tmax_centered |>
  group_by(drug) |>
  group_modify(function(d, key) {
    if (nrow(d) < 5) return(tibble())
    fit <- fit_deming(d, "tmax_c", "log_ratio_c", "se_tmax", "se_log_ratio")
    summarise_deming(fit, "tmax_c", nrow(d))
  }) |>
  ungroup()

cat("\n=== Deming: tmax within-group centered ===\n")
print(deming_within_tmax |> select(drug, n, slope, se_slope, t_slope, p_slope))


# =============================================================================
# 5f. Deming regression: ΔTmax (relative to ancestor) vs log-ratio
# =============================================================================
# Both axes are now referenced to the ancestor fRS585:
#   x = strain tmax − ancestor tmax  (°C)
#   y = log(strain IC50 / ancestor IC50)
# SE on x propagated from both strain and ancestor TPC bootstrap SEs:
#   SE(ΔTmax) = sqrt(se_tmax_strain² + se_tmax_ancestor²)

anc_tmax    <- tpc_se |> filter(evolution_history == "fRS585") |> pull(tmax)
anc_se_tmax <- tpc_se |> filter(evolution_history == "fRS585") |> pull(se_tmax)

delta_tmax_dat <- deming_dat |>
  mutate(
    delta_tmax    = tmax - anc_tmax,
    se_delta_tmax = sqrt(se_tmax^2 + anc_se_tmax^2)
  )

# ── Pooled ────────────────────────────────────────────────────────────────────

deming_pooled_dtmax <- delta_tmax_dat |>
  group_by(drug) |>
  group_modify(function(d, key) {
    if (nrow(d) < 5) return(tibble())
    fit <- fit_deming(d, "delta_tmax", "mean_log_ratio", "se_delta_tmax", "se_log_ratio")
    summarise_deming(fit, "delta_tmax", nrow(d))
  }) |>
  ungroup()

cat("=== Deming: ΔTmax vs log-ratio (pooled) ===\n")
print(deming_pooled_dtmax |> select(drug, n, slope, se_slope, t_slope, p_slope))

# ── Per evolution history ──────────────────────────────────────────────────────

deming_by_group_dtmax <- delta_tmax_dat |>
  group_by(drug, evolution_history) |>
  group_modify(function(d, key) {
    if (nrow(d) < 4) return(tibble())
    fit <- fit_deming(d, "delta_tmax", "mean_log_ratio", "se_delta_tmax", "se_log_ratio")
    summarise_deming(fit, "delta_tmax", nrow(d))
  }) |>
  ungroup()

cat("\n=== Deming: ΔTmax vs log-ratio (by evolution history) ===\n")
print(deming_by_group_dtmax |>
        select(drug, evolution_history, n, slope, se_slope, t_slope, p_slope) |>
        mutate(across(where(is.numeric), ~round(., 3))),
      n = Inf, width = Inf)

# ── OLS by evolution history ───────────────────────────────────────────────────

ols_dtmax_by_group <- delta_tmax_dat |>
  group_by(drug, evolution_history) |>
  group_modify(function(d, key) {
    fit <- lm(mean_log_ratio ~ delta_tmax, data = d)
    s   <- summary(fit)$coefficients
    tibble(
      n        = nrow(d),
      r        = cor(d$delta_tmax, d$mean_log_ratio),
      slope    = s["delta_tmax", "Estimate"],
      se_slope = s["delta_tmax", "Std. Error"],
      t_slope  = s["delta_tmax", "t value"],
      p_slope  = s["delta_tmax", "Pr(>|t|)"]
    )
  }) |>
  ungroup()

cat("\n=== OLS: ΔTmax vs log-ratio (by evolution history) ===\n")
print(ols_dtmax_by_group |> mutate(across(where(is.numeric), ~round(., 3))),
      n = Inf, width = Inf)

# ── Within-group centered Deming ───────────────────────────────────────────────

dtmax_centered <- delta_tmax_dat |>
  group_by(drug, evolution_history) |>
  mutate(
    delta_tmax_c = delta_tmax    - mean(delta_tmax),
    log_ratio_c  = mean_log_ratio - mean(mean_log_ratio)
  ) |>
  ungroup()

deming_within_dtmax <- dtmax_centered |>
  group_by(drug) |>
  group_modify(function(d, key) {
    if (nrow(d) < 5) return(tibble())
    fit <- fit_deming(d, "delta_tmax_c", "log_ratio_c", "se_delta_tmax", "se_log_ratio")
    summarise_deming(fit, "delta_tmax_c", nrow(d))
  }) |>
  ungroup()

cat("\n=== Deming: ΔTmax within-group centered ===\n")
print(deming_within_dtmax |> select(drug, n, slope, se_slope, t_slope, p_slope))


# =============================================================================
# 5e. Permutation tests — within-group centered Deming (th_c and tmax)
# =============================================================================
# Permute log_ratio_c within each evolution_history group (preserving
# group-mean = 0 structure) and refit Deming N_PERM times to build a null
# distribution of slopes. Two-sided p-value = proportion of |null slopes|
# >= |observed slope|.
#
# N_PERM = 5000 gives p-value resolution to 0.0002 with good stability.

perm_deming_slope <- function(centered_dat, x_var, x_se, y_var = "log_ratio_c",
                              y_se = "se_log_ratio", n_perm = N_PERM_DEMING) {
  centered_dat |>
    group_by(drug) |>
    group_modify(function(d, key) {
      obs_fit <- tryCatch(
        deming(reformulate(x_var, y_var), data = d,
               xstd = d[[x_se]], ystd = d[[y_se]]),
        error = function(e) NULL
      )
      if (is.null(obs_fit)) return(tibble())
      obs_slope <- coef(obs_fit)[x_var]

      null_slopes <- vapply(seq_len(n_perm), function(i) {
        d_perm <- d |>
          group_by(evolution_history) |>
          mutate(!!y_var := sample(.data[[y_var]])) |>
          ungroup()
        fit_p <- tryCatch(
          deming(reformulate(x_var, y_var), data = d_perm,
                 xstd = d_perm[[x_se]], ystd = d_perm[[y_se]]),
          error = function(e) NULL
        )
        if (is.null(fit_p)) NA_real_ else coef(fit_p)[x_var]
      }, numeric(1))

      null_slopes <- null_slopes[!is.na(null_slopes)]
      tibble(
        predictor    = x_var,
        obs_slope    = obs_slope,
        p_perm       = mean(abs(null_slopes) >= abs(obs_slope)),
        n_perm_valid = length(null_slopes)
      )
    }) |>
    ungroup()
}

cat("\nRunning permutation tests — this will take a few minutes...\n")

perm_th_c <- perm_deming_slope(deming_centered, x_var = "th_c_c", x_se = "se_th")
perm_tmax <- perm_deming_slope(tmax_centered,   x_var = "tmax_c", x_se = "se_tmax")

perm_results <- bind_rows(perm_th_c, perm_tmax)

cat("\n=== Permutation p-values: within-group centered Deming ===\n")
perm_results |>
  left_join(
    bind_rows(
      deming_within      |> select(drug, slope) |> mutate(predictor = "th_c_c"),
      deming_within_tmax |> select(drug, slope) |> mutate(predictor = "tmax_c")
    ),
    by = c("drug", "predictor")
  ) |>
  mutate(
    obs_slope    = round(obs_slope, 3),
    p_perm       = round(p_perm, 4)
  ) |>
  select(predictor, drug, obs_slope, p_perm, n_perm_valid) |>
  print(n = Inf)


# =============================================================================
# 6. Deming plots
# =============================================================================

make_deming_line <- function(results, dat, x_var, y_hat_var,
                             slope_col = "slope", intercept_col = "intercept",
                             has_intercept = TRUE) {
  results |>
    inner_join(
      dat |>
        group_by(drug) |>
        summarise(x_min = min(.data[[x_var]]), x_max = max(.data[[x_var]]),
                  .groups = "drop"),
      by = "drug"
    ) |>
    group_by(drug) |>
    reframe(
      x      = seq(unique(x_min), unique(x_max), length.out = 200),
      y_hat  = if (has_intercept)
        unique(.data[[intercept_col]]) + unique(.data[[slope_col]]) * x
      else
        unique(.data[[slope_col]]) * x
    ) |>
    rename(!!x_var := x, !!y_hat_var := y_hat)
}

clip_lines <- function(lines, dat, x_var, y_hat_var, y_var, se_var) {
  y_range <- dat |>
    group_by(drug) |>
    summarise(
      ylo = min(.data[[y_var]] - .data[[se_var]], na.rm = TRUE),
      yhi = max(.data[[y_var]] + .data[[se_var]], na.rm = TRUE),
      .groups = "drop"
    )
  lines |>
    left_join(y_range, by = "drug") |>
    filter(.data[[y_hat_var]] >= ylo, .data[[y_hat_var]] <= yhi)
}

label_position <- function(results, dat, x_var, y_var, se_var,
                           slope_col = "slope", se_col = "se_slope") {
  results |>
    mutate(
      ci_lo = .data[[slope_col]] - 1.96 * .data[[se_col]],
      ci_hi = .data[[slope_col]] + 1.96 * .data[[se_col]],
      label = sprintf("slope = %.2f\n95%% CI [%.2f, %.2f]",
                      .data[[slope_col]], ci_lo, ci_hi)
    ) |>
    left_join(
      dat |>
        group_by(drug) |>
        summarise(
          x_pos = min(.data[[x_var]] - 0) + 0.02 * diff(range(.data[[x_var]])),
          y_pos = max(.data[[y_var]] + .data[[se_var]], na.rm = TRUE),
          .groups = "drop"
        ),
      by = "drug"
    )
}

# ── 6a. Pooled Deming ─────────────────────────────────────────────────────────

pooled_lines <- make_deming_line(
  deming_pooled, deming_dat,
  x_var = "th_c", y_hat_var = "log_ratio_hat"
) |>
  clip_lines(deming_dat, "th_c", "log_ratio_hat", "mean_log_ratio", "se_log_ratio")

pooled_labels <- label_position(
  deming_pooled, deming_dat, "th_c", "mean_log_ratio", "se_log_ratio"
)

ggplot(deming_dat, aes(x = th_c, y = mean_log_ratio, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_errorbar(
    aes(ymin = mean_log_ratio - se_log_ratio, ymax = mean_log_ratio + se_log_ratio),
    width = 0, alpha = 0.5, linewidth = 0.4
  ) +
  geom_errorbar(
    aes(xmin = th_c - se_th, xmax = th_c + se_th),
    width = 0, alpha = 0.5, linewidth = 0.4, orientation = "y"
  ) +
  geom_point(size = 2, alpha = 0.85) +
  geom_line(
    data        = pooled_lines,
    aes(x = th_c, y = log_ratio_hat),
    inherit.aes = FALSE,
    color = "black", linewidth = 0.9
  ) +
  geom_text(
    data        = pooled_labels,
    aes(x = x_pos, y = y_pos, label = label),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1, size = 3.2, color = "black", lineheight = 1.3
  ) +
  facet_wrap(~ drug, scales = "free") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x       = "Deactivation temperature Th (\u00b0C)",
    y       = "log(IC50 / ancestor IC50)",
    caption = paste(
      "Error bars: \u00b11 bootstrap SE on each axis",
      " | Line: Deming regression (pooled across evolution histories, per drug)"
    )
  ) +
  theme_evo() +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 12, face = "bold"),
    plot.caption    = element_text(size = 8, color = "grey40")
  )

ggsave("figures/deming-norm-20.png", width = 13, height = 5, dpi = 300, bg = "transparent")

# ── 6b. Within-group centered Deming ─────────────────────────────────────────

within_lines <- make_deming_line(
  deming_within, deming_centered,
  x_var = "th_c_c", y_hat_var = "log_ratio_hat",
  slope_col = "slope", has_intercept = FALSE
) |>
  clip_lines(deming_centered, "th_c_c", "log_ratio_hat", "log_ratio_c", "se_log_ratio")

within_labels <- label_position(
  deming_within, deming_centered, "th_c_c", "log_ratio_c", "se_log_ratio"
)

ggplot(deming_centered, aes(x = th_c_c, y = log_ratio_c, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_errorbar(
    aes(ymin = log_ratio_c - se_log_ratio, ymax = log_ratio_c + se_log_ratio),
    width = 0, alpha = 0.4, linewidth = 0.4
  ) +
  geom_errorbar(
    aes(xmin = th_c_c - se_th, xmax = th_c_c + se_th),
    width = 0, alpha = 0.4, linewidth = 0.4, orientation = "y"
  ) +
  geom_point(size = 2, alpha = 0.85) +
  geom_line(
    data      = within_lines,
    aes(x = th_c_c, y = log_ratio_hat),
    color     = "black", linewidth = 1.0
  ) +
  geom_text(
    data        = within_labels,
    aes(x = x_pos, y = y_pos, label = label),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1, size = 3.2, color = "black", lineheight = 1.3
  ) +
  facet_wrap(~ drug, scales = "free") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x       = "\u0394Th (\u00b0C, group-mean centered)",
    y       = "\u0394log(IC50 / ancestor IC50) (group-mean centered)",
    caption = paste(
      "Variables centered within each evolution history group to remove between-group confounding",
      " | Error bars: \u00b11 bootstrap SE | Line: within-group Deming regression (pooled across groups)"
    )
  ) +
  theme_evo() +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 12, face = "bold"),
    plot.caption    = element_text(size = 8, color = "grey40")
  )

ggsave("figures/deming-norm-within-20.png", width = 13, height = 5, dpi = 300, bg = "transparent")


# ── 6c. Pooled Deming: tmax ───────────────────────────────────────────────────

tmax_pooled_lines <- make_deming_line(
  deming_pooled_tmax, deming_dat,
  x_var = "tmax", y_hat_var = "log_ratio_hat"
) |>
  clip_lines(deming_dat, "tmax", "log_ratio_hat", "mean_log_ratio", "se_log_ratio")

tmax_pooled_labels <- label_position(
  deming_pooled_tmax, deming_dat, "tmax", "mean_log_ratio", "se_log_ratio"
)

ggplot(deming_dat, aes(x = tmax, y = mean_log_ratio, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_errorbar(
    aes(ymin = mean_log_ratio - se_log_ratio, ymax = mean_log_ratio + se_log_ratio),
    width = 0, alpha = 0.5, linewidth = 0.4
  ) +
  geom_errorbar(
    aes(xmin = tmax - se_tmax, xmax = tmax + se_tmax),
    width = 0, alpha = 0.5, linewidth = 0.4, orientation = "y"
  ) +
  geom_point(size = 2, alpha = 0.85) +
  geom_line(
    data        = tmax_pooled_lines,
    aes(x = tmax, y = log_ratio_hat),
    inherit.aes = FALSE,
    color = "black", linewidth = 0.9
  ) +
  geom_text(
    data        = tmax_pooled_labels,
    aes(x = x_pos, y = y_pos, label = label),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1, size = 3.2, color = "black", lineheight = 1.3
  ) +
  facet_wrap(~ drug, scales = "free") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x       = "CTmax (\u00b0C)",
    y       = "log(IC50 / ancestor IC50)",
    caption = paste(
      "Error bars: \u00b11 bootstrap SE on each axis",
      " | Line: Deming regression (pooled across evolution histories, per drug)"
    )
  ) +
  theme_evo() +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 12, face = "bold"),
    plot.caption    = element_text(size = 8, color = "grey40")
  )

ggsave("figures/deming-norm-tmax-20.png", width = 13, height = 5, dpi = 300, bg = "transparent")

# ── 6d. Within-group centered Deming: tmax ────────────────────────────────────

tmax_within_lines <- make_deming_line(
  deming_within_tmax, tmax_centered,
  x_var = "tmax_c", y_hat_var = "log_ratio_hat",
  slope_col = "slope", has_intercept = FALSE
) |>
  clip_lines(tmax_centered, "tmax_c", "log_ratio_hat", "log_ratio_c", "se_log_ratio")

tmax_within_labels <- label_position(
  deming_within_tmax, tmax_centered, "tmax_c", "log_ratio_c", "se_log_ratio"
)

ggplot(tmax_centered, aes(x = tmax_c, y = log_ratio_c, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_errorbar(
    aes(ymin = log_ratio_c - se_log_ratio, ymax = log_ratio_c + se_log_ratio),
    width = 0, alpha = 0.4, linewidth = 0.4
  ) +
  geom_errorbar(
    aes(xmin = tmax_c - se_tmax, xmax = tmax_c + se_tmax),
    width = 0, alpha = 0.4, linewidth = 0.4, orientation = "y"
  ) +
  geom_point(size = 2, alpha = 0.85) +
  geom_line(
    data      = tmax_within_lines,
    aes(x = tmax_c, y = log_ratio_hat),
    color     = "black", linewidth = 1.0
  ) +
  geom_text(
    data        = tmax_within_labels,
    aes(x = x_pos, y = y_pos, label = label),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1, size = 3.2, color = "black", lineheight = 1.3
  ) +
  facet_wrap(~ drug, scales = "free") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x       = "\u0394Tmax (\u00b0C, group-mean centered)",
    y       = "\u0394log(IC50 / ancestor IC50) (group-mean centered)",
    caption = paste(
      "Variables centered within each evolution history group to remove between-group confounding",
      " | Error bars: \u00b11 bootstrap SE | Line: within-group Deming regression (pooled across groups)"
    )
  ) +
  theme_evo() +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 12, face = "bold"),
    plot.caption    = element_text(size = 8, color = "grey40")
  )

ggsave("figures/deming-norm-tmax-within-20.png", width = 13, height = 5, dpi = 300, bg = "transparent")


# ── 6e. ΔTmax vs log-ratio: quantile regression at 50th and 75th percentiles ──
# Regression line omitted for pooled Deming (slopes ~128, −20: inflation artifact).
# Quantile regression (pooled across groups) shows the median and upper tail.

taus <- c(0.50, 0.75)

qr_lines <- delta_tmax_dat |>
  group_by(drug) |>
  group_modify(function(d, key) {
    x_seq <- seq(min(d$delta_tmax), max(d$delta_tmax), length.out = 200)
    map_dfr(taus, function(tau) {
      fit <- rq(mean_log_ratio ~ delta_tmax, tau = tau, data = d)
      tibble(
        delta_tmax    = x_seq,
        log_ratio_hat = predict(fit, newdata = data.frame(delta_tmax = x_seq)),
        quantile      = paste0(tau * 100, "%")
      )
    })
  }) |>
  ungroup()

ggplot(delta_tmax_dat,
       aes(x = delta_tmax, y = mean_log_ratio, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_errorbar(
    aes(ymin = mean_log_ratio - se_log_ratio,
        ymax = mean_log_ratio + se_log_ratio),
    width = 0, alpha = 0.4, linewidth = 0.4
  ) +
  geom_errorbar(
    aes(xmin = delta_tmax - se_delta_tmax,
        xmax = delta_tmax + se_delta_tmax),
    width = 0, alpha = 0.4, linewidth = 0.4, orientation = "y"
  ) +
  geom_point(size = 2, alpha = 0.85) +
  geom_line(
    data        = qr_lines,
    aes(x = delta_tmax, y = log_ratio_hat, linetype = quantile),
    inherit.aes = FALSE,
    color = "black", linewidth = 0.9
  ) +
  facet_wrap(~ drug, scales = "free_y") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_linetype_manual(values = c("50%" = "solid", "75%" = "dotted"),
                        name = "Quantile") +
  labs(
    x       = "\u0394Tmax relative to ancestor (\u00b0C)",
    y       = "log(IC50 / ancestor IC50)",
    caption = "Error bars: \u00b11 bootstrap SE  |  Dashed lines: ancestor reference (0, 0)  |  Lines: pooled quantile regression"
  ) +
  theme_evo() +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 12, face = "bold"),
    plot.caption    = element_text(size = 8, color = "grey40")
  )

ggsave("figures/deming-norm-dtmax-20.png", width = 13, height = 5, dpi = 300, bg = "transparent")

# ── 6f. Swapped axes: log-ratio on x, ΔTmax on y ─────────────────────────────

qr_lines_swapped <- delta_tmax_dat |>
  group_by(drug) |>
  group_modify(function(d, key) {
    x_seq <- seq(min(d$mean_log_ratio), max(d$mean_log_ratio), length.out = 200)
    map_dfr(taus, function(tau) {
      fit <- rq(delta_tmax ~ mean_log_ratio, tau = tau, data = d)
      tibble(
        mean_log_ratio = x_seq,
        delta_tmax_hat = predict(fit, newdata = data.frame(mean_log_ratio = x_seq)),
        quantile       = paste0(tau * 100, "%")
      )
    })
  }) |>
  ungroup()

ggplot(delta_tmax_dat,
       aes(x = mean_log_ratio, y = delta_tmax, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_errorbar(
    aes(ymin = delta_tmax - se_delta_tmax,
        ymax = delta_tmax + se_delta_tmax),
    width = 0, alpha = 0.4, linewidth = 0.4
  ) +
  geom_errorbar(
    aes(xmin = mean_log_ratio - se_log_ratio,
        xmax = mean_log_ratio + se_log_ratio),
    width = 0, alpha = 0.4, linewidth = 0.4, orientation = "y"
  ) +
  geom_point(size = 2, alpha = 0.85) +
  geom_line(
    data        = qr_lines_swapped,
    aes(x = mean_log_ratio, y = delta_tmax_hat, linetype = quantile),
    inherit.aes = FALSE,
    color = "black", linewidth = 0.9
  ) +
  facet_wrap(~ drug, scales = "free_x") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_linetype_manual(values = c("50%" = "solid", "75%" = "dotted"),
                        name = "Quantile") +
  labs(
    x       = "log(IC50 / ancestor IC50)",
    y       = "\u0394Tmax relative to ancestor (\u00b0C)",
    caption = "Error bars: \u00b11 bootstrap SE  |  Dashed lines: ancestor reference (0, 0)  |  Lines: pooled quantile regression"
  ) +
  theme_evo() +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 12, face = "bold"),
    plot.caption    = element_text(size = 8, color = "grey40")
  )

ggsave("figures/deming-norm-dtmax-swapped-20.png", width = 13, height = 5, dpi = 300, bg = "transparent")

# ── 6g. By-group: ΔTmax on x, log-ratio on y ─────────────────────────────────

qr_lines_bygroup <- delta_tmax_dat |>
  group_by(drug, evolution_history) |>
  group_modify(function(d, key) {
    x_seq <- seq(min(d$delta_tmax), max(d$delta_tmax), length.out = 200)
    map_dfr(taus, function(tau) {
      fit <- rq(mean_log_ratio ~ delta_tmax, tau = tau, data = d)
      tibble(
        delta_tmax    = x_seq,
        log_ratio_hat = predict(fit, newdata = data.frame(delta_tmax = x_seq)),
        quantile      = paste0(tau * 100, "%")
      )
    })
  }) |>
  ungroup()

ggplot(delta_tmax_dat,
       aes(x = delta_tmax, y = mean_log_ratio, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_errorbar(
    aes(ymin = mean_log_ratio - se_log_ratio,
        ymax = mean_log_ratio + se_log_ratio),
    width = 0, alpha = 0.4, linewidth = 0.4
  ) +
  geom_errorbar(
    aes(xmin = delta_tmax - se_delta_tmax,
        xmax = delta_tmax + se_delta_tmax),
    width = 0, alpha = 0.4, linewidth = 0.4, orientation = "y"
  ) +
  geom_point(size = 2, alpha = 0.85) +
  geom_line(
    data        = qr_lines_bygroup,
    aes(x = delta_tmax, y = log_ratio_hat, linetype = quantile),
    inherit.aes = FALSE,
    color = "black", linewidth = 0.9
  ) +
  facet_grid(evolution_history ~ drug, scales = "free") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_linetype_manual(values = c("50%" = "solid", "75%" = "dotted"),
                        name = "Quantile") +
  labs(
    x       = "\u0394Tmax relative to ancestor (\u00b0C)",
    y       = "log(IC50 / ancestor IC50)",
    caption = "Error bars: \u00b11 bootstrap SE  |  Dashed lines: ancestor reference (0, 0)  |  Lines: per-group quantile regression"
  ) +
  theme_evo() +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 12, face = "bold"),
    plot.caption    = element_text(size = 8, color = "grey40")
  )

ggsave("figures/deming-norm-dtmax-bygroup-flipped-20.png", width = 13, height = 8, dpi = 300, bg = "transparent")

# ── 6h. By-group: log-ratio on x, ΔTmax on y ─────────────────────────────────

qr_lines_bygroup_swapped <- delta_tmax_dat |>
  group_by(drug, evolution_history) |>
  group_modify(function(d, key) {
    x_seq <- seq(min(d$mean_log_ratio), max(d$mean_log_ratio), length.out = 200)
    map_dfr(taus, function(tau) {
      fit <- rq(delta_tmax ~ mean_log_ratio, tau = tau, data = d)
      tibble(
        mean_log_ratio = x_seq,
        delta_tmax_hat = predict(fit, newdata = data.frame(mean_log_ratio = x_seq)),
        quantile       = paste0(tau * 100, "%")
      )
    })
  }) |>
  ungroup()

ggplot(delta_tmax_dat,
       aes(x = mean_log_ratio, y = delta_tmax, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_errorbar(
    aes(ymin = delta_tmax - se_delta_tmax,
        ymax = delta_tmax + se_delta_tmax),
    width = 0, alpha = 0.4, linewidth = 0.4
  ) +
  geom_errorbar(
    aes(xmin = mean_log_ratio - se_log_ratio,
        xmax = mean_log_ratio + se_log_ratio),
    width = 0, alpha = 0.4, linewidth = 0.4, orientation = "y"
  ) +
  geom_point(size = 2, alpha = 0.85) +
  geom_line(
    data        = qr_lines_bygroup_swapped,
    aes(x = mean_log_ratio, y = delta_tmax_hat, linetype = quantile),
    inherit.aes = FALSE,
    color = "black", linewidth = 0.9
  ) +
  facet_grid(evolution_history ~ drug, scales = "free") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_linetype_manual(values = c("50%" = "solid", "75%" = "dotted"),
                        name = "Quantile") +
  labs(
    x       = "log(IC50 / ancestor IC50)",
    y       = "\u0394Tmax relative to ancestor (\u00b0C)",
    caption = "Error bars: \u00b11 bootstrap SE  |  Dashed lines: ancestor reference (0, 0)  |  Lines: per-group quantile regression"
  ) +
  theme_evo() +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 12, face = "bold"),
    plot.caption    = element_text(size = 8, color = "grey40")
  )

ggsave("figures/deming-norm-dtmax-bygroup-20.png", width = 13, height = 8, dpi = 300, bg = "transparent")


# =============================================================================
# 6i. Enrichment tests — 40-evolved, top-third ΔTh and ΔTmax
# =============================================================================
# Do strains with the largest thermal adaptation (top third of ΔTh or ΔTmax)
# show higher drug tolerance than the bottom two-thirds?
#
# Two tests per thermal trait × drug (one-sided, H1: top-third > bottom):
#   1. Fisher's exact test  — binarises outcome at log-ratio > 0
#   2. Wilcoxon rank-sum    — uses full continuous log-ratio distribution

anc_th_c      <- tpc_se |> filter(evolution_history == "fRS585") |> pull(th_c)

forty_evolved <- delta_tmax_dat |> filter(evolution_history == "40 evolved")
forty_th      <- deming_dat |>
  filter(evolution_history == "40 evolved") |>
  mutate(delta_th = th_c - anc_th_c)

tmax_threshold_40 <- forty_evolved |>
  distinct(strain, delta_tmax) |> pull(delta_tmax) |> quantile(2/3)

th_threshold_40 <- forty_th |>
  distinct(strain, delta_th) |> pull(delta_th) |> quantile(2/3)

cat(sprintf("ΔTmax top-third threshold (40-evolved): ≥ %.3f°C\n", tmax_threshold_40))
cat(sprintf("ΔTh  top-third threshold (40-evolved): ≥ %.3f°C\n\n", th_threshold_40))

run_enrichment <- function(dat, delta_var, threshold, label) {
  d <- dat |> mutate(high = .data[[delta_var]] >= threshold)

  # Fisher's exact test
  fisher_res <- d |>
    group_by(drug) |>
    group_modify(function(g, key) {
      tab <- table(high_thermal = g$high,
                   high_ic50    = g$mean_log_ratio > 0)
      ft  <- fisher.test(tab, alternative = "greater")
      tibble(
        n_top_high_ic50  = sum(g$high  & g$mean_log_ratio > 0),
        n_bot_high_ic50  = sum(!g$high & g$mean_log_ratio > 0),
        pct_top          = mean(g$mean_log_ratio[g$high]  > 0),
        pct_bot          = mean(g$mean_log_ratio[!g$high] > 0),
        odds_ratio       = ft$estimate,
        p_fisher         = ft$p.value
      )
    }) |> ungroup()

  # Wilcoxon rank-sum
  wilcox_res <- d |>
    group_by(drug) |>
    group_modify(function(g, key) {
      top <- g$mean_log_ratio[g$high]
      bot <- g$mean_log_ratio[!g$high]
      wt  <- wilcox.test(top, bot, alternative = "greater", exact = FALSE)
      tibble(
        median_top = median(top),
        median_bot = median(bot),
        W          = wt$statistic,
        p_wilcox   = wt$p.value
      )
    }) |> ungroup()

  res <- left_join(fisher_res, wilcox_res, by = "drug") |>
    mutate(across(where(is.numeric), ~round(., 3)))

  cat(sprintf("=== %s ===\n", label))
  print(res, n = Inf, width = Inf)
  cat("\n")
  res
}

enrich_dtmax <- run_enrichment(forty_evolved, "delta_tmax", tmax_threshold_40,
                               "ΔTmax, 40-evolved (top-third ≥ 0.650°C)")
enrich_dth   <- run_enrichment(forty_th,      "delta_th",   th_threshold_40,
                               "ΔTh, 40-evolved (top-third ≥ 0.378°C)")

write_csv(enrich_dtmax, "data-processed/enrichment-dtmax-40-20.csv")
write_csv(enrich_dth,   "data-processed/enrichment-dth-40-20.csv")

# ── Visualise top-third vs bottom two-thirds on ΔTmax scatter (40-evolved) ───

plot_dat_40 <- delta_tmax_dat |>
  filter(evolution_history == "40 evolved") |>
  mutate(
    tier = if_else(delta_tmax >= tmax_threshold_40,
                   "top third", "bottom two-thirds"),
    tier = factor(tier, levels = c("top third", "bottom two-thirds"))
  )

qr_40 <- qr_lines_bygroup |> filter(evolution_history == "40 evolved")

# ── Scatter: tier shown by point shape (no regression line) ──────────────────
ggplot(plot_dat_40, aes(x = delta_tmax, y = mean_log_ratio)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = tmax_threshold_40, linetype = "dotted",
             color = "grey40", linewidth = 0.5) +
  geom_errorbar(
    aes(ymin = mean_log_ratio - se_log_ratio,
        ymax = mean_log_ratio + se_log_ratio),
    width = 0, alpha = 0.4, linewidth = 0.4, color = EVO_COLORS["40 evolved"]
  ) +
  geom_errorbar(
    aes(xmin = delta_tmax - se_delta_tmax,
        xmax = delta_tmax + se_delta_tmax),
    width = 0, alpha = 0.4, linewidth = 0.4, orientation = "y",
    color = EVO_COLORS["40 evolved"]
  ) +
  geom_point(aes(shape = tier), size = 3,
             color = EVO_COLORS["40 evolved"], alpha = 0.9) +
  facet_wrap(~ drug, scales = "free_y") +
  scale_shape_manual(values = c("top third" = 17, "bottom two-thirds" = 16),
                     name = "\u0394Tmax tier") +
  labs(
    x       = "\u0394Tmax relative to ancestor (\u00b0C)",
    y       = "log(IC50 / ancestor IC50)",
    caption = paste0(
      "Dotted vertical line: top-third threshold (\u0394Tmax \u2265 ",
      round(tmax_threshold_40, 2), "\u00b0C)",
      "  |  Error bars: \u00b11 bootstrap SE"
    )
  ) +
  theme_evo() +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 12, face = "bold"),
    plot.caption    = element_text(size = 8, color = "grey40")
  )

ggsave("figures/wilcox-dtmax-tier-40-20.png", width = 13, height = 5, dpi = 300, bg = "transparent")

# ── Strip chart: direct Wilcoxon visualization ────────────────────────────────
ggplot(plot_dat_40, aes(x = tier, y = mean_log_ratio, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_jitter(width = 0.12, size = 2.5, alpha = 0.85) +
  stat_summary(fun = median, geom = "crossbar", width = 0.4,
               linewidth = 0.7, color = "black", fatten = 1) +
  facet_wrap(~ drug, scales = "free_y") +
  scale_color_manual(values = EVO_COLORS, guide = "none") +
  labs(
    x       = "\u0394Tmax tier (40-evolved)",
    y       = "log(IC50 / ancestor IC50)",
    caption = "Crossbar: median  |  Wilcoxon rank-sum p: amphotericin = 0.051, caspofungin = 0.337, fluconazole = 0.213"
  ) +
  theme_evo() +
  theme(
    strip.text   = element_text(size = 12, face = "bold"),
    plot.caption = element_text(size = 8, color = "grey40")
  )

ggsave("figures/wilcox-dtmax-strip-40-20.png", width = 10, height = 5, dpi = 300, bg = "transparent")


# =============================================================================
# 7. PCA: thermal traits + normalized log-ratios
# =============================================================================
# Evolved strains only; ancestor has log-ratio = 0 by definition.
# Variables: topt, tmax, th_c, b80 + log-ratio for each drug.

ic50_wide <- norm_boot |>
  filter(evolution_history %in% c("35 evolved", "40 evolved")) |>
  select(population, drug, mean_log_ratio) |>
  pivot_wider(names_from = drug, values_from = mean_log_ratio)

pca_input <- tpc_se |>
  filter(evolution_history %in% c("35 evolved", "40 evolved")) |>
  select(strain, evolution_history, topt, tmax, th_c) |>
  inner_join(ic50_wide, by = c("strain" = "population")) |>
  drop_na()

PCA_VARS <- c("topt", "tmax", "th_c", "fluconazole", "caspofungin", "amphotericin")

cat(sprintf("\nPCA input: %d strains × %d variables\n", nrow(pca_input), length(PCA_VARS)))
cat("Missing after drop_na:", nrow(tpc_se |> filter(evolution_history != "fRS585")) - nrow(pca_input), "strains\n")

# ── 7a. Standard (pooled) PCA ─────────────────────────────────────────────────

pca <- prcomp(pca_input[, PCA_VARS], center = TRUE, scale. = TRUE)

cat("\n=== PCA: variance explained ===\n")
print(round(summary(pca)$importance[, 1:5], 3))

cat("\n=== PCA: loadings (PC1–PC3) ===\n")
print(round(pca$rotation[, 1:3], 3))

pct <- summary(pca)$importance["Proportion of Variance", ] * 100

scores <- as.data.frame(pca$x) |>
  bind_cols(pca_input |> select(strain, evolution_history)) |>
  mutate(evolution_history = factor(evolution_history,
                                    levels = c("35 evolved", "40 evolved")))

loadings <- as.data.frame(pca$rotation[, 1:2]) |>
  rownames_to_column("variable") |>
  mutate(
    PC1_scaled = PC1 * max(abs(scores$PC1)),
    PC2_scaled = PC2 * max(abs(scores$PC2))
  )

ggplot(scores, aes(x = PC1, y = PC2, color = evolution_history)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_segment(
    data        = loadings,
    aes(x = 0, y = 0, xend = PC1_scaled, yend = PC2_scaled),
    inherit.aes = FALSE,
    arrow       = arrow(length = unit(0.25, "cm")),
    color = "grey30", linewidth = 0.6
  ) +
  geom_text(
    data        = loadings,
    aes(x = PC1_scaled * 1.12, y = PC2_scaled * 1.12, label = variable),
    inherit.aes = FALSE,
    size = 3.2, color = "grey20"
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x       = sprintf("PC1 (%.1f%%)", pct[1]),
    y       = sprintf("PC2 (%.1f%%)", pct[2]),
    caption = "Variables: Topt, Tmax, Th + log(IC50 / ancestor IC50) for 3 drugs"
  ) +
  theme_evo() +
  theme(legend.position = "bottom")

ggsave("figures/pca-biplot-20.png", width = 7, height = 6, dpi = 300, bg = "transparent")

# ── 7b. Within-group centered PCA ─────────────────────────────────────────────
# Group-mean center each variable within evolution history before PCA so the
# ordination reflects within-group covariation only.

wide_centered <- pca_input |>
  group_by(evolution_history) |>
  mutate(across(all_of(PCA_VARS), ~ . - mean(., na.rm = TRUE))) |>
  ungroup()

pca_c <- prcomp(wide_centered[, PCA_VARS], center = FALSE, scale. = TRUE)

cat("\n=== Within-group PCA: variance explained ===\n")
print(round(summary(pca_c)$importance[, 1:5], 3))

cat("\n=== Within-group PCA: loadings (PC1–PC3) ===\n")
print(round(pca_c$rotation[, 1:3], 3))

pct_c <- summary(pca_c)$importance["Proportion of Variance", ] * 100

scores_c <- as.data.frame(pca_c$x) |>
  bind_cols(pca_input |> select(strain, evolution_history)) |>
  mutate(evolution_history = factor(evolution_history,
                                    levels = c("35 evolved", "40 evolved")))

loadings_c <- as.data.frame(pca_c$rotation[, 1:2]) |>
  rownames_to_column("variable") |>
  mutate(
    PC1_scaled = PC1 * max(abs(scores_c$PC1)),
    PC2_scaled = PC2 * max(abs(scores_c$PC2))
  )

ggplot(scores_c, aes(x = PC1, y = PC2, color = evolution_history)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_segment(
    data        = loadings_c,
    aes(x = 0, y = 0, xend = PC1_scaled, yend = PC2_scaled),
    inherit.aes = FALSE,
    arrow       = arrow(length = unit(0.25, "cm")),
    color = "grey30", linewidth = 0.6
  ) +
  geom_text(
    data        = loadings_c,
    aes(x = PC1_scaled * 1.12, y = PC2_scaled * 1.12, label = variable),
    inherit.aes = FALSE,
    size = 3.2, color = "grey20"
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x       = sprintf("PC1 (%.1f%%)", pct_c[1]),
    y       = sprintf("PC2 (%.1f%%)", pct_c[2])
  ) +
  theme_evo() +
  theme(legend.position = "bottom")

ggsave("figures/pca-biplot-within-20.png", width = 5, height = 4, dpi = 300, bg = "transparent")


# =============================================================================
# 8. Export
# =============================================================================

write_csv(norm_boot,             "data-processed/norm-ic50-boot-20.csv")
write_csv(deming_pooled,         "data-processed/deming-norm-pooled-20.csv")
write_csv(deming_by_group,       "data-processed/deming-norm-by-group-20.csv")
write_csv(deming_within,         "data-processed/deming-norm-within-20.csv")
write_csv(deming_pooled_tmax,    "data-processed/deming-norm-tmax-pooled-20.csv")
write_csv(deming_by_group_tmax,  "data-processed/deming-norm-tmax-by-group-20.csv")
write_csv(deming_within_tmax,    "data-processed/deming-norm-tmax-within-20.csv")
write_csv(ols_tmax_by_group,        "data-processed/ols-norm-tmax-by-group-20.csv")
write_csv(deming_pooled_dtmax,     "data-processed/deming-norm-dtmax-pooled-20.csv")
write_csv(deming_by_group_dtmax,   "data-processed/deming-norm-dtmax-by-group-20.csv")
write_csv(deming_within_dtmax,     "data-processed/deming-norm-dtmax-within-20.csv")
write_csv(ols_dtmax_by_group,      "data-processed/ols-norm-dtmax-by-group-20.csv")
write_csv(perm_results,          "data-processed/deming-norm-perm-20.csv")

write_csv(
  scores   |> select(strain, evolution_history, PC1, PC2, PC3),
  "data-processed/pca-scores-20.csv"
)
write_csv(
  loadings |> select(variable, PC1, PC2),
  "data-processed/pca-loadings-20.csv"
)
write_csv(
  scores_c |> select(strain, evolution_history, PC1, PC2, PC3),
  "data-processed/pca-scores-within-20.csv"
)
write_csv(
  loadings_c |> select(variable, PC1, PC2),
  "data-processed/pca-loadings-within-20.csv"
)

cat("\nAll outputs written.\n")


# =============================================================================
# 9. Group-separation tests on PCA scores
# =============================================================================
# Tests whether 35-evolved and 40-evolved strains differ in PCA space.
# Uses the POOLED PCA from section 7a only. The within-group-centered PCA
# removes between-group mean differences by construction, so testing group
# separation on those scores would be circular.
#
# 9a. Welch t-test + Wilcoxon on PC1 and PC2 scores individually
#     (unadjusted p-values)
# 9b. MANOVA (Pillai trace) on PCs retaining ≥ 5% variance
# 9c. PERMANOVA (vegan::adonis2) — permutation-based, no distributional
#     assumptions, on the same PCs as MANOVA

library(vegan)

# Variance explained per PC (from pooled PCA)
pct_var  <- summary(pca)$importance["Proportion of Variance", ] * 100
pcs_keep <- names(pct_var[pct_var >= 5])

cat(sprintf("\nPCs retained for MANOVA/PERMANOVA (≥ 5%% variance each): %s\n",
            paste(pcs_keep, collapse = ", ")))
cat("Variance explained:", paste(sprintf("%s = %.1f%%", pcs_keep, pct_var[pcs_keep]),
                                 collapse = ", "), "\n")

scores_ev <- scores |>
  mutate(evolution_history = as.character(evolution_history))

g35 <- scores_ev |> filter(evolution_history == "35 evolved")
g40 <- scores_ev |> filter(evolution_history == "40 evolved")

# ── 9a. Per-PC Welch t-test + Wilcoxon ───────────────────────────────────────

pc_tests <- map_dfr(c("PC1", "PC2"), function(pc) {
  t_res <- t.test(g35[[pc]], g40[[pc]])
  w_res <- wilcox.test(g35[[pc]], g40[[pc]], exact = FALSE)
  tibble(
    PC         = pc,
    mean_35    = mean(g35[[pc]]),
    mean_40    = mean(g40[[pc]]),
    diff_40_35 = mean(g40[[pc]]) - mean(g35[[pc]]),
    ci_lo      = t_res$conf.int[1],
    ci_hi      = t_res$conf.int[2],
    p_welch    = t_res$p.value,
    p_wilcox   = w_res$p.value
  )
}) |>


cat("\n=== Per-PC tests: 35 evolved vs 40 evolved (pooled PCA) ===\n")
print(pc_tests |> mutate(across(where(is.numeric), ~round(., 4))), width = Inf)

# ── 9b. MANOVA (Pillai trace) ─────────────────────────────────────────────────

manova_mat <- scores_ev |> select(all_of(pcs_keep)) |> as.matrix()
grp        <- scores_ev$evolution_history

manova_fit <- manova(manova_mat ~ grp)

cat("\n=== MANOVA (Pillai trace) on PCA scores ===\n")
print(summary(manova_fit, test = "Pillai"))

# Univariate follow-up (already done per-PC above, but printed again for context)
cat("\n=== MANOVA: univariate follow-up ===\n")
print(summary.aov(manova_fit))

# ── 9c. PERMANOVA (adonis2) ───────────────────────────────────────────────────
# Euclidean distance on PC scores. With orthogonal PCs and Euclidean distance,
# adonis2 is testing the same hypothesis as MANOVA but without normality
# assumptions. Results should be very similar; large discrepancies would
# suggest the normality assumption matters.

set.seed(6193)
perm_sep <- adonis2(
  manova_mat ~ grp,
  method       = "euclidean",
  permutations = 9999
)

cat("\n=== PERMANOVA (adonis2, 9999 permutations) on PCA scores ===\n")
print(perm_sep)

# ── Export ────────────────────────────────────────────────────────────────────

write_csv(pc_tests, "data-processed/pca-group-tests-20.csv")

tidy_manova <- broom::tidy(manova_fit)
write_csv(tidy_manova, "data-processed/pca-manova-20.csv")

perm_tbl <- as_tibble(perm_sep, rownames = "term")
write_csv(perm_tbl, "data-processed/pca-permanova-20.csv")


# =============================================================================
# 10. Within-group Spearman correlations: thermal traits vs drug tolerance
# =============================================================================
# For each evolution history group, correlate each thermal trait (topt, tmax,
# th_c) against each drug's mean log-ratio across strains. Spearman rank
# correlation is used: nonparametric, robust to extreme values in a few
# strains, and symmetric (no need to designate a predictor axis).
#
# No multiple-testing correction applied.

thermal_vars <- c("topt", "tmax", "th_c")
drug_vars    <- c("fluconazole", "caspofungin", "amphotericin")

# Join thermal traits and IC50 log-ratios per strain
corr_input <- tpc_se |>
  filter(evolution_history %in% c("35 evolved", "40 evolved")) |>
  dplyr::select(strain, evolution_history, all_of(thermal_vars)) |>
  inner_join(
    norm_boot |>
      filter(evolution_history %in% c("35 evolved", "40 evolved")) |>
      dplyr::select(population, drug, mean_log_ratio) |>
      pivot_wider(names_from = drug, values_from = mean_log_ratio),
    by = c("strain" = "population")
  ) |>
  drop_na()

within_corr <- corr_input |>
  group_by(evolution_history) |>
  group_modify(function(d, key) {
    expand.grid(
      thermal = thermal_vars,
      drug    = drug_vars,
      stringsAsFactors = FALSE
    ) |>
      as_tibble() |>
      rowwise() |>
      mutate(
        n     = nrow(d),
        rho   = cor(d[[thermal]], d[[drug]], method = "spearman"),
        p_raw = cor.test(d[[thermal]], d[[drug]],
                         method = "spearman", exact = FALSE)$p.value
      ) |>
      ungroup()
  }) |>
  ungroup() |>
  mutate(sig = case_when(
    p_raw < 0.001 ~ "***",
    p_raw < 0.01  ~ "**",
    p_raw < 0.05  ~ "*",
    TRUE          ~ "ns"
  ))

cat("\n=== Within-group Spearman correlations: thermal traits vs log(IC50/ancestor) ===\n")
within_corr |>
  mutate(across(c(rho, p_raw), ~round(., 3))) |>
  arrange(evolution_history, thermal, drug) |>
  print(n = Inf, width = Inf)

# ── Heatmap of rho values ─────────────────────────────────────────────────────

within_corr |>
  mutate(
    thermal           = factor(thermal, levels = c("topt", "tmax", "th_c"),
                               labels = c("Topt", "Tmax", "Th")),
    drug              = factor(drug, levels = c("fluconazole", "caspofungin", "amphotericin")),
    evolution_history = factor(evolution_history, levels = c("35 evolved", "40 evolved"))
  ) |>
  ggplot(aes(x = drug, y = thermal, fill = rho)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f%s", rho, sig)), size = 3.8) +
  facet_wrap(~ evolution_history) +
  scale_fill_gradient2(
    low = "#0E63FF", mid = "white", high = "#FA3208",
    midpoint = 0, limits = c(-1, 1), name = "Spearman \u03c1"
  ) +
  labs(
    x       = NULL,
    y       = NULL,
    caption = "Values: Spearman \u03c1  |  Significance: unadjusted p (* p<0.05, ** p<0.01, *** p<0.001)"
  ) +
  theme_evo() +
  theme(
    strip.text   = element_text(size = 13, face = "bold"),
    axis.text    = element_text(size = 11),
    plot.caption = element_text(size = 8, color = "grey40")
  )

ggsave("figures/within-group-spearman-20.png", width = 9, height = 4,
       dpi = 300, bg = "transparent")

write_csv(within_corr, "data-processed/within-group-spearman-20.csv")


# =============================================================================
# 11. TPC trait correlation figures
# =============================================================================
# Pairs plot (Topt, Tmax, Th) and investigation of the Topt–Th sign flip
# between evolution history groups.
#
# Outputs (figures/):
#   tpc-trait-pairs-plot.png      — pairs plot with density diagonals
#   topt-th-scatter.png           — Topt vs Th with per-group regression lines
#   eh-topt-th-scatter.png        — eh vs Topt and Th, illustrating the
#                                   mechanism behind the Topt–Th sign flip

library(patchwork)
library(ggrepel)

# ── 11a. Load raw SSH params (needed for eh) ──────────────────────────────────

tpc_params_raw <- read_csv("data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv",
                           show_col_types = FALSE)

traits <- tpc_se |>
  filter(evolution_history %in% c("35 evolved", "40 evolved", "fRS585")) |>
  mutate(evolution_history = factor(evolution_history,
                                    levels = c("35 evolved", "40 evolved", "fRS585")))

traits_ssh <- tpc_params_raw |>
  filter(evolution_history %in% c("35 evolved", "40 evolved")) |>
  mutate(evolution_history = factor(evolution_history,
                                    levels = c("35 evolved", "40 evolved")))

# ── 11b. TPC trait pairs plot ─────────────────────────────────────────────────

var_labels <- c(topt = "Topt (\u00b0C)", tmax = "Tmax (\u00b0C)", th_c = "Th (\u00b0C)")

make_diag <- function(v) {
  ggplot(traits, aes(x = .data[[v]], fill = evolution_history,
                     color = evolution_history)) +
    geom_density(alpha = 0.4, linewidth = 0.5) +
    scale_fill_manual(values  = EVO_COLORS, guide = "none") +
    scale_color_manual(values = EVO_COLORS, guide = "none") +
    labs(x = NULL, y = NULL, title = var_labels[v]) +
    theme_classic(base_size = 11) +
    theme(plot.title = element_text(hjust = 0.5, size = 11))
}

make_lower <- function(xv, yv) {
  ggplot(traits, aes(x = .data[[xv]], y = .data[[yv]], color = evolution_history)) +
    geom_point(size = 1.8, alpha = 0.7) +
    scale_color_manual(values = EVO_COLORS, guide = "none") +
    labs(x = var_labels[xv], y = var_labels[yv]) +
    theme_classic(base_size = 11)
}

make_upper <- function(xv, yv) {
  r_all <- cor(traits[[xv]], traits[[yv]], use = "complete.obs")
  r_35  <- cor(
    traits[[xv]][traits$evolution_history == "35 evolved"],
    traits[[yv]][traits$evolution_history == "35 evolved"],
    use = "complete.obs"
  )
  r_40  <- cor(
    traits[[xv]][traits$evolution_history == "40 evolved"],
    traits[[yv]][traits$evolution_history == "40 evolved"],
    use = "complete.obs"
  )
  label_df <- tibble(
    x     = c(0.5, 0.5, 0.5),
    y     = c(0.65, 0.45, 0.25),
    label = c(sprintf("r = %.2f (all)", r_all),
              sprintf("r = %.2f (35\u00b0C)", r_35),
              sprintf("r = %.2f (40\u00b0C)", r_40)),
    color = c("black", EVO_COLORS["35 evolved"], EVO_COLORS["40 evolved"])
  )
  ggplot(label_df, aes(x = x, y = y, label = label, color = color)) +
    geom_text(size = 3.5, hjust = 0.5) +
    scale_color_identity() +
    xlim(0, 1) + ylim(0, 1) +
    theme_void()
}

pairs_plot <-
  (make_diag("topt")          | make_upper("topt", "tmax") | make_upper("topt", "th_c")) /
  (make_lower("topt", "tmax") | make_diag("tmax")           | make_upper("tmax", "th_c")) /
  (make_lower("topt", "th_c") | make_lower("tmax", "th_c")  | make_diag("th_c")) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("figures/tpc-trait-pairs-plot.png", pairs_plot,
       width = 8, height = 7, dpi = 300, bg = "transparent")

# ── 11c. Topt vs Th scatter with per-group regression lines ───────────────────
# Within-group correlations: 35 evolved r = 0.956 (p < 0.001),
#                             40 evolved r = −0.55 (p = 0.018)

topt_th_plot <- traits |>
  filter(evolution_history != "fRS585") |>
  ggplot(aes(x = topt, y = th_c, color = evolution_history)) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, alpha = 0.15) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_text_repel(
    aes(label = strain),
    size = 2.8, max.overlaps = 20, show.legend = FALSE
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(x = "Topt (\u00b0C)", y = "Th (\u00b0C)") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("figures/topt-th-scatter.png", topt_th_plot,
       width = 7, height = 6, dpi = 300, bg = "transparent")

# ── 11d. eh vs Topt and Th ────────────────────────────────────────────────────
# eh (deactivation energy) predicts Topt equally in both groups (r ≈ 0.95),
# but has opposite relationships with Th: positive in 35-evolved (r = 0.82),
# negative in 40-evolved (r = −0.66). This mechanistically explains the
# Topt–Th sign flip: the 40-evolved group shows an eh–Th trade-off in which
# high deactivation steepness accompanies lower deactivation temperature.

annotate_r <- function(plt, x_var, y_var) {
  r_df <- traits_ssh |>
    group_by(evolution_history) |>
    summarise(
      r = cor(.data[[x_var]], .data[[y_var]], use = "complete.obs"),
      p = cor.test(.data[[x_var]], .data[[y_var]])$p.value,
      .groups = "drop"
    ) |>
    mutate(
      label = sprintf("r = %.2f%s", r,
                      ifelse(p < 0.001, "***",
                             ifelse(p < 0.01, "**",
                                    ifelse(p < 0.05, "*", "")))),
      x     = -Inf,
      y     = if_else(evolution_history == "35 evolved", Inf, -Inf),
      vjust = if_else(evolution_history == "35 evolved", 1.5, -0.5)
    )
  plt +
    geom_text(
      data        = r_df,
      aes(x = x, y = y, label = label, color = evolution_history, vjust = vjust),
      hjust       = -0.1, size = 3.8, fontface = "bold",
      show.legend = FALSE, inherit.aes = FALSE
    )
}

make_eh_scatter <- function(y_var, y_lab) {
  plt <- traits_ssh |>
    ggplot(aes(x = eh, y = .data[[y_var]], color = evolution_history)) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, alpha = 0.15) +
    geom_point(size = 2.5, alpha = 0.8) +
    scale_color_manual(values = EVO_COLORS, name = NULL) +
    labs(x = "Deactivation energy eh (eV)", y = y_lab) +
    theme_classic(base_size = 12) +
    theme(legend.position = "bottom")
  annotate_r(plt, "eh", y_var)
}

eh_fig <- (make_eh_scatter("topt", "Topt (\u00b0C)") |
           make_eh_scatter("th_c", "Th (\u00b0C)")) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("figures/eh-topt-th-scatter.png", eh_fig,
       width = 10, height = 5, dpi = 300, bg = "transparent")
