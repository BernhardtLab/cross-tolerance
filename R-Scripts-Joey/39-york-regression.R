# =============================================================================
# 39-york-regression.R
# York regression (York et al. 2004) of log2 IC50 fold change on the
# ancestor-window mean growth, within each evolution treatment.
#
# York regression accounts for error in both variables, with a standard error
# for every point in x and in y.
#   y error: SE of each strain's mean of plate-level log2 IC50 ratios
#            = sd(plates) / sqrt(n_plates)
#   x error: SE of the ancestor-window mean from the TPC bootstrap bands
#            (Script 19, tpc-ci-ribbon-19.csv): mean over the window of
#            (ci_hi - ci_lo) / (2 * 1.96). Conservative (errors treated as
#            fully correlated across temperatures).
# No error correlation between x and y is assumed (r = 0).
#
# y error: pooled plate-to-plate SD per drug / sqrt(n_plates), so every strain
# has the same per-plate precision (per-strain SDs from 3 plates are too noisy).
# Checks: with x-error set to ~0, York reduces to weighted least squares (1/sy^2).
#
# Inputs:  data-processed/normalised-ic50-per-plate.csv
#          data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv
#          data-processed/gcplyr/tpc-ci-ribbon-19.csv
# Outputs: data-processed/york-regression-39.csv
# =============================================================================

library(tidyverse)

OUT <- "data-processed"
EVO_LEVELS <- c("35 evolved", "40 evolved")
DRUG_LABELS <- c(fluconazole = "Fluconazole", caspofungin = "Caspofungin",
                 amphotericin = "Amphotericin B")


# =============================================================================
# 1. York (2004) fit with per-point errors in x and y, no error correlation
# =============================================================================

york_fit <- function(x, y, sx, sy, tol = 1e-12, maxit = 1000) {
  wx <- 1 / sx^2
  wy <- 1 / sy^2
  b  <- cov(x, y) / var(x)            # starting value: OLS
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
  n    <- length(x)
  tcrit <- qt(0.975, df = n - 2)
  tibble(slope = b, se = se_b, lo = b - tcrit * se_b, hi = b + tcrit * se_b,
         intercept = sum(W * y) / sum(W) - b * xbar, iterations = i,
         pivot_x = xbar, pivot_y = sum(W * y) / sum(W))
}


# =============================================================================
# 2. Data: strain means of IC50 ratios (y) and ancestor-window means with error (x)
# =============================================================================

ic50_plate <- read_csv(file.path(OUT, "normalised-ic50-per-plate.csv"), show_col_types = FALSE) |>
  filter(evolution_history %in% EVO_LEVELS)

y_dat <- ic50_plate |>
  group_by(strain = population, evolution_history, drug) |>
  summarise(y = mean(log_ratio), sy = sd(log_ratio) / sqrt(n()), n_plates = n(), .groups = "drop")

stopifnot(min(y_dat$n_plates) >= 2)

tpc_par <- read_csv(file.path(OUT, "gcplyr/tpc-params-auc-gcplyr-17.csv"), show_col_types = FALSE)
ribbon  <- read_csv(file.path(OUT, "gcplyr/tpc-ci-ribbon-19.csv"), show_col_types = FALSE)

anc <- tpc_par |> filter(evolution_history == "fRS585") |> select(topt, tmax)
stopifnot(nrow(anc) == 1)

# Window mean from the fitted curve, computed exactly as in Script 37
# (trapezoid integral over the ancestor window, divided by its width)
integrate_window <- function(temp, pred, lo, hi) {
  x <- sort(unique(c(lo, hi, temp[temp > lo & temp < hi])))
  y <- approx(temp, pred, xout = x)$y
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

x_mean <- tpc_pred |>
  filter(evolution_history %in% EVO_LEVELS) |>
  group_by(strain, evolution_history) |>
  summarise(x = integrate_window(temp, pred, anc$topt, anc$tmax) / (anc$tmax - anc$topt),
            .groups = "drop")

# Standard error of the window mean from the TPC bootstrap bands (conservative bound)
x_se <- ribbon |>
  filter(evolution_history %in% EVO_LEVELS,
         temperature >= anc$topt, temperature <= anc$tmax) |>
  mutate(se_t = (ci_hi - ci_lo) / (2 * 1.96)) |>
  group_by(strain, evolution_history) |>
  summarise(sx = mean(se_t), .groups = "drop")

x_dat <- x_mean |> inner_join(x_se, by = c("strain", "evolution_history"))

dat <- y_dat |> inner_join(x_dat, by = c("strain", "evolution_history"))
stopifnot(nrow(dat) == 36 * 3)

# Plate-to-plate SD pooled across strains (within-strain deviations), per drug.
# Per-strain SDs from 3 plates are too noisy: a few strains would get most of
# the York weight. The pooled SD gives every strain the same per-plate precision.
pooled_sd <- ic50_plate |>
  group_by(population, drug) |>
  mutate(dev = log_ratio - mean(log_ratio)) |>
  ungroup() |>
  group_by(drug) |>
  summarise(pool_sd = sqrt(sum(dev^2) / (n() - n_distinct(population))), .groups = "drop")

dat <- dat |>
  left_join(pooled_sd, by = "drug") |>
  mutate(sy = pool_sd / sqrt(n_plates))


# =============================================================================
# 3. Within-treatment York fits, with OLS for comparison
# =============================================================================

results <- dat |>
  group_by(drug, evolution_history) |>
  group_modify(\(d, k) {
    yk <- york_fit(d$x, d$y, d$sx, d$sy)
    ols <- lm(y ~ x, data = d)
    tibble(
      n = nrow(d),
      york_slope = yk$slope, york_lo = yk$lo, york_hi = yk$hi,
      york_intercept = yk$intercept,
      york_pivot_x = yk$pivot_x, york_pivot_y = yk$pivot_y,
      ols_slope = unname(coef(ols)["x"]),
      ols_lo = unname(confint(ols)["x", 1]), ols_hi = unname(confint(ols)["x", 2]),
      median_sx = median(d$sx), sd_x = sd(d$x),
      median_sy = median(d$sy), sd_y = sd(d$y),
      iterations = yk$iterations
    )
  }) |>
  ungroup() |>
  mutate(drug = DRUG_LABELS[drug])

print(results |> mutate(across(where(is.double), \(v) round(v, 3))), n = Inf)


# =============================================================================
# 4. Sanity check: York reduces to OLS when x-error is negligible
# =============================================================================

sanity <- dat |>
  group_by(drug, evolution_history) |>
  group_modify(\(d, k) {
    york0 <- york_fit(d$x, d$y, sx = rep(1e-6, nrow(d)), sy = d$sy)
    tibble(york_slope_x_exact = york0$slope,
           wls_slope = unname(coef(lm(y ~ x, data = d, weights = 1 / d$sy^2))["x"]))
  }) |>
  ungroup()

cat("\nSanity check (x-error ~ 0 should match WLS with weights 1/sy^2), max abs difference:",
    format(max(abs(sanity$york_slope_x_exact - sanity$wls_slope)), digits = 3), "\n")

write_csv(results, file.path(OUT, "york-regression-39.csv"))


# =============================================================================
# 5. Plots
# =============================================================================

FIGS <- "figures"
EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF")

dat_p <- dat |>
  mutate(drug_l = factor(DRUG_LABELS[drug], levels = DRUG_LABELS))

res_p <- results |>
  mutate(drug_l = factor(names(DRUG_LABELS)[match(drug, DRUG_LABELS)], levels = names(DRUG_LABELS)),
         drug_l = factor(DRUG_LABELS[as.character(drug_l)], levels = DRUG_LABELS))

rng <- dat_p |>
  group_by(drug_l, evolution_history) |>
  summarise(x1 = min(x), x2 = max(x), .groups = "drop")

york_seg <- res_p |>
  left_join(rng, by = c("drug_l", "evolution_history")) |>
  transmute(drug_l, evolution_history,
            x1, x2,
            y1 = york_intercept + york_slope * x1,
            y2 = york_intercept + york_slope * x2)

theme_y <- theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), legend.position = "bottom")

# 95% CI band for each York line. The slope CI is pivoted at the York
# weighted centroid, so the band half-width at x is (CI width / 2) * |x - x_centroid|.
# 95% CI band for each York line (analytic York standard error). The slope CI
# is pivoted at the York weighted centroid, so the half-width at x is
# (CI width / 2) * |x - x_centroid|.
band_dat <- pmap_dfr(
  list(results$drug, results$evolution_history, results$york_slope,
       results$york_pivot_x, results$york_pivot_y, results$york_hi - results$york_lo),
  \(d, e, b, px, py, w2) {
    r  <- rng |> filter(as.character(drug_l) == d, evolution_history == e)
    xs <- seq(r$x1, r$x2, length.out = 60)
    yc <- py + b * (xs - px)
    hw <- (w2 / 2) * abs(xs - px)
    tibble(drug_l = factor(d, levels = DRUG_LABELS), evolution_history = e,
           x = xs, y = yc, lo = yc - hw, hi = yc + hw)
  })

ggplot(dat_p, aes(x = x, y = y, color = evolution_history)) +
  geom_ribbon(data = band_dat,
              aes(x = x, ymin = lo, ymax = hi, fill = evolution_history,
                  group = evolution_history),
              alpha = 0.2, color = NA, inherit.aes = FALSE) +
  geom_errorbar(aes(ymin = y - sy, ymax = y + sy), width = 0, alpha = 0.35) +
  geom_errorbar(aes(xmin = x - sx, xmax = x + sx), orientation = "y", width = 0, alpha = 0.35) +
  geom_point(size = 2) +
  geom_segment(data = york_seg,
               aes(x = x1, xend = x2, y = y1, yend = y2, color = evolution_history),
               linewidth = 0.8, inherit.aes = FALSE) +
  facet_wrap(~ drug_l, scales = "free") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_fill_manual(values = EVO_COLORS, guide = "none") +
  labs(x = "Ancestor-window mean growth (error bars: TPC bootstrap SE)",
       y = expression(paste(log[2], " IC"[50], " / same-plate ancestor (error bars: SE of strain mean)")),
       caption = "Lines: York regression within each treatment; shaded: 95% CI of the slope. Error bars: x from TPC bootstrap; y from pooled plate SD / sqrt(plates).") +
  theme_y +
  theme(plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "york-scatter-39.png"), width = 10, height = 4.2, dpi = 300, bg = "white")

forest_dat <- results |>
  transmute(drug = factor(drug, levels = DRUG_LABELS), evolution_history,
            method = "OLS", slope = ols_slope, lo = ols_lo, hi = ols_hi) |>
  bind_rows(results |>
              transmute(drug = factor(drug, levels = DRUG_LABELS), evolution_history,
                        method = "York", slope = york_slope, lo = york_lo, hi = york_hi))

# Shared-slope OLS (one slope for both treatments, separate intercepts), as in Script 37
shared_dat <- dat |>
  group_by(drug) |>
  group_modify(\(d, k) {
    m  <- lm(y ~ x + evolution_history, data = d)
    ci <- confint(m)["x", ]
    tibble(slope = coef(m)[["x"]], lo = ci[1], hi = ci[2])
  }) |>
  ungroup() |>
  transmute(drug = factor(DRUG_LABELS[drug], levels = DRUG_LABELS),
            evolution_history = "Shared slope", method = "OLS (shared)", slope, lo, hi)

forest_all <- bind_rows(forest_dat, shared_dat) |>
  mutate(evolution_history = factor(evolution_history,
                                    levels = c("Shared slope", "40 evolved", "35 evolved")))

ggplot(forest_all, aes(x = slope, y = evolution_history, color = method)) +
  geom_vline(xintercept = 0, color = "grey70") +
  geom_pointrange(aes(xmin = lo, xmax = hi),
                  position = position_dodge(width = 0.5), linewidth = 0.7) +
  facet_wrap(~ drug, scales = "free_x") +
  scale_color_manual(values = c(OLS = "grey40", York = "black", "OLS (shared)" = "#6a3d9a"),
                     name = NULL) +
  labs(x = "Slope (log2 IC50 fold change per unit ancestor-window mean growth), 95% CI",
       y = NULL,
       caption = paste("Within treatment: OLS (no error in x or y) and York (error in x from TPC bootstrap; y error from pooled plate SD).",
                       "Shared slope: OLS with one slope for both treatments and separate intercepts (as in Script 37).",
                       sep = "\n")) +
  theme_y +
  theme(plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "york-forest-39.png"), width = 9, height = 3.8, dpi = 300, bg = "white")
