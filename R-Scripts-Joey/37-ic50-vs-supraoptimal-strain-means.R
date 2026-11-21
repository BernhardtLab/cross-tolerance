# =============================================================================
# 37-ic50-vs-supraoptimal-strain-means.R
# IC50 vs supra-optimal figure, with regressions on strain averages.
#
# Steps
#   1. Per plate: log2 IC50 ratio vs the same-plate ancestor
#      (normalised-ic50-per-plate.csv, Script 11d). Plates are normalised to
#      their own ancestor, which handles day-to-day differences in conditions.
#   2. Per strain: simple (unweighted) mean of its plate-level log ratios.
#   3. Thermal metrics from the saved TPC fits (Script 17):
#        mean_above_topt   integral from the strain's T_opt to its T_max / width
#        auc_above_topt    the integral itself
#        mean_fixed_window mean over the ancestor's T_opt to T_max (39.4-42.2 C)
#   4. Per drug x metric: OLS on strain means,
#        log2_ic50 ~ metric + evolution_history
#      Slope and 95% CI; within-group partial r = t / sqrt(t^2 + df); p from t.
#      Fitted lines with 95% CI per evolution group.
#   5. Figure (rows = drugs, columns = metrics). Triangles mark the six
#      re-tested 40-evolved strains. No subset labels.
#   6. Check: the r, p and slope values reproduce Script 32 exactly.
#
# Inputs:  data-processed/normalised-ic50-per-plate.csv
#          data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv
#          data-processed/gcplyr/tpc-predictions-auc-gcplyr-17.csv
#          data-processed/drug-vs-thermal-metric-comparison-32.csv (check only)
# Outputs: figures/ic50-vs-above-topt-37.png
#          data-processed/ic50-vs-supraoptimal-37.csv
# =============================================================================


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)

OUT  <- "data-processed"
FIGS <- "figures"

EVO_COLORS  <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF")
EVO_LEVELS  <- c("35 evolved", "40 evolved")
DRUG_LABELS <- c(fluconazole = "Fluconazole", caspofungin = "Caspofungin",
                 amphotericin = "Amphotericin B")
METRIC_LABELS <- c(mean_above_topt   = "Supra-optimal mean (growth units)",
                   auc_above_topt    = "Supra-optimal AUC (growth \u00d7 \u00b0C)",
                   mean_fixed_window = "Mean, ancestor window (growth units)")
MARCH40 <- c("40_C9", "40_E7", "40_E11", "40_F6", "40_G9", "40_G11")
N_BOOT <- 2000

# Standardized major axis (= reduced major axis) slope: sign(r) * sd(y) / sd(x).
# Descriptive line; does not correct for measurement error.
sma_slope <- function(x, y) sign(cor(x, y)) * sd(y) / sd(x)

theme_evo <- function(base_size = 11) {
  theme_classic(base_size = base_size) +
    theme(panel.background = element_blank(),
          legend.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          strip.background = element_blank(),
          plot.caption = element_text(size = 9, color = "grey40"))
}

# Integral of a fitted curve between lo and hi (trapezoid, endpoints interpolated)
integrate_window <- function(temp, pred, lo, hi) {
  x <- sort(unique(c(lo, hi, temp[temp > lo & temp < hi])))
  y <- approx(temp, pred, xout = x)$y
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}


# =============================================================================
# 2. Strain-level IC50: simple mean of plate-level log ratios
# =============================================================================

ic50_plate <- read_csv(file.path(OUT, "normalised-ic50-per-plate.csv"), show_col_types = FALSE) |>
  filter(evolution_history %in% EVO_LEVELS)

stopifnot(!anyNA(ic50_plate$log_ratio))

ic50_strain <- ic50_plate |>
  group_by(strain = population, evolution_history, drug) |>
  summarise(log2_ic50 = mean(log_ratio), n_plates = n(), .groups = "drop")


# =============================================================================
# 3. Thermal metrics from the fitted TPCs
# =============================================================================

tpc_par  <- read_csv(file.path(OUT, "gcplyr/tpc-params-auc-gcplyr-17.csv"), show_col_types = FALSE)
tpc_pred <- read_csv(file.path(OUT, "gcplyr/tpc-predictions-auc-gcplyr-17.csv"), show_col_types = FALSE)

anc <- tpc_par |> filter(evolution_history == "fRS585") |> select(topt, tmax)
stopifnot(nrow(anc) == 1)
anc_width <- anc$tmax - anc$topt

thermal <- tpc_par |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(strain, evolution_history, topt, tmax) |>
  mutate(
    auc_above_topt = pmap_dbl(list(strain, topt, tmax), \(s, lo, hi) {
      cv <- tpc_pred |> filter(strain == s) |> arrange(temp)
      integrate_window(cv$temp, cv$pred, lo, hi)
    }),
    mean_above_topt = auc_above_topt / (tmax - topt),
    mean_fixed_window = map_dbl(strain, \(s) {
      cv <- tpc_pred |> filter(strain == s) |> arrange(temp)
      integrate_window(cv$temp, cv$pred, anc$topt, anc$tmax) / anc_width
    })
  )

stopifnot(all(ic50_strain$strain %in% thermal$strain))


# =============================================================================
# 4. Strain-level data, reshaped by metric
# =============================================================================

strain_dat <- ic50_strain |>
  inner_join(thermal |> select(strain, evolution_history, all_of(names(METRIC_LABELS))),
             by = c("strain", "evolution_history"))

stopifnot(nrow(strain_dat) == 36 * 3)

strain_long <- strain_dat |>
  pivot_longer(all_of(names(METRIC_LABELS)), names_to = "metric", values_to = "x")


# =============================================================================
# 5. Shared-slope OLS on strain means
# =============================================================================

fit_shared <- function(d) {
  m  <- lm(log2_ic50 ~ x + evolution_history, data = d)
  co <- summary(m)$coefficients["x", ]
  ci <- confint(m)["x", ]
  tt <- co[["t value"]]
  tibble(
    n        = nrow(d),
    slope    = co[["Estimate"]],
    slope_lo = ci[[1]],
    slope_hi = ci[[2]],
    r        = tt / sqrt(tt^2 + m$df.residual),
    p        = co[["Pr(>|t|)"]]
  )
}

fit_lines <- function(d) {
  m  <- lm(log2_ic50 ~ x + evolution_history, data = d)
  nd <- d |>
    group_by(evolution_history) |>
    summarise(x = list(seq(min(x), max(x), length.out = 50)), .groups = "drop") |>
    unnest(x)
  pr <- predict(m, newdata = nd, interval = "confidence")
  nd |> mutate(pred = pr[, "fit"], lo = pr[, "lwr"], hi = pr[, "upr"])
}

models <- strain_long |>
  group_by(drug, metric) |>
  group_modify(\(d, k) fit_shared(d)) |>
  ungroup()

lines_dat <- strain_long |>
  group_by(drug, metric) |>
  group_modify(\(d, k) fit_lines(d)) |>
  ungroup()

labels_dat <- models |>
  mutate(label = sprintf("slope = %.2f [%.2f, %.2f]\nr = %.2f, p = %.3f",
                         slope, slope_lo, slope_hi, r, p))

print(models |> mutate(across(where(is.double), \(v) round(v, 3))))


# =============================================================================
# 6. Figure
# =============================================================================

relabel <- \(d) d |>
  mutate(drug   = factor(drug, levels = names(DRUG_LABELS), labels = DRUG_LABELS),
         metric = factor(metric, levels = names(METRIC_LABELS), labels = METRIC_LABELS))

# RMA/SMA slope per drug x metric x treatment, through the treatment centroid,
# with a strain-bootstrap 95% CI
rma_fit <- strain_long |>
  group_by(drug, metric, evolution_history) |>
  group_modify(\(d, k) {
    bs <- replicate(N_BOOT, {
      s <- d[sample(nrow(d), replace = TRUE), ]
      sma_slope(s$x, s$log2_ic50)
    })
    tibble(rma = sma_slope(d$x, d$log2_ic50),
           rma_lo = unname(quantile(bs, 0.025)), rma_hi = unname(quantile(bs, 0.975)),
           mx = mean(d$x), my = mean(d$log2_ic50), xmin = min(d$x), xmax = max(d$x))
  }) |>
  ungroup()

write_csv(rma_fit, file.path(OUT, "rma-slopes-37.csv"))

rma_seg <- rma_fit |>
  mutate(x1 = xmin, x2 = xmax,
         y1 = my + rma * (xmin - mx), y2 = my + rma * (xmax - mx))

rma_lab <- rma_fit |>
  mutate(txt = sprintf("RMA %s: %.2f [%.2f, %.2f]",
                       if_else(evolution_history == "35 evolved", "35 C", "40 C"),
                       rma, rma_lo, rma_hi)) |>
  group_by(drug, metric) |>
  summarise(txt = paste(txt, collapse = "\n"), .groups = "drop")

ggplot(relabel(strain_long), aes(x = x, y = log2_ic50, color = evolution_history)) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_segment(data = relabel(rma_seg),
               aes(x = x1, xend = x2, y = y1, yend = y2, color = evolution_history),
               linetype = "dashed", linewidth = 0.7, inherit.aes = FALSE) +
  geom_text(data = relabel(rma_lab), aes(x = Inf, y = -Inf, label = txt),
            hjust = 1.05, vjust = -0.3, size = 2.6, color = "black", inherit.aes = FALSE) +
  geom_ribbon(data = relabel(lines_dat),
              aes(x = x, ymin = lo, ymax = hi, fill = evolution_history,
                  group = evolution_history),
              alpha = 0.2, color = NA, inherit.aes = FALSE) +
  geom_line(data = relabel(lines_dat),
            aes(x = x, y = pred, color = evolution_history), linewidth = 0.7,
            inherit.aes = FALSE) +
  geom_point(aes(shape = strain %in% MARCH40), size = 2.2, alpha = 0.85) +
  geom_text(data = relabel(labels_dat), aes(x = -Inf, y = Inf, label = label),
            hjust = -0.05, vjust = 1.2, size = 2.8, color = "black", inherit.aes = FALSE) +
  facet_grid(drug ~ metric, scales = "free", switch = "x") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.4))) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_fill_manual(values = EVO_COLORS, guide = "none") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17),
                     labels = c("other strains", "re-tested 40C-ev_Res"), name = NULL) +
  labs(
    x = NULL,
    y = expression(paste(log[2], "(", IC[50], " / same-plate ancestor ", IC[50], ")")),
    caption = paste(
      "Points: strain means of plate-level log2 IC50 ratios (simple mean of plates).",
      "Supra-optimal AUC = integral of fitted TPC from T_opt to T_max; mean = AUC / (T_max - T_opt).",
      sprintf("Ancestor window = mean fitted growth from %.1f to %.1f C (ancestor T_opt to T_max).",
              anc$topt, anc$tmax),
      "Lines: OLS, log2 IC50 ~ metric + evolution history (shared slope); bands: 95% CI.",
      "Slope: change in log2 IC50 per unit of metric, 95% CI. r = within-group partial correlation; unadjusted p.",
      "Dashed: RMA/SMA slope through each treatment's centroid, bootstrap 95% CI (descriptive; not an error-corrected slope).",
      sep = "\n")
  ) +
  theme_evo(11) +
  theme(legend.position = "bottom", strip.placement = "outside")

ggsave(file.path(FIGS, "ic50-vs-above-topt-37-rma.png"),
       width = 12, height = 10, dpi = 300, bg = "transparent")


# =============================================================================
# 7. Check against Script 32
# =============================================================================

ref <- read_csv(file.path(OUT, "drug-vs-thermal-metric-comparison-32.csv"), show_col_types = FALSE) |>
  filter(subset == "all", drug_metric == "log2_ic50", thermal %in% names(METRIC_LABELS)) |>
  transmute(drug = as.character(drug), metric = thermal,
            r_ref = r_within, p_ref = p, slope_ref = slope)

check <- models |>
  inner_join(ref, by = c("drug", "metric"))

cat("\nMax abs difference vs Script 32 (all strains):\n")
check |>
  summarise(max_dr = max(abs(r - r_ref)), max_dp = max(abs(p - p_ref)),
            max_dslope = max(abs(slope - slope_ref))) |>
  print()


# =============================================================================
# 8. Export
# =============================================================================

write_csv(models, file.path(OUT, "ic50-vs-supraoptimal-37.csv"))
