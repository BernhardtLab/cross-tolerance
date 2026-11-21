# =============================================================================
# 36-ic50-vs-supraoptimal-figure.R
# Self-contained rebuild of the IC50 vs supra-optimal figure, from two inputs:
#   1. normalised-ic50-per-plate.csv  (Script 11d: plate-level log2 IC50 ratios
#                                      vs the same-plate ancestor)
#   2. TPC fits                       (Script 17: parameters and fitted curves)
#
# Model (plate level, because plates were run on different days):
#   log_ratio ~ metric + evolution_history + (1 | strain) + (1 | batch)
#   batch = month x set (plates run together share conditions).
#   Random intercepts for strain (plates from one strain are not independent)
#   and batch (day-to-day conditions). Batch is a random effect rather than a
#   fixed effect because 35 C plates were all run in March while most 40 C
#   plates were run in January/February, so a fixed batch term is nearly
#   collinear with evolution history. Slope and its 95% CI; partial r =
#   t / sqrt(t^2 + df) from the metric term (Satterthwaite df); fitted lines
#   with 95% CI from the fixed effects.
#
# Check (strain-mean model, as in Script 32): mean of plate log ratios per strain,
#   then log2_ic50 ~ metric + evolution_history. Reproduces the Script 32 r, p
#   and slope values, confirming the thermal metrics and IC50 inputs match.
#
# Subset: no subset labels. The six re-tested 40-evolved strains are shown with
# triangles but are not excluded anywhere.
#
# Inputs:  data-processed/normalised-ic50-per-plate.csv
#          data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv
#          data-processed/gcplyr/tpc-predictions-auc-gcplyr-17.csv
#          data-processed/drug-vs-thermal-metric-comparison-32.csv (check only)
# Outputs: figures/ic50-vs-above-topt-36.png
#          data-processed/ic50-vs-supraoptimal-36.csv
# =============================================================================


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)

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
# 2. Plate-level IC50 data
# =============================================================================

ic50_plate <- read_csv(file.path(OUT, "normalised-ic50-per-plate.csv"), show_col_types = FALSE) |>
  filter(evolution_history %in% EVO_LEVELS) |>
  transmute(strain = population, evolution_history, drug, month, set, rep,
            log_ratio, batch = interaction(month, set, drop = TRUE))

stopifnot(!anyNA(ic50_plate$log_ratio))


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

stopifnot(all(ic50_plate$strain %in% thermal$strain))


# =============================================================================
# 4. Plate-level data with thermal metrics, reshaped by metric
# =============================================================================

plate_dat <- ic50_plate |>
  inner_join(thermal |> select(strain, all_of(names(METRIC_LABELS))), by = "strain")

plate_long <- plate_dat |>
  pivot_longer(all_of(names(METRIC_LABELS)), names_to = "metric", values_to = "x")


# =============================================================================
# 5. Models
# =============================================================================

# Plate-level mixed model: strain random intercept, batch fixed effect
fit_plate <- function(d) {
  m  <- lmer(log_ratio ~ x + evolution_history + (1 | strain) + (1 | batch), data = d)
  co <- summary(m)$coefficients["x", ]
  est <- co[["Estimate"]]
  se  <- co[["Std. Error"]]
  df  <- co[["df"]]
  tibble(
    n_plates  = nrow(d),
    n_strains = n_distinct(d$strain),
    slope     = est,
    slope_lo  = est - qt(0.975, df) * se,
    slope_hi  = est + qt(0.975, df) * se,
    r         = co[["t value"]] / sqrt(co[["t value"]]^2 + df),
    p         = co[["Pr(>|t|)"]]
  )
}

models <- plate_long |>
  group_by(drug, metric) |>
  group_modify(\(d, k) fit_plate(d)) |>
  ungroup()

# Fitted lines: fixed-effect predictions over each group's x range, batch fixed
fit_lines <- function(d) {
  m  <- lmer(log_ratio ~ x + evolution_history + (1 | strain) + (1 | batch), data = d)
  nd <- d |>
    group_by(evolution_history) |>
    summarise(x = list(seq(min(x), max(x), length.out = 50)), .groups = "drop") |>
    unnest(x)
  X   <- model.matrix(delete.response(terms(m)), nd)
  fit <- drop(X %*% fixef(m))
  V   <- as.matrix(vcov(m))
  se  <- sqrt(rowSums((X %*% V) * X))
  nd |> mutate(pred = fit, lo = fit - 1.96 * se, hi = fit + 1.96 * se)
}

lines_dat <- plate_long |>
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

ggplot(relabel(plate_long), aes(x = x, y = log_ratio, color = evolution_history)) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_ribbon(data = relabel(lines_dat),
              aes(x = x, ymin = lo, ymax = hi, fill = evolution_history,
                  group = evolution_history),
              alpha = 0.2, color = NA, inherit.aes = FALSE) +
  geom_line(data = relabel(lines_dat),
            aes(x = x, y = pred, color = evolution_history), linewidth = 0.7,
            inherit.aes = FALSE) +
  geom_point(aes(shape = strain %in% MARCH40), size = 1.6, alpha = 0.6) +
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
      "Points: individual plates. Supra-optimal AUC = integral of fitted TPC from T_opt to T_max; mean = AUC / (T_max - T_opt).",
      sprintf("Ancestor window = mean fitted growth from %.1f to %.1f C (ancestor T_opt to T_max).",
              anc$topt, anc$tmax),
      "Lines: plate-level mixed model (random intercepts for strain and batch = month x set); bands: 95% CI.",
      "Slope: change in log2 IC50 per unit of metric, 95% CI. r = within-group partial correlation; unadjusted p.",
      sep = "\n")
  ) +
  theme_evo(11) +
  theme(legend.position = "bottom", strip.placement = "outside")

ggsave(file.path(FIGS, "ic50-vs-above-topt-36.png"),
       width = 12, height = 10, dpi = 300, bg = "transparent")


# =============================================================================
# 7. Check: strain-mean model reproduces Script 32
# =============================================================================

strain_long <- plate_long |>
  group_by(strain, evolution_history, drug, metric) |>
  summarise(log2_ic50 = mean(log_ratio), x = first(x), .groups = "drop")

check_strain <- strain_long |>
  group_by(drug, metric) |>
  group_modify(\(d, k) {
    m  <- lm(log2_ic50 ~ x + evolution_history, data = d)
    co <- summary(m)$coefficients["x", ]
    tt <- co[["t value"]]
    tibble(r = tt / sqrt(tt^2 + m$df.residual), p = co[["Pr(>|t|)"]], slope = co[["Estimate"]])
  }) |>
  ungroup()

ref <- read_csv(file.path(OUT, "drug-vs-thermal-metric-comparison-32.csv"), show_col_types = FALSE) |>
  filter(subset == "all", drug_metric == "log2_ic50", thermal %in% names(METRIC_LABELS)) |>
  transmute(drug = as.character(drug), metric = thermal, r_ref = r_within, p_ref = p, slope_ref = slope)

cat("\nStrain-mean check vs Script 32 (max abs difference):\n")
check_strain |>
  inner_join(ref, by = c("drug", "metric")) |>
  summarise(max_dr = max(abs(r - r_ref)), max_dp = max(abs(p - p_ref)),
            max_dslope = max(abs(slope - slope_ref))) |>
  print()


# =============================================================================
# 8. Export
# =============================================================================

write_csv(models, file.path(OUT, "ic50-vs-supraoptimal-36.csv"))
