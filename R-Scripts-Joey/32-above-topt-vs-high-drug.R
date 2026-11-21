# =============================================================================
# 32-above-topt-vs-high-drug.R
# Is growth at temperatures above T_opt positively associated with growth at
# high drug concentrations, for each drug?
#
# Thermal (per strain, fitted TPCs from Script 17/19 predictions):
#   mean_above_topt = mean performance above T_opt: integral of fitted growth
#                     from the strain's own T_opt to its own T_max, divided by
#                     (T_max - T_opt) (primary; tracks r_max, r ~ 0.77)
#   auc_above_topt  = the integral itself (secondary; ~ window width, r ~ 0.99)
#   mean_fixed_window = mean fitted growth over the ancestor's T_opt to T_max
#                     (same temperatures for every strain)
# Drug (per strain): mean log2 fold change in relative growth vs same-plate
#   ancestor, log2((rel + 0.05) / (rel_anc + 0.05)), over the four doubling
#   steps above the ancestor's median IC50 for that drug.
#
# Test: drug_high ~ thermal + evolution_history (within-group association);
#   partial r = t / sqrt(t^2 + df). Unadjusted p-values. Run on all strains.
#
# Section 3b: drug metric x thermal metric comparison. Drug metrics: log2 IC50
#   ratio vs same-plate ancestor (strain mean) and high-drug growth (above).
#   Thermal metrics: mean_above_topt, auc_above_topt, T_opt, T_max, T_h. Same model and
#   subsets. Shows whether associations depend on the drug or thermal metric.
#
# Inputs:  data-processed/dr-difference-curves-29.csv
#          data-processed/normalised-ic50-per-strain.csv
#          data-processed/normalised-ic50-per-plate.csv
#          data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv
#          data-processed/gcplyr/tpc-predictions-auc-gcplyr-17.csv
# Outputs: data-processed/above-topt-vs-high-drug-32.csv
#          data-processed/drug-vs-thermal-metric-comparison-32.csv
#          data-processed/ic50-vs-high-drug-32.csv
#          data-processed/ic50-rmax-vs-above-topt-32.csv
#          figures/above-topt-vs-high-drug-32.png
#          figures/ic50-vs-above-topt-32.png
# =============================================================================

library(tidyverse)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF")
EVO_LEVELS <- c("35 evolved", "40 evolved")
DRUG_LABELS <- c(fluconazole = "Fluconazole", caspofungin = "Caspofungin",
                 amphotericin = "Amphotericin B")
N_STEPS <- 4
FIGS <- "figures"
OUT  <- "data-processed"

theme_evo <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(panel.background = element_blank(),
          legend.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          strip.background = element_blank())
}


# =============================================================================
# 1. Growth above T_opt from fitted TPCs
# =============================================================================

tpc_par  <- read_csv(file.path(OUT, "gcplyr/tpc-params-auc-gcplyr-17.csv"), show_col_types = FALSE)
tpc_pred <- read_csv(file.path(OUT, "gcplyr/tpc-predictions-auc-gcplyr-17.csv"), show_col_types = FALSE)

# Trapezoid integral of the fitted curve between T_opt and T_max, with the
# window endpoints added by linear interpolation of the prediction grid.
integrate_window <- function(temp, pred, lo, hi) {
  x <- sort(unique(c(lo, hi, temp[temp > lo & temp < hi])))
  y <- approx(temp, pred, xout = x)$y
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

# Fixed common window: the ancestor's T_opt to T_max (same temperatures for all strains)
anc_window <- tpc_par |> filter(evolution_history == "fRS585") |> select(topt, tmax)
stopifnot(nrow(anc_window) == 1)

thermal <- tpc_pred |>
  filter(evolution_history %in% EVO_LEVELS) |>
  left_join(tpc_par |> select(strain, topt, tmax), by = "strain") |>
  group_by(strain, evolution_history, topt, tmax) |>
  summarise(
    auc_above_topt = integrate_window(temp, pred, first(topt), first(tmax)),
    mean_fixed_window = integrate_window(temp, pred, anc_window$topt, anc_window$tmax) /
      (anc_window$tmax - anc_window$topt),
    .groups = "drop"
  ) |>
  mutate(mean_above_topt = auc_above_topt / (tmax - topt))


# =============================================================================
# 2. Growth at high drug concentrations
# =============================================================================

anc_ic50 <- read_csv(file.path(OUT, "normalised-ic50-per-plate.csv"), show_col_types = FALSE) |>
  group_by(drug) |>
  summarise(anc_ic50 = median(ic50_anc))

curves <- read_csv(file.path(OUT, "dr-difference-curves-29.csv"), show_col_types = FALSE) |>
  select(strain = population, evolution_history, drug, concentration, log2_fc)

high_window <- curves |>
  distinct(drug, concentration) |>
  left_join(anc_ic50, by = "drug") |>
  filter(concentration > anc_ic50) |>
  group_by(drug) |>
  slice_min(concentration, n = N_STEPS) |>
  ungroup()

print(high_window |> group_by(drug) |>
        summarise(anc_ic50 = first(anc_ic50), concs = paste(signif(concentration, 2), collapse = ", ")))

drug_high <- curves |>
  semi_join(high_window, by = c("drug", "concentration")) |>
  group_by(strain, evolution_history, drug) |>
  summarise(high_drug_lfc = mean(log2_fc), .groups = "drop")


# =============================================================================
# 3. Within-group associations
# =============================================================================

assoc_dat <- drug_high |>
  inner_join(thermal, by = c("strain", "evolution_history")) |>
  mutate(evolution_history = factor(evolution_history, levels = EVO_LEVELS))

fit_assoc <- function(d, xvar) {
  m  <- lm(reformulate(c(xvar, "evolution_history"), "high_drug_lfc"), data = d)
  co <- summary(m)$coefficients[xvar, ]
  ci <- confint(m)[xvar, ]
  tibble(n = nrow(d), slope = co[["Estimate"]], ci95_lo = ci[[1]], ci95_hi = ci[[2]],
         r_within = co[["t value"]] / sqrt(co[["t value"]]^2 + m$df.residual),
         p = co[["Pr(>|t|)"]])
}

assoc <- expand_grid(
  subset  = "all",
  thermal = c("auc_above_topt", "mean_above_topt"),
  drug    = names(DRUG_LABELS)
) |>
  mutate(res = pmap(list(subset, thermal, drug), \(s, x, dg) {
    d <- assoc_dat |> filter(drug == dg)
    fit_assoc(d, x)
  })) |>
  unnest(res)

print(assoc |> mutate(across(where(is.double), \(x) round(x, 3))), n = Inf)


# =============================================================================
# 3b. Drug metric (IC50 vs high-drug growth) x thermal metric comparison
# =============================================================================

ic50_strain <- read_csv(file.path(OUT, "normalised-ic50-per-strain.csv"), show_col_types = FALSE) |>
  select(strain = population, drug, log2_ic50 = log_ratio)

cmp_dat <- assoc_dat |>
  select(strain, evolution_history, drug, high_drug_lfc,
         mean_above_topt, auc_above_topt, mean_fixed_window) |>
  left_join(ic50_strain, by = c("strain", "drug")) |>
  left_join(tpc_par |> select(strain, topt, tmax, th_c), by = "strain")

stopifnot(!anyNA(cmp_dat$log2_ic50))

fit_partial <- function(d, y, x) {
  m  <- lm(reformulate(c(x, "evolution_history"), y), data = d)
  co <- summary(m)$coefficients[x, ]
  ci <- confint(m)[x, ]
  tibble(n = nrow(d), slope = co[["Estimate"]], ci95_lo = ci[[1]], ci95_hi = ci[[2]],
         r_within = co[["t value"]] / sqrt(co[["t value"]]^2 + m$df.residual),
         p = co[["Pr(>|t|)"]])
}

metric_comparison <- expand_grid(
  drug        = names(DRUG_LABELS),
  drug_metric = c("log2_ic50", "high_drug_lfc"),
  thermal     = c("mean_above_topt", "auc_above_topt", "mean_fixed_window", "topt", "tmax", "th_c"),
  subset      = "all"
) |>
  mutate(res = pmap(list(drug, drug_metric, thermal, subset), \(dg, y, x, s) {
    d <- cmp_dat |> filter(drug == dg)
    fit_partial(d, y, x)
  })) |>
  unnest(res)

# Readable summary: r (p) per cell
metric_comparison |>
  mutate(cell = sprintf("%.2f (%.3f)", r_within, p)) |>
  select(drug, thermal, drug_metric, subset, cell) |>
  pivot_wider(names_from = c(drug_metric, subset), values_from = cell) |>
  arrange(drug, thermal) |>
  print(n = Inf)

# How related are the two drug metrics? (within-group partial r)
ic50_vs_high <- cmp_dat |>
  group_by(drug) |>
  group_modify(\(d, k) fit_partial(d, "high_drug_lfc", "log2_ic50")) |>
  ungroup()

print(ic50_vs_high)


# =============================================================================
# 3c. Does mean performance above T_opt add anything beyond r_max?
# =============================================================================
# log2_ic50 ~ r_max + group; ~ mean_above_topt + group; and both together.
# r_max and mean_above_topt are correlated within groups (r = 0.77),
# so the joint model shows which carries the association.

rmax_dat <- cmp_dat |> left_join(tpc_par |> select(strain, rmax), by = "strain")

partial_r <- function(m, x) {
  co <- summary(m)$coefficients[x, ]
  tibble(term = x, r_within = co[["t value"]] / sqrt(co[["t value"]]^2 + m$df.residual),
         p = co[["Pr(>|t|)"]])
}

rmax_check <- expand_grid(drug = names(DRUG_LABELS), subset = "all") |>
  mutate(res = map2(drug, subset, \(dg, s) {
    d <- rmax_dat |> filter(drug == dg)
    m_both <- lm(log2_ic50 ~ mean_above_topt + rmax + evolution_history, data = d)
    bind_rows(
      partial_r(lm(log2_ic50 ~ rmax + evolution_history, data = d), "rmax") |>
        mutate(model = "rmax alone"),
      partial_r(lm(log2_ic50 ~ mean_above_topt + evolution_history, data = d), "mean_above_topt") |>
        mutate(model = "mean_above_topt alone"),
      partial_r(m_both, "mean_above_topt") |> mutate(model = "both"),
      partial_r(m_both, "rmax") |> mutate(model = "both")
    )
  })) |>
  unnest(res)

rmax_check |>
  mutate(cell = sprintf("%.2f (%.3f)", r_within, p), col = paste(model, term, sep = ": ")) |>
  select(drug, subset, col, cell) |>
  pivot_wider(names_from = col, values_from = cell) |>
  print()


# =============================================================================
# 4. Plot (both thermal summaries vs high-drug growth)
# =============================================================================

THERMAL_LABELS <- c(auc_above_topt  = "Area~above~T[opt]~(breadth)",
                    mean_above_topt = "Mean~growth~above~T[opt]~(height)")

assoc_dat |>
  pivot_longer(c(auc_above_topt, mean_above_topt), names_to = "thermal", values_to = "x") |>
  mutate(drug = factor(drug, levels = names(DRUG_LABELS), labels = DRUG_LABELS),
         thermal = factor(thermal, levels = names(THERMAL_LABELS), labels = THERMAL_LABELS)) |>
  ggplot(aes(x = x, y = high_drug_lfc, color = evolution_history)) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_point(size = 2, alpha = 0.85) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.6) +
  facet_grid(drug ~ thermal, scales = "free", labeller = labeller(thermal = label_parsed)) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(x = expression(paste("Growth above ", T[opt], " (fitted TPC, ", T[opt], " to ", T[max], ")")),
       y = expression(paste("Growth at high drug (mean ", log[2], " FC vs ancestor)")),
       caption = paste(
         "High drug = four doubling steps above the ancestor's median IC50 for each drug.",
         "Area above T_opt tracks window width (T_max - T_opt); mean growth above T_opt tracks r_max.",
         sep = "\n")) +
  theme_evo(11) +
  theme(legend.position = "bottom", plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "above-topt-vs-high-drug-32.png"),
       width = 8, height = 9, dpi = 300, bg = "transparent")


# =============================================================================
# 4b. Plot: IC50 vs supra-optimal mean and supra-optimal AUC
# =============================================================================
# Rows = supra-optimal metric, columns = drug. Lines: shared-slope model
# log2_ic50 ~ metric + evolution_history (parallel lines, one per group) -
# the model behind the reported r values.

SUPRA_METRICS <- c(mean_above_topt   = "Supra-optimal mean (growth units)",
                   auc_above_topt    = "Supra-optimal AUC (growth \u00d7 \u00b0C)",
                   mean_fixed_window = "Mean, ancestor window (growth units)")

ic50_long <- cmp_dat |>
  pivot_longer(names(SUPRA_METRICS), names_to = "thermal", values_to = "x")

# Fitted lines with 95% confidence bands (predict, interval = "confidence")
ic50_fit_lines <- ic50_long |>
  group_by(drug, thermal) |>
  group_modify(\(d, k) {
    m  <- lm(log2_ic50 ~ x + evolution_history, data = d)
    nd <- d |>
      group_by(evolution_history) |>
      summarise(x = list(seq(min(x), max(x), length.out = 50)), .groups = "drop") |>
      unnest(x)
    pr <- predict(m, newdata = nd, interval = "confidence")
    nd |> mutate(pred = pr[, "fit"], lo = pr[, "lwr"], hi = pr[, "upr"])
  }) |>
  ungroup()

# Shared-slope coefficient on the metric with its 95% CI
ic50_slopes <- ic50_long |>
  group_by(drug, thermal) |>
  group_modify(\(d, k) {
    m  <- lm(log2_ic50 ~ x + evolution_history, data = d)
    ci <- confint(m)["x", ]
    tibble(slope_txt = sprintf("slope = %.2f [%.2f, %.2f]", coef(m)[["x"]], ci[1], ci[2]))
  }) |>
  ungroup()

ic50_labels <- metric_comparison |>
  filter(drug_metric == "log2_ic50", thermal %in% names(SUPRA_METRICS)) |>
  select(drug, thermal, subset, r_within, p) |>
  mutate(txt = sprintf("%s: r = %.2f, p = %.3f",
                       "all strains", r_within, p)) |>
  group_by(drug, thermal) |>
  summarise(label = paste(txt, collapse = "\n"), .groups = "drop") |>
  left_join(ic50_slopes, by = c("drug", "thermal")) |>
  mutate(label = paste0(slope_txt, "\n", label))

relabel <- \(d) mutate(d,
  drug    = factor(drug, levels = names(DRUG_LABELS), labels = DRUG_LABELS),
  thermal = factor(thermal, levels = names(SUPRA_METRICS), labels = SUPRA_METRICS))

ggplot(relabel(ic50_long), aes(x = x, y = log2_ic50, color = evolution_history)) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_ribbon(data = relabel(ic50_fit_lines),
              aes(x = x, ymin = lo, ymax = hi, fill = evolution_history, group = evolution_history),
              alpha = 0.2, color = NA, inherit.aes = FALSE) +
  geom_line(data = relabel(ic50_fit_lines), aes(y = pred), linewidth = 0.7) +
  geom_point(size = 2.2, alpha = 0.85) +
  geom_text(data = relabel(ic50_labels), aes(label = label), x = -Inf, y = Inf,
            hjust = -0.05, vjust = 1.2, size = 3, color = "black", inherit.aes = FALSE) +
  facet_grid(drug ~ thermal, scales = "free", switch = "x") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.4))) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_fill_manual(values = EVO_COLORS, guide = "none") +
  labs(
    x = NULL,
    y = expression(paste(log[2], "(", IC[50], " / same-plate ancestor ", IC[50], ")")),
    caption = paste(
      "Supra-optimal AUC = integral of fitted TPC from the strain's T_opt to T_max; mean = AUC / (T_max - T_opt).",
      sprintf("Ancestor window = mean fitted growth from %.1f to %.1f \u00b0C (ancestor T_opt to T_max) for every strain.",
              anc_window$topt, anc_window$tmax),
      "Lines: log2 IC50 ~ metric + evolution history (shared slope); bands: 95% CI of fitted lines.",
      "Slope: change in log2 IC50 per unit of metric, 95% CI.",
      "r = within-group partial correlation; unadjusted p-values.",
      sep = "\n")
  ) +
  theme_evo(11) +
  theme(legend.position = "bottom", strip.placement = "outside",
        plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "ic50-vs-above-topt-32.png"),
       width = 12, height = 10, dpi = 300, bg = "transparent")


# =============================================================================
# 5. Export
# =============================================================================

write_csv(assoc, file.path(OUT, "above-topt-vs-high-drug-32.csv"))
write_csv(metric_comparison, file.path(OUT, "drug-vs-thermal-metric-comparison-32.csv"))
write_csv(ic50_vs_high, file.path(OUT, "ic50-vs-high-drug-32.csv"))
write_csv(rmax_check, file.path(OUT, "ic50-rmax-vs-above-topt-32.csv"))
