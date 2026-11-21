# =============================================================================
# 41-york-vs-ols-plot.R
# Scatter of IC50 fold change vs ancestor-window mean growth, showing both the
# within-treatment OLS fit (no error in x) and the York fit (error in x and y),
# each with its 95% CI ribbon. Uses the replicate-based x error from Script 40.
#
# Reads saved outputs only (no bootstrap rerun):
#   data-processed/tpc-window-boot-40.csv   (replicate SE of the window mean)
#   data-processed/york-regression-40.csv   (York slopes, intercepts, CIs)
#   data-processed/gcplyr/tpc-predictions-auc-gcplyr-17.csv
#   data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv
#   data-processed/normalised-ic50-per-plate.csv
#
# Outputs: figures/york-vs-ols-scatter-41.png
# =============================================================================

library(tidyverse)

OUT  <- "data-processed"
FIGS <- "figures"
EVO_LEVELS  <- c("35 evolved", "40 evolved")
EVO_COLORS  <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF")
DRUG_LABELS <- c(fluconazole = "Fluconazole", caspofungin = "Caspofungin",
                 amphotericin = "Amphotericin B")

integrate_window <- function(temp, pred, lo, hi) {
  x <- sort(unique(c(lo, hi, temp[temp > lo & temp < hi])))
  y <- approx(temp, pred, xout = x)$y
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

# ---- x: ancestor-window mean (point estimate) and its replicate SE ----------
tpc_par  <- read_csv(file.path(OUT, "gcplyr/tpc-params-auc-gcplyr-17.csv"), show_col_types = FALSE)
tpc_pred <- read_csv(file.path(OUT, "gcplyr/tpc-predictions-auc-gcplyr-17.csv"), show_col_types = FALSE)
anc <- tpc_par |> filter(evolution_history == "fRS585") |> select(topt, tmax)
stopifnot(nrow(anc) == 1)

x_point <- tpc_pred |>
  filter(evolution_history %in% EVO_LEVELS) |>
  group_by(strain, evolution_history) |>
  summarise(x = integrate_window(temp, pred, anc$topt, anc$tmax) / (anc$tmax - anc$topt),
            .groups = "drop")

boot <- read_csv(file.path(OUT, "tpc-window-boot-40.csv"), show_col_types = FALSE) |>
  select(strain, evolution_history, sx = se_fixed)

# ---- y: strain mean of plate log ratios, error = pooled plate SD / sqrt(plates)
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

dat <- y_dat |>
  inner_join(x_point, by = c("strain", "evolution_history")) |>
  inner_join(boot, by = c("strain", "evolution_history"))
stopifnot(nrow(dat) == 36 * 3)

# ---- York fits from Script 40 (replicate-based x error) ----------------------
york <- read_csv(file.path(OUT, "york-regression-40.csv"), show_col_types = FALSE) |>
  filter(x_error == "replicate SD (Script 40)") |>
  mutate(drug = names(DRUG_LABELS)[match(drug, DRUG_LABELS)])

# ---- Plot data: points, OLS fit + CI, York line + CI -------------------------
rng <- dat |>
  group_by(drug, evolution_history) |>
  summarise(x1 = min(x), x2 = max(x), .groups = "drop")

ols_band <- dat |>
  group_by(drug, evolution_history) |>
  group_modify(\(d, k) {
    m  <- lm(y ~ x, data = d)
    nd <- tibble(x = seq(min(d$x), max(d$x), length.out = 60))
    pr <- predict(m, newdata = nd, interval = "confidence")
    nd |> mutate(fit = pr[, "fit"], lo = pr[, "lwr"], hi = pr[, "upr"])
  }) |>
  ungroup()

york_band <- york |>
  left_join(rng, by = c("drug", "evolution_history")) |>
  rowwise() |>
  reframe(
    drug = drug, evolution_history = evolution_history,
    x = seq(x1, x2, length.out = 60),
    fit = york_pivot_y + york_slope * (x - york_pivot_x),
    lo = fit - (york_hi - york_lo) / 2 * abs(x - york_pivot_x),
    hi = fit + (york_hi - york_lo) / 2 * abs(x - york_pivot_x)
  ) |>
  ungroup()

york_seg <- york |>
  left_join(rng, by = c("drug", "evolution_history")) |>
  transmute(drug, evolution_history, x1, x2,
            y1 = york_intercept + york_slope * x1,
            y2 = york_intercept + york_slope * x2)

relabel <- \(d) mutate(d, drug = factor(DRUG_LABELS[drug], levels = DRUG_LABELS))

ggplot(relabel(dat), aes(x = x, y = y, color = evolution_history)) +
  geom_ribbon(data = relabel(ols_band),
              aes(x = x, ymin = lo, ymax = hi, group = evolution_history),
              fill = "grey60", alpha = 0.25, color = NA, inherit.aes = FALSE) +
  geom_ribbon(data = relabel(york_band),
              aes(x = x, ymin = lo, ymax = hi, fill = evolution_history, group = evolution_history),
              alpha = 0.2, color = NA, inherit.aes = FALSE) +
  geom_line(data = relabel(ols_band), aes(x = x, y = fit, color = evolution_history),
            linetype = "dashed", linewidth = 0.7, inherit.aes = FALSE) +
  geom_segment(data = relabel(york_seg),
               aes(x = x1, xend = x2, y = y1, yend = y2, color = evolution_history),
               linewidth = 0.9, inherit.aes = FALSE) +
  geom_errorbar(aes(ymin = y - sy, ymax = y + sy), width = 0, alpha = 0.35) +
  geom_errorbar(aes(xmin = x - sx, xmax = x + sx), orientation = "y", width = 0, alpha = 0.35) +
  geom_point(size = 2) +
  facet_wrap(~ drug, scales = "free") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_fill_manual(values = EVO_COLORS, guide = "none") +
  labs(
    x = "Ancestor-window mean growth (error bars: SE across TPC bootstrap replicates)",
    y = expression(paste(log[2], " IC"[50], " fold change (error bars: SE)")),
    caption = paste(
      "Solid lines and coloured bands: York regression (error in x and y), 95% CI of the slope.",
      "Dashed lines and grey bands: within-treatment OLS (no error in x), 95% CI of the fitted line.",
      "y error = pooled plate SD / sqrt(plates); x error = SE from replicate SD (Script 40).",
      sep = "\n")
  ) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), legend.position = "bottom",
        plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "york-vs-ols-scatter-41.png"), width = 10, height = 4.6, dpi = 300, bg = "white")
