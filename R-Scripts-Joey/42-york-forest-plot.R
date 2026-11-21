# =============================================================================
# 42-york-forest-plot.R
# Forest plot of ancestor-window slopes with the replicate-based York results
# from Script 40. Same layout as york-forest-39.png, so the two can be compared:
#   within-treatment OLS (no error in x), York (error in x and y),
#   and shared-slope OLS (one slope for both treatments).
#
# Reads saved outputs; the bootstrap is not rerun.
#   data-processed/york-regression-40.csv   (York, replicate-based x error)
#   data-processed/tpc-window-boot-40.csv   (replicate SE of the window mean)
#   data-processed/gcplyr/tpc-predictions-auc-gcplyr-17.csv
#   data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv
#   data-processed/normalised-ic50-per-plate.csv
#
# Outputs: figures/york-forest-42.png
# =============================================================================

library(tidyverse)

OUT  <- "data-processed"
FIGS <- "figures"
EVO_LEVELS  <- c("35 evolved", "40 evolved")
DRUG_LABELS <- c(fluconazole = "Fluconazole", caspofungin = "Caspofungin",
                 amphotericin = "Amphotericin B")

integrate_window <- function(temp, pred, lo, hi) {
  x <- sort(unique(c(lo, hi, temp[temp > lo & temp < hi])))
  y <- approx(temp, pred, xout = x)$y
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

# ---- Data (same construction as Scripts 40 and 41) --------------------------
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

# ---- Slopes and intervals ---------------------------------------------------
ols_within <- dat |>
  group_by(drug, evolution_history) |>
  group_modify(\(d, k) {
    m  <- lm(y ~ x, data = d)
    ci <- confint(m)["x", ]
    tibble(slope = coef(m)[["x"]], lo = ci[1], hi = ci[2])
  }) |>
  ungroup() |>
  transmute(drug = DRUG_LABELS[drug], evolution_history, method = "OLS", slope, lo, hi)

ols_shared <- dat |>
  group_by(drug) |>
  group_modify(\(d, k) {
    m  <- lm(y ~ x + evolution_history, data = d)
    ci <- confint(m)["x", ]
    tibble(slope = coef(m)[["x"]], lo = ci[1], hi = ci[2])
  }) |>
  ungroup() |>
  transmute(drug = DRUG_LABELS[drug], evolution_history = "Shared slope",
            method = "OLS (shared)", slope, lo, hi)

york <- read_csv(file.path(OUT, "york-regression-40.csv"), show_col_types = FALSE) |>
  filter(x_error == "replicate SD (Script 40)") |>
  transmute(drug, evolution_history, method = "York",
            slope = york_slope, lo = york_lo, hi = york_hi)

forest <- bind_rows(ols_within, york, ols_shared) |>
  mutate(drug = factor(drug, levels = DRUG_LABELS),
         evolution_history = factor(evolution_history,
                                    levels = c("Shared slope", "40 evolved", "35 evolved")))

# ---- Plot -------------------------------------------------------------------
ggplot(forest, aes(x = slope, y = evolution_history, color = method)) +
  geom_vline(xintercept = 0, color = "grey70") +
  geom_pointrange(aes(xmin = lo, xmax = hi),
                  position = position_dodge(width = 0.5), linewidth = 0.7) +
  facet_wrap(~ drug, scales = "free_x") +
  scale_color_manual(values = c(OLS = "grey40", York = "black", "OLS (shared)" = "#6a3d9a"),
                     name = NULL) +
  labs(
    x = "Slope (log2 IC50 fold change per unit ancestor-window mean growth), 95% CI",
    y = NULL,
    caption = paste(
      "Within treatment: OLS (no error in x or y) and York (replicate-based x error from Script 40; y error from pooled plate SD).",
      "Shared slope: OLS with one slope for both treatments and separate intercepts (as in Script 37).",
      sep = "\n")
  ) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), legend.position = "bottom",
        plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "york-forest-42.png"), width = 9, height = 3.8, dpi = 300, bg = "white")
