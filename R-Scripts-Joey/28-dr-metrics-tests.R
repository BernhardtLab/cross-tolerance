# =============================================================================
# 28-dr-metrics-tests.R
# Exploratory tests of the additional dose-response metrics from Script 27,
# all on the log2 fold-change scale relative to the same-plate ancestor:
#   (a) 40 vs 35 evolved (Welch t-test on strain means; 95% CI of difference)
#   (b) change-on-change regressions vs thermal traits, as in Script 26:
#       scale(metric) ~ scale(d_thermal) + evolution_history; one-sided slope > 0
# P-values are unadjusted (no multiple-comparison correction).
# IC50 is included for reference (already reported in Scripts 25-26).
#
# Inputs:  data-processed/dr-metrics-per-strain-27.csv
#          data-processed/quadrant-changes-24.csv (thermal changes)
# Outputs: data-processed/dr-metrics-group-tests-28.csv
#          data-processed/dr-metrics-regressions-28.csv
#          data-processed/dr-fluconazole-month-adjusted-28.csv
#          figures/dr-metrics-28.png
# =============================================================================

library(tidyverse)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF")
EVO_LEVELS <- c("35 evolved", "40 evolved")
DRUG_LABELS <- c(fluconazole = "Fluconazole", caspofungin = "Caspofungin",
                 amphotericin = "Amphotericin B")
# All metrics as log2 fold change vs same-plate ancestor (Script 27)
METRICS <- c(log2_ic50 = "log2 FC IC50", log2_ic90 = "log2 FC IC90",
             lfc_smg24 = "log2 FC SMG (24 h)", lfc_resid = "log2 FC residual growth",
             lfc_auc = "log2 FC AUC")
FIGS <- "figures"
OUT  <- "data-processed"

theme_evo <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(panel.background = element_blank(),
          legend.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          strip.background = element_blank())
}

per_strain <- read_csv(file.path(OUT, "dr-metrics-per-strain-27.csv"), show_col_types = FALSE)

long <- per_strain |>
  pivot_longer(all_of(names(METRICS)), names_to = "metric", values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(evolution_history = factor(evolution_history, levels = EVO_LEVELS))


# =============================================================================
# 1. 40 vs 35 evolved
# =============================================================================

group_tests <- long |>
  group_by(metric, drug) |>
  group_modify(\(d, k) {
    x40 <- d$value[d$evolution_history == "40 evolved"]
    x35 <- d$value[d$evolution_history == "35 evolved"]
    tt  <- t.test(x40, x35)
    tibble(n_35 = length(x35), n_40 = length(x40),
           diff_40_35 = mean(x40) - mean(x35),
           ci95_lo = tt$conf.int[1], ci95_hi = tt$conf.int[2],
           t = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value)
  }) |>
  ungroup()

print(group_tests |> mutate(across(where(is.double), \(x) round(x, 3))), n = Inf)


# =============================================================================
# 2. Change-on-change regressions vs thermal traits
# =============================================================================

thermal <- read_csv(file.path(OUT, "quadrant-changes-24.csv"), show_col_types = FALSE) |>
  distinct(strain, trait, d_th)

reg_input <- long |>
  inner_join(thermal, by = c("population" = "strain"), relationship = "many-to-many")

fit_slope <- function(d) {
  d <- d |> mutate(x = as.numeric(scale(d_th)), y = as.numeric(scale(value)))
  m <- lm(y ~ x + evolution_history, data = d)
  co <- summary(m)$coefficients["x", ]
  ci <- confint(m)["x", ]
  tibble(n = nrow(d), slope = co[["Estimate"]], ci95_lo = ci[[1]], ci95_hi = ci[[2]],
         p_pos = pt(co[["t value"]], m$df.residual, lower.tail = FALSE))
}

regressions <- reg_input |>
  group_by(metric, drug, trait) |>
  group_modify(\(d, k) fit_slope(d)) |>
  ungroup()

print(regressions |> arrange(p_pos) |> mutate(across(where(is.double), \(x) round(x, 3))), n = 15)


# =============================================================================
# 2b. Fluconazole tolerance: month confound check
# =============================================================================
# Most 40-evolved fluconazole plates were run in February, all 35-evolved in
# March (6 x 40-evolved strains also in March). Plate-level mixed model with
# month as a covariate and strain as a random intercept.

library(lme4)
library(lmerTest)

per_plate <- read_csv(file.path(OUT, "dr-metrics-per-plate-27.csv"), show_col_types = FALSE)

fl_mm <- per_plate |>
  filter(drug == "fluconazole") |>
  mutate(evolution_history = factor(evolution_history, levels = EVO_LEVELS))

fl_month_models <- map_dfr(c("lfc_resid", "lfc_smg24"), \(v) {
  m <- lmer(reformulate(c("evolution_history", "month", "(1 | population)"), v), data = fl_mm)
  broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) |>
    filter(term != "(Intercept)") |>
    mutate(response = v, .before = 1)
})

print(fl_month_models)
write_csv(fl_month_models, file.path(OUT, "dr-fluconazole-month-adjusted-28.csv"))


# =============================================================================
# 3. Plot: strain-level metrics by group
# =============================================================================

long |>
  mutate(metric = factor(metric, levels = names(METRICS), labels = METRICS),
         drug = factor(drug, levels = names(DRUG_LABELS), labels = DRUG_LABELS)) |>
  ggplot(aes(x = evolution_history, y = value, color = evolution_history)) +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_jitter(width = 0.12, height = 0, size = 1.6, alpha = 0.8) +
  stat_summary(fun.data = mean_se, color = "black", size = 0.4) +
  facet_grid(metric ~ drug, scales = "free_y", switch = "y") +
  scale_color_manual(values = EVO_COLORS, guide = "none") +
  labs(x = NULL, y = NULL,
       caption = "Change relative to same-plate ancestor; points = strains; black = mean \u00b1 SE.") +
  theme_evo(11) +
  theme(strip.placement = "outside", plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "dr-metrics-28.png"), width = 8, height = 10, dpi = 300, bg = "transparent")


# =============================================================================
# 4. Export
# =============================================================================

write_csv(group_tests, file.path(OUT, "dr-metrics-group-tests-28.csv"))
write_csv(regressions, file.path(OUT, "dr-metrics-regressions-28.csv"))
