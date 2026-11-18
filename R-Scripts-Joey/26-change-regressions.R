# =============================================================================
# 26-change-regressions.R
# Regressions of change in drug tolerance on change in thermal traits
# (relative to the ancestor fRS585), following Fig. 5 / Tables S1-S3 in
# Bernhardt et al. 2020 Phil Trans B.
#
# Model (one per thermal trait x drug):
#   scale(d_drug) ~ scale(d_thermal) + evolution_history
# Evolution history is a fixed effect (analogous to ancestor ID in the paper),
# so slopes reflect within-group covariation, not the between-group
# difference in thermal traits. Variables are scaled across all 36 strains.
#
# Test of a positive slope: cross-tolerance predicts slope > 0 (a priori
# direction), so a one-sided t-test is used. P-values are unadjusted.
#
# Sensitivity: (a) drop March Set2 35-evolved strains (ancestor-dependent
# log-ratios); (b) drop the 4 strains with T_max change > 1 degC (high leverage).
#
# Inputs:  data-processed/quadrant-changes-24.csv      (from Script 24)
#          data-processed/normalised-ic50-per-plate.csv (to identify Set2)
# Outputs: data-processed/change-regressions-26.csv
#          figures/change-regressions-26.png
# =============================================================================


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")
EVO_LEVELS <- c("35 evolved", "40 evolved")
DRUG_LEVELS <- c("Fluconazole", "Caspofungin", "Amphotericin B")
THERMAL <- tribble(
  ~trait, ~label,
  "topt", "Delta~T[opt]~(scaled)",
  "tmax", "Delta~T[max]~(scaled)",
  "th_c", "Delta~T[h]~(scaled)"
)
FIGS <- "figures"
OUT  <- "data-processed"

theme_evo <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(
      panel.background  = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      strip.background  = element_blank()
    )
}


# =============================================================================
# 2. Data
# =============================================================================

changes <- read_csv(file.path(OUT, "quadrant-changes-24.csv"), show_col_types = FALSE) |>
  select(strain, evolution_history, trait, drug, d_th, d_drug) |>
  mutate(
    evolution_history = factor(evolution_history, levels = EVO_LEVELS),
    trait = factor(trait, levels = THERMAL$trait),
    drug  = factor(drug, levels = DRUG_LEVELS)
  )

set2_strains <- read_csv(file.path(OUT, "normalised-ic50-per-plate.csv"),
                         show_col_types = FALSE) |>
  filter(month == "March", set == "Set2", evolution_history == "35 evolved") |>
  distinct(population) |>
  pull()

high_tmax <- changes |>
  filter(trait == "tmax", d_th > 1) |>
  distinct(strain) |>
  pull()


# =============================================================================
# 3. Fit models
# =============================================================================

fit_change <- function(d) {
  d <- d |> mutate(x = as.numeric(scale(d_th)), y = as.numeric(scale(d_drug)))
  m  <- lm(y ~ x + evolution_history, data = d)
  co <- summary(m)$coefficients["x", ]
  ci <- confint(m)["x", ]
  tibble(
    n         = nrow(d),
    slope     = co[["Estimate"]],
    ci95_lo   = ci[[1]],
    ci95_hi   = ci[[2]],
    t         = co[["t value"]],
    df        = m$df.residual,
    p_two     = co[["Pr(>|t|)"]],
    p_pos     = pt(co[["t value"]], m$df.residual, lower.tail = FALSE),
    adj_r2    = summary(m)$adj.r.squared,
    max_cook  = max(cooks.distance(m))
  )
}

fit_set <- function(dat, label) {
  dat |>
    group_by(trait, drug) |>
    group_modify(\(d, k) fit_change(d)) |>
    ungroup() |>
    mutate(analysis = label, .before = 1)
}

reg_main    <- fit_set(changes, "all strains")
reg_no_set2 <- fit_set(changes |> filter(!strain %in% set2_strains), "excl. March Set2")
reg_no_high <- fit_set(changes |> filter(!strain %in% high_tmax), "excl. T_max > 1 degC")

reg_all <- bind_rows(reg_main, reg_no_set2, reg_no_high)

reg_all |>
  select(analysis, trait, drug, n, slope, ci95_lo, ci95_hi, p_pos) |>
  mutate(across(where(is.double), \(x) round(x, 3))) |>
  print(n = Inf)


# =============================================================================
# 4. Partial regression (added-variable) plots
# =============================================================================
# Residualise both axes on evolution history; the OLS slope through these
# residuals equals the model slope for x.

av_dat <- changes |>
  group_by(trait, drug) |>
  mutate(
    x = as.numeric(scale(d_th)),
    y = as.numeric(scale(d_drug)),
    x_res = resid(lm(x ~ evolution_history)),
    y_res = resid(lm(y ~ evolution_history))
  ) |>
  ungroup()

slope_labels <- reg_main |>
  mutate(
    stars = case_when(p_pos < 0.01 ~ "***", p_pos < 0.05 ~ "**", p_pos < 0.1 ~ "*", .default = ""),
    label = sprintf("slope = %.2f%s\n(%.2f, %.2f)", slope, stars, ci95_lo, ci95_hi)
  )

trait_labeller <- as_labeller(setNames(THERMAL$label, THERMAL$trait), label_parsed)

ggplot(av_dat, aes(x = x_res, y = y_res)) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black",
              fill = "grey85", linewidth = 0.7) +
  geom_point(aes(fill = evolution_history), shape = 21, size = 2.2, color = "black", stroke = 0.3) +
  geom_text(data = slope_labels, aes(label = label), x = -Inf, y = Inf,
            hjust = -0.05, vjust = 1.2, size = 3, inherit.aes = FALSE) +
  facet_grid(drug ~ trait, labeller = labeller(trait = trait_labeller)) +
  scale_fill_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x = "Change in thermal trait | evolution history",
    y = "Change in drug tolerance | evolution history (scaled)",
    caption = paste(
      "Partial regression plots from scale(\u0394drug) ~ scale(\u0394thermal) + evolution history.",
      "Slope with 95% CI; one-sided test of slope > 0: *p < 0.1, **p < 0.05, ***p < 0.01 (unadjusted).",
      sep = "\n"
    )
  ) +
  theme_evo() +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "change-regressions-26.png"),
       width = 10, height = 10, dpi = 300, bg = "transparent")


# =============================================================================
# 5. Export
# =============================================================================

write_csv(reg_all, file.path(OUT, "change-regressions-26.csv"))
