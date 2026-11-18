# =============================================================================
# 31-cross-covariance.R
# Function-valued cross-tolerance: within-group correlation between growth
# at each assay temperature (TPC) and ancestor-normalised growth at each drug
# concentration (dose-response), per drug.
#
# Thermal: mean growth (gcplyr AUC; the TPC response used in Scripts 17-19)
#          per strain at 25, 35, 38, 41, 42 degC, averaged over blocks.
# Drug:    strain mean log2 fold-change curves from Script 29:
#          log2((rel_strain + 0.05) / (rel_anc + 0.05)), same plate, at 11
#          concentrations (consistent with Scripts 27-30).
# Both are centred within evolution history before correlating (removes the
# between-group difference, as in the partial RDA / change regressions).
#
# Inference: per-cell permutation p-values (strains permuted within evolution
#   history, 9999 perms). Unadjusted: no multiple-comparison correction.
#
# Inputs:  data-processed/dr-difference-curves-29.csv
#          data-processed/gcplyr/gcplyr-metrics-per-well-17.csv
# Outputs: data-processed/cross-covariance-31.csv
#          data-processed/cross-covariance-fluconazole-robustness-31.csv
#          figures/cross-covariance-31.png
# =============================================================================

library(tidyverse)

DRUG_LABELS <- c(fluconazole = "Fluconazole", caspofungin = "Caspofungin",
                 amphotericin = "Amphotericin B")
EVO_LEVELS <- c("35 evolved", "40 evolved")
N_PERM <- 9999
FIGS   <- "figures"
OUT    <- "data-processed"

theme_evo <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(panel.background = element_blank(),
          legend.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          strip.background = element_blank())
}


# =============================================================================
# 1. Data
# =============================================================================

tpc <- read_csv(file.path(OUT, "gcplyr/gcplyr-metrics-per-well-17.csv"), show_col_types = FALSE) |>
  filter(evolution_history %in% EVO_LEVELS) |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(growth = mean(auc_gc), .groups = "drop")

dr <- read_csv(file.path(OUT, "dr-difference-curves-29.csv"), show_col_types = FALSE) |>
  select(strain = population, evolution_history, drug, concentration, log2_fc)

# Wide matrices (rows = strains in a common order), centred within group
centre_within <- function(wide) {
  wide |>
    group_by(evolution_history) |>
    mutate(across(-strain, \(x) x - mean(x))) |>
    ungroup() |>
    arrange(strain)
}

tpc_w <- tpc |>
  pivot_wider(names_from = test_temperature, values_from = growth) |>
  centre_within()

strains <- tpc_w$strain
grp     <- tpc_w$evolution_history
T_mat   <- as.matrix(tpc_w |> select(-strain, -evolution_history))


# =============================================================================
# 2. Cross-correlation surfaces + within-group permutation p-values
# =============================================================================

perm_within <- function(grp) {
  idx <- seq_along(grp)
  for (g in unique(grp)) {
    w <- which(grp == g)
    idx[w] <- w[sample.int(length(w))]
  }
  idx
}

set.seed(7351)
perm_idx <- replicate(N_PERM, perm_within(grp), simplify = FALSE)

cross_cov <- map_dfr(names(DRUG_LABELS), \(dg) {
  D_w <- dr |>
    filter(drug == dg) |>
    select(-drug) |>
    pivot_wider(names_from = concentration, values_from = log2_fc) |>
    centre_within()
  stopifnot(identical(D_w$strain, strains))
  D_mat <- as.matrix(D_w |> select(-strain, -evolution_history))

  r_obs     <- cor(T_mat, D_mat)
  cell_null <- simplify2array(map(perm_idx, \(i) abs(cor(T_mat, D_mat[i, ]))))
  p_mat     <- (apply(sweep(cell_null, 1:2, abs(r_obs), ">="), 1:2, sum) + 1) / (N_PERM + 1)

  as_tibble(r_obs, rownames = "temperature") |>
    pivot_longer(-temperature, names_to = "concentration", values_to = "r") |>
    left_join(
      as_tibble(p_mat, rownames = "temperature") |>
        pivot_longer(-temperature, names_to = "concentration", values_to = "p"),
      by = c("temperature", "concentration")
    ) |>
    mutate(drug = dg,
           temperature = as.numeric(temperature),
           concentration = as.numeric(concentration),
           .before = 1)
})

cross_cov |>
  group_by(drug) |>
  slice_max(abs(r), n = 3) |>
  mutate(across(where(is.double), \(x) round(x, 3))) |>
  print()

cross_cov |>
  group_by(drug) |>
  summarise(max_abs_r = max(abs(r)), n_cells_p_lt_05 = sum(p < 0.05),
            n_pos = sum(p < 0.05 & r > 0), n_neg = sum(p < 0.05 & r < 0),
            n_cells = n(), mean_r = mean(r)) |>
  print()


# =============================================================================
# 3. Plot
# =============================================================================

ggplot(cross_cov |> mutate(drug = factor(drug, levels = names(DRUG_LABELS), labels = DRUG_LABELS)),
       aes(x = factor(signif(concentration, 2)), y = factor(temperature), fill = r)) +
  geom_tile(color = "white") +
  geom_point(data = \(d) filter(d, p < 0.05), shape = 1, size = 2.5) +
  facet_wrap(~ drug, scales = "free_x") +
  scale_fill_gradient2(low = "#0E63FF", mid = "white", high = "#FA3208",
                       midpoint = 0, limits = c(-0.8, 0.8), name = "r") +
  labs(
    x = "Drug concentration (\u00b5g/mL)",
    y = "Assay temperature (\u00b0C)",
    caption = paste(
      "Within-group correlation between growth at each temperature and log2 fold change in",
      "relative growth vs ancestor at each concentration. Circles: permutation p < 0.05.",
      sep = "\n"
    )
  ) +
  theme_evo(11) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "cross-covariance-31.png"), width = 11, height = 4, dpi = 300, bg = "transparent")


# =============================================================================
# 4. Robustness: fluconazole supra-MIC growth vs overall / heat-specific growth
# =============================================================================
# Supra-MIC growth = mean log2 FC at 32-256 ug/mL. Overall growth = mean
# across temperatures; 42 degC growth (raw); heat-specific = partial
# correlation with 42 degC growth adjusted for 25 degC growth. Subsets
# drop March Set2 35-evolved strains and/or the six 40-evolved strains
# re-tested in March ("40C-ev_Res"; more resistant).

set2_35 <- c("35_D2", "35_D4", "35_D8", "35_E11", "35_E7", "35_E9")
march40 <- c("40_C9", "40_E7", "40_E11", "40_F6", "40_G9", "40_G11")

fl_supra <- dr |>
  filter(drug == "fluconazole", concentration %in% c(32, 64, 128, 256)) |>
  group_by(strain, evolution_history) |>
  summarise(fl_supra = mean(log2_fc), .groups = "drop")

growth_summ <- tpc |>
  group_by(strain, evolution_history) |>
  summarise(
    mean_growth = mean(growth),
    g42 = growth[test_temperature == 42],
    g25 = growth[test_temperature == 25],
    .groups = "drop"
  )

robust_raw <- fl_supra |>
  inner_join(growth_summ, by = c("strain", "evolution_history"))

# Within-group centred copies for simple (zero-order) correlations
robust_dat <- robust_raw |>
  group_by(evolution_history) |>
  mutate(across(c(fl_supra, mean_growth, g42, g25), \(v) v - mean(v))) |>
  ungroup()

# Partial correlation of fluconazole tolerance with 42 degC growth, holding
# 25 degC growth (overall vigour) and evolution history constant:
# fl_supra ~ g42 + g25 + evolution_history; partial r = t / sqrt(t^2 + df).
partial_42 <- function(d) {
  m  <- lm(fl_supra ~ g42 + g25 + evolution_history, data = d)
  tt <- summary(m)$coefficients["g42", "t value"]
  tibble(r_42_given_25 = tt / sqrt(tt^2 + m$df.residual),
         p_42_given_25 = summary(m)$coefficients["g42", "Pr(>|t|)"])
}

fl_robustness <- tibble(
  subset = c("all", "excl. Set2", "excl. March-40", "excl. both"),
  drop   = list(character(0), set2_35, march40, c(set2_35, march40))
) |>
  mutate(
    dat     = map(drop, \(s) filter(robust_dat, !strain %in% s)),
    dat_raw = map(drop, \(s) filter(robust_raw, !strain %in% s)),
    n = map_int(dat, nrow),
    r_mean_growth = map_dbl(dat, \(d) cor(d$fl_supra, d$mean_growth)),
    p_mean_growth = map_dbl(dat, \(d) cor.test(d$fl_supra, d$mean_growth)$p.value),
    r_42 = map_dbl(dat, \(d) cor(d$fl_supra, d$g42)),
    p_42 = map_dbl(dat, \(d) cor.test(d$fl_supra, d$g42)$p.value),
    partial = map(dat_raw, partial_42)
  ) |>
  unnest(partial) |>
  select(-drop, -dat, -dat_raw)

print(fl_robustness)


# =============================================================================
# 4b. Is growth at each temperature higher for strains with higher growth at
#     high fluconazole? growth_T ~ fl_supra + evolution_history, per
#     temperature (within-group association; no adjustment for other temps).
# =============================================================================

temp_dat <- tpc |>
  inner_join(fl_supra, by = c("strain", "evolution_history")) |>
  mutate(retested = strain %in% march40)

fit_temp <- function(d) {
  m  <- lm(growth ~ fl_supra + evolution_history, data = d)
  co <- summary(m)$coefficients["fl_supra", ]
  ci <- confint(m)["fl_supra", ]
  tibble(n = nrow(d), slope = co[["Estimate"]], ci95_lo = ci[[1]], ci95_hi = ci[[2]],
         r_within = co[["t value"]] / sqrt(co[["t value"]]^2 + m$df.residual),
         p = co[["Pr(>|t|)"]])
}

fl_by_temp <- bind_rows(
  temp_dat |> group_by(test_temperature) |> group_modify(\(d, k) fit_temp(d)) |>
    mutate(subset = "all"),
  temp_dat |> filter(!retested) |> group_by(test_temperature) |>
    group_modify(\(d, k) fit_temp(d)) |> mutate(subset = "excl. March-40")
) |>
  ungroup() |>
  relocate(subset)

print(fl_by_temp)

ggplot(temp_dat, aes(x = fl_supra, y = growth, color = evolution_history)) +
  geom_point(aes(shape = retested), size = 2, alpha = 0.85) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.6) +
  facet_wrap(~ test_temperature, nrow = 1, scales = "free_y",
             labeller = labeller(test_temperature = \(x) paste0(x, " \u00b0C"))) +
  scale_color_manual(values = c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF"), name = NULL) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17),
                     labels = c("other strains", "re-tested 40C-ev_Res"), name = NULL) +
  labs(x = expression(paste("Fluconazole tolerance (mean ", log[2], " FC vs ancestor, 32\u2013256 \u00b5g/mL)")),
       y = "Growth (AUC)") +
  theme_evo(11) +
  theme(legend.position = "bottom")

ggsave(file.path(FIGS, "fluconazole-vs-growth-by-temp-31.png"),
       width = 12, height = 3.8, dpi = 300, bg = "transparent")


# =============================================================================
# 5. Export
# =============================================================================

write_csv(cross_cov, file.path(OUT, "cross-covariance-31.csv"))
write_csv(fl_robustness, file.path(OUT, "cross-covariance-fluconazole-robustness-31.csv"))
write_csv(fl_by_temp, file.path(OUT, "fluconazole-vs-growth-by-temp-31.csv"))
