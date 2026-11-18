# =============================================================================
# 29-fpca.R
# Function-valued trait analysis of dose-response curves: discretised
# functional PCA of ancestor-normalised curves, per drug.
#
# Curve per plate: log2 fold change in relative growth (OD / zero-drug OD) vs
# the same-sheet ancestor at each of the 11 non-zero concentrations:
#   log2((rel_strain + EPS) / (rel_anc + EPS)), EPS = 0.05 (as in Script 30).
# 0 = identical to ancestor. Averaged across plates per strain -> 36 strains x
# 11 log2-spaced concentrations per drug. All plates use the same grid, so PCA
# on the grid is a discretised FPCA (centred, unscaled: same log2 FC scale).
# The difference-scale curve (d_rel) is also exported for Script 31.
#
# Inputs:  data-processed/dr-relative-growth-27.csv (from Script 27)
# Outputs: data-processed/fpca-scores-29.csv
#          data-processed/fpca-loadings-29.csv
#          data-processed/fpca-variance-29.csv
#          data-processed/fpca-group-tests-29.csv
#          data-processed/dr-difference-curves-29.csv (strain x concentration)
#          figures/fpca-modes-29.png, figures/fpca-scores-29.png
# =============================================================================

library(tidyverse)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")
EVO_LEVELS <- c("35 evolved", "40 evolved")
DRUG_LABELS <- c(fluconazole = "Fluconazole", caspofungin = "Caspofungin",
                 amphotericin = "Amphotericin B")
N_PC  <- 3
EPS   <- 0.05
FIGS  <- "figures"
OUT   <- "data-processed"
SHEET_KEYS <- c("drug", "month", "set", "rep")

theme_evo <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(panel.background = element_blank(),
          legend.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          strip.background = element_blank())
}


# =============================================================================
# 1. Ancestor-normalised difference curves
# =============================================================================

rel <- read_csv(file.path(OUT, "dr-relative-growth-27.csv"), show_col_types = FALSE) |>
  filter(concentration > 0)

anc_rel <- rel |>
  filter(population == "fRS585") |>
  select(all_of(SHEET_KEYS), concentration, rel_anc = rel_growth)

diff_curves <- rel |>
  filter(population != "fRS585") |>
  left_join(anc_rel, by = c(SHEET_KEYS, "concentration")) |>
  mutate(
    log2_fc = log2((rel_growth + EPS) / (rel_anc + EPS)),
    d_rel   = rel_growth - rel_anc
  ) |>
  group_by(population, evolution_history, drug, concentration) |>
  summarise(log2_fc = mean(log2_fc), d_rel = mean(d_rel), n_plates = n(), .groups = "drop") |>
  mutate(evolution_history = factor(evolution_history, levels = EVO_LEVELS))

write_csv(diff_curves, file.path(OUT, "dr-difference-curves-29.csv"))


# =============================================================================
# 2. FPCA per drug
# =============================================================================

run_fpca <- function(d) {
  wide <- d |>
    select(population, evolution_history, concentration, log2_fc) |>
    pivot_wider(names_from = concentration, values_from = log2_fc) |>
    arrange(population)
  conc <- as.numeric(colnames(wide)[-(1:2)])
  X    <- as.matrix(wide[, -(1:2)])
  pca  <- prcomp(X, center = TRUE, scale. = FALSE)
  ev   <- pca$sdev^2
  list(
    scores = as_tibble(pca$x[, 1:N_PC]) |>
      mutate(population = wide$population, evolution_history = wide$evolution_history, .before = 1),
    loadings = as_tibble(pca$rotation[, 1:N_PC]) |>
      mutate(concentration = conc, mean_curve = pca$center, .before = 1),
    variance = tibble(pc = paste0("PC", seq_along(ev)), var = ev, pct = 100 * ev / sum(ev))
  )
}

fpca <- diff_curves |>
  group_by(drug) |>
  group_map(\(d, k) run_fpca(d) |> map(\(x) mutate(x, drug = k$drug, .before = 1))) |>
  transpose() |>
  map(bind_rows)

fpca_scores   <- fpca$scores
fpca_loadings <- fpca$loadings
fpca_variance <- fpca$variance

print(fpca_variance |> filter(pc %in% paste0("PC", 1:N_PC)) |> mutate(pct = round(pct, 1)))


# =============================================================================
# 3. Group comparison of scores (40 vs 35; Welch; unadjusted p-values)
# =============================================================================

fpca_tests <- fpca_scores |>
  pivot_longer(starts_with("PC"), names_to = "pc", values_to = "score") |>
  group_by(drug, pc) |>
  group_modify(\(d, k) {
    tt <- t.test(d$score[d$evolution_history == "40 evolved"],
                 d$score[d$evolution_history == "35 evolved"])
    tibble(diff_40_35 = unname(tt$estimate[1] - tt$estimate[2]),
           ci95_lo = tt$conf.int[1], ci95_hi = tt$conf.int[2], p = tt$p.value)
  }) |>
  ungroup() |>
  left_join(fpca_variance, by = c("drug", "pc"))

print(fpca_tests |> mutate(across(where(is.double), \(x) round(x, 3))))


# =============================================================================
# 4. Plots
# =============================================================================

# Modes of variation: mean curve +/- 2 SD of scores along each PC
score_sd <- fpca_variance |> mutate(sd = sqrt(var)) |> select(drug, pc, sd)

modes <- fpca_loadings |>
  pivot_longer(starts_with("PC"), names_to = "pc", values_to = "loading") |>
  left_join(score_sd, by = c("drug", "pc")) |>
  left_join(fpca_variance |> select(drug, pc, pct), by = c("drug", "pc")) |>
  mutate(plus = mean_curve + 2 * sd * loading,
         minus = mean_curve - 2 * sd * loading,
         drug = factor(drug, levels = names(DRUG_LABELS), labels = DRUG_LABELS),
         pc_lab = sprintf("%s (%.0f%%)", pc, pct))

ggplot(modes, aes(x = concentration)) +
  geom_hline(yintercept = 0, color = "grey75") +
  geom_line(aes(y = mean_curve), linewidth = 0.8) +
  geom_line(aes(y = plus), linetype = "dashed", color = "grey35") +
  geom_line(aes(y = minus), linetype = "dotted", color = "grey35") +
  geom_text(data = modes |> distinct(drug, pc, pc_lab),
            aes(label = pc_lab), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.3, size = 3.3) +
  facet_grid(pc ~ drug, scales = "free_x") +
  scale_x_log10() +
  labs(x = "Concentration (\u00b5g/mL)",
       y = expression(paste(log[2], " FC relative growth vs ancestor")),
       caption = "Solid: mean difference curve; dashed / dotted: mean \u00b1 2 SD of PC scores.") +
  theme_evo(11) +
  theme(plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "fpca-modes-29.png"), width = 9, height = 7.5, dpi = 300, bg = "transparent")

ggplot(fpca_scores |> mutate(drug = factor(drug, levels = names(DRUG_LABELS), labels = DRUG_LABELS)),
       aes(x = PC1, y = PC2, color = evolution_history)) +
  geom_hline(yintercept = 0, color = "grey85") +
  geom_vline(xintercept = 0, color = "grey85") +
  geom_point(size = 2, alpha = 0.85) +
  facet_wrap(~ drug, scales = "free") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  theme_evo(11) +
  theme(legend.position = "bottom")

ggsave(file.path(FIGS, "fpca-scores-29.png"), width = 9, height = 3.8, dpi = 300, bg = "transparent")


# =============================================================================
# 5. Export
# =============================================================================

write_csv(fpca_scores,   file.path(OUT, "fpca-scores-29.csv"))
write_csv(fpca_loadings, file.path(OUT, "fpca-loadings-29.csv"))
write_csv(fpca_variance, file.path(OUT, "fpca-variance-29.csv"))
write_csv(fpca_tests,    file.path(OUT, "fpca-group-tests-29.csv"))
