# =============================================================================
# 22b-pca.R
# Within-group centred PCA: thermal traits + IC50 log-ratios
#
# Version for the July IC50 file format — plate-level data with one row per
# replicate (columns: population, evolution_history, rep, set, drug, month,
# ic50, bot, top, slope, converged, ic50_anc, log_ratio).
# Aggregates to per-strain means before running the PCA.
#
# Set IC50_FILE below to point to the July CSV.
#
# Inputs:
#   IC50_FILE (set below)                          — July plate-level IC50 file
#   data-processed/gcplyr/tpc-boot-se-19.csv       (from Script 19)
#
# Outputs — data-processed/:
#   pca-scores-within-20.csv
#   pca-loadings-within-20.csv
#   pca-pct-within-20.csv
#
# Outputs — figures/:
#   pca-biplot-within-22b.png
# =============================================================================


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")
EVO_LEVELS <- c("35 evolved", "40 evolved")
FIGS       <- "figures"
OUT        <- "data-processed"

# ── Set path to July IC50 file ────────────────────────────────────────────────
IC50_FILE  <- "data-processed/normalised-ic50-per-plate-july2026.csv"

theme_evo <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(
      panel.background  = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA),
      plot.background   = element_rect(fill = "transparent", colour = NA)
    )
}

PCA_VARS <- c("topt", "tmax", "th_c", "fluconazole", "caspofungin", "amphotericin")


# =============================================================================
# 2. Load data
# =============================================================================

# July format: plate-level, one row per replicate
ic50_plate <- read_csv(IC50_FILE, show_col_types = FALSE)

tpc_se <- read_csv(file.path(OUT, "gcplyr/tpc-boot-se-19.csv"),
                   show_col_types = FALSE)

cat("IC50 plate-level rows:", nrow(ic50_plate), "\n")
cat("TPC strains:", nrow(tpc_se), "\n")


# =============================================================================
# 3. Aggregate to per-strain means
# =============================================================================

ic50_wide <- ic50_plate |>
  filter(evolution_history %in% EVO_LEVELS) |>
  group_by(population, drug) |>
  summarise(log_ratio = mean(log_ratio, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = drug, values_from = log_ratio)

cat("Strains after aggregation:", n_distinct(ic50_wide$population), "\n")


# =============================================================================
# 4. Build PCA input
# =============================================================================

pca_input <- tpc_se |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(strain, evolution_history, topt, tmax, th_c) |>
  inner_join(ic50_wide, by = c("strain" = "population")) |>
  drop_na()

cat(sprintf("\nPCA input: %d strains × %d variables\n", nrow(pca_input), length(PCA_VARS)))
cat("Missing after drop_na:", nrow(tpc_se |> filter(evolution_history %in% EVO_LEVELS)) - nrow(pca_input), "strains\n")


# =============================================================================
# 5. Within-group centred PCA
# =============================================================================

wide_centered <- pca_input |>
  group_by(evolution_history) |>
  mutate(across(all_of(PCA_VARS), ~ . - mean(., na.rm = TRUE))) |>
  ungroup()

pca_c <- prcomp(wide_centered[, PCA_VARS], center = FALSE, scale. = TRUE)

cat("\n=== Within-group PCA: variance explained ===\n")
print(round(summary(pca_c)$importance[, 1:5], 3))

cat("\n=== Within-group PCA: loadings (PC1–PC3) ===\n")
print(round(pca_c$rotation[, 1:3], 3))

pct_c <- summary(pca_c)$importance["Proportion of Variance", ] * 100

scores_c <- as.data.frame(pca_c$x) |>
  bind_cols(pca_input |> select(strain, evolution_history)) |>
  mutate(evolution_history = factor(evolution_history, levels = EVO_LEVELS))

loadings_c <- as.data.frame(pca_c$rotation[, 1:2]) |>
  rownames_to_column("variable") |>
  mutate(
    PC1_scaled = PC1 * max(abs(scores_c$PC1)),
    PC2_scaled = PC2 * max(abs(scores_c$PC2))
  )


# =============================================================================
# 6. Plot
# =============================================================================

ggplot(scores_c, aes(x = PC1, y = PC2, color = evolution_history)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_segment(
    data        = loadings_c,
    aes(x = 0, y = 0, xend = PC1_scaled, yend = PC2_scaled),
    inherit.aes = FALSE,
    arrow       = arrow(length = unit(0.25, "cm")),
    color = "grey30", linewidth = 0.6
  ) +
  geom_text(
    data        = loadings_c,
    aes(x = PC1_scaled * 1.12, y = PC2_scaled * 1.12, label = variable),
    inherit.aes = FALSE,
    size = 3.5, color = "grey20"
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x       = sprintf("PC1 (%.1f%%)", pct_c[1]),
    y       = sprintf("PC2 (%.1f%%)", pct_c[2]),
    caption = paste(
      "Variables group\u2013mean centered within each evolution history before PCA.",
      "Within\u2013group covariation only."
    )
  ) +
  theme_evo() +
  theme(
    legend.position = "bottom",
    plot.caption    = element_text(size = 9, color = "grey40")
  )

ggsave(file.path(FIGS, "pca-biplot-within-22b.png"),
       width = 5, height = 4.5, dpi = 300, bg = "transparent")


# =============================================================================
# 7. Export
# =============================================================================

write_csv(
  scores_c |> select(strain, evolution_history, PC1, PC2, PC3),
  file.path(OUT, "pca-scores-within-20.csv")
)
write_csv(
  loadings_c |> select(variable, PC1, PC2),
  file.path(OUT, "pca-loadings-within-20.csv")
)
write_csv(
  tibble(PC = names(pct_c), pct = as.numeric(pct_c)),
  file.path(OUT, "pca-pct-within-20.csv")
)

cat("\nAll outputs written.\n")
