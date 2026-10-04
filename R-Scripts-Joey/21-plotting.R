# =============================================================================
# 21-plotting.R
# Combined multi-panel figure
#
# 
#   Row 1: (A) TPC curves  |  (B) PCA biplot
#   Row 2: (C) Thermal traits dotplot  [3-panel facet: Topt, Tmax, Th]
#   Row 3: (D) IC50 dotplot            [3-panel facet: by drug]
#
# 
#
# Inputs:
#   data-processed/gcplyr/tpc-predictions-auc-gcplyr-17.csv
#   data-processed/gcplyr/gcplyr-metrics-per-well-17.csv
#   data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv
#   data-processed/gcplyr/stats-results-auc-gcplyr-17.csv
#   data-processed/normalised-ic50-per-strain.csv
#   data-processed/stats-results-normalised-ic50.csv
#   data-processed/pca-scores-within-20.csv
#   data-processed/pca-loadings-within-20.csv
#   data-processed/pca-pct-within-20.csv
#
# Output:
#   figures/combined-figure-21.png
# =============================================================================


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)
library(ggsignif)
library(patchwork)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")
EVO_LEVELS <- c("35 evolved", "40 evolved")

FIGS    <- "figures"
OUT_TPC <- "data-processed/gcplyr"
OUT     <- "data-processed"

theme_evo <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      panel.background  = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA),
      plot.background   = element_rect(fill = "transparent", colour = NA)
    )
}

p_stars <- function(p) dplyr::case_when(
  p < 0.001 ~ "***",
  p < 0.01  ~ "**",
  p < 0.05  ~ "*",
  TRUE      ~ "ns"
)


# =============================================================================
# 2. Load data
# =============================================================================

tpc_preds   <- read_csv(file.path(OUT_TPC, "tpc-predictions-auc-gcplyr-17.csv"),
                        show_col_types = FALSE)
well_metrics <- read_csv(file.path(OUT_TPC, "gcplyr-metrics-per-well-17.csv"),
                         show_col_types = FALSE)
tpc_params  <- read_csv(file.path(OUT_TPC, "tpc-params-auc-gcplyr-17.csv"),
                        show_col_types = FALSE)
stats_tpc   <- read_csv(file.path(OUT_TPC, "stats-results-auc-gcplyr-17.csv"),
                        show_col_types = FALSE)

ic50_strains <- read_csv(file.path(OUT, "normalised-ic50-per-strain.csv"),
                         show_col_types = FALSE)
stats_ic50   <- read_csv(file.path(OUT, "stats-results-normalised-ic50.csv"),
                         show_col_types = FALSE)

pca_scores   <- read_csv(file.path(OUT, "pca-scores-within-20.csv"),
                         show_col_types = FALSE) |>
  mutate(evolution_history = factor(evolution_history, levels = EVO_LEVELS))
pca_loadings <- read_csv(file.path(OUT, "pca-loadings-within-20.csv"),
                         show_col_types = FALSE)
pca_pct      <- read_csv(file.path(OUT, "pca-pct-within-20.csv"),
                         show_col_types = FALSE)


# =============================================================================
# 3. TPC curves plot
# =============================================================================

mean_curves <- tpc_preds |>
  filter(evolution_history %in% c(EVO_LEVELS, "fRS585")) |>
  group_by(evolution_history, temp) |>
  summarise(pred = mean(pred, na.rm = TRUE), .groups = "drop")

obs_means <- well_metrics |>
  filter(!is.na(auc_gc), evolution_history %in% c(EVO_LEVELS, "fRS585")) |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(auc_gc = mean(auc_gc, na.rm = TRUE), .groups = "drop")

plot_tpc <- ggplot() +
  geom_line(
    data = tpc_preds |>
      filter(evolution_history %in% EVO_LEVELS) |>
      mutate(pred = ifelse(pred < 0, NA, pred)),
    aes(x = temp, y = pred, group = strain, color = evolution_history),
    alpha = 0.2, linewidth = 0.8
  ) +
  geom_line(
    data = mean_curves,
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 2.0
  ) +
  geom_point(
    data = obs_means,
    aes(x = test_temperature, y = auc_gc, color = evolution_history),
    size = 2.0, alpha = 0.6
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  coord_cartesian(xlim = c(23, 45)) +
  labs(x = "Temperature (\u00b0C)", y = "Growth performance (OD\u00b7day)") +
  theme_evo()


# =============================================================================
# 4. PCA biplot
# =============================================================================

pca_loadings <- pca_loadings |>
  mutate(
    PC1_scaled = PC1 * max(abs(pca_scores$PC1)),
    PC2_scaled = PC2 * max(abs(pca_scores$PC2))
  )

pc1_pct <- pca_pct |> filter(PC == "PC1") |> pull(pct)
pc2_pct <- pca_pct |> filter(PC == "PC2") |> pull(pct)

plot_pca <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = evolution_history)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_segment(
    data        = pca_loadings,
    aes(x = 0, y = 0, xend = PC1_scaled, yend = PC2_scaled),
    inherit.aes = FALSE,
    arrow       = arrow(length = unit(0.25, "cm")),
    color = "grey30", linewidth = 0.6
  ) +
  geom_text(
    data        = pca_loadings,
    aes(x = PC1_scaled * 1.12, y = PC2_scaled * 1.12, label = variable),
    inherit.aes = FALSE,
    size = 3.2, color = "grey20"
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x = sprintf("PC1 (%.1f%%)", pc1_pct),
    y = sprintf("PC2 (%.1f%%)", pc2_pct)
  ) +
  theme_evo()


# =============================================================================
# 5. Thermal traits dotplot with significance annotations
# =============================================================================

TRAIT_LEVELS <- c("topt", "tmax", "th_c")

plot_data_traits <- tpc_params |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(strain, evolution_history, topt, tmax, th_c) |>
  pivot_longer(c(topt, tmax, th_c), names_to = "trait", values_to = "value") |>
  mutate(
    trait             = factor(trait, levels = TRAIT_LEVELS),
    evolution_history = factor(evolution_history, levels = EVO_LEVELS)
  )

anc_ref_traits <- tpc_params |>
  filter(evolution_history == "fRS585") |>
  select(topt, tmax, th_c) |>
  pivot_longer(everything(), names_to = "trait", values_to = "anc_val") |>
  mutate(trait = factor(trait, levels = TRAIT_LEVELS))

group_means_traits <- plot_data_traits |>
  summarise(
    mean_val = mean(value),
    se       = sd(value) / sqrt(n()),
    .by      = c(evolution_history, trait)
  )

y_range_traits <- plot_data_traits |>
  summarise(y_max = max(value), y_span = diff(range(value)), .by = trait)

bracket_traits <- stats_tpc |>
  filter(comparison == "35 evolved vs 40 evolved") |>
  mutate(
    trait       = factor(trait, levels = TRAIT_LEVELS),
    xmin        = "35 evolved",
    xmax        = "40 evolved",
    annotations = p_stars(p_welch_holm)
  ) |>
  filter(!is.na(trait)) |>
  left_join(y_range_traits, by = "trait") |>
  mutate(y_position = y_max + y_span * 0.18)

anc_stars_traits <- stats_tpc |>
  filter(str_detect(comparison, "vs ancestor")) |>
  mutate(
    trait             = factor(trait, levels = TRAIT_LEVELS),
    evolution_history = factor(str_remove(comparison, " vs ancestor"),
                               levels = EVO_LEVELS),
    label             = p_stars(p_welch_holm)
  ) |>
  filter(!is.na(trait)) |>
  left_join(group_means_traits, by = c("trait", "evolution_history")) |>
  left_join(y_range_traits, by = "trait") |>
  mutate(y_pos = mean_val + se + y_span * 0.10)

plot_traits <- ggplot(plot_data_traits,
                      aes(x = evolution_history, y = value, color = evolution_history)) +
  geom_hline(data = anc_ref_traits, aes(yintercept = anc_val),
             linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(width = 0.12, size = 2.5, alpha = 0.6) +
  geom_pointrange(
    data = group_means_traits,
    aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
    size = 0.8, linewidth = 1.4
  ) +
  suppressWarnings(ggsignif::geom_signif(
    data       = bracket_traits,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual     = TRUE, tip_length = 0.02, textsize = 5.5, color = "black"
  )) +
  geom_text(
    data  = anc_stars_traits,
    aes(x = evolution_history, y = y_pos, label = label),
    color = "black", size = 5, fontface = "bold", nudge_x = 0.3
  ) +
  facet_wrap(~ trait, scales = "free_y",
             labeller = as_labeller(c(topt = "Topt", tmax = "Tmax", th_c = "Th"))) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
  labs(x = NULL, y = "Temperature (\u00b0C)") +
  theme_evo() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 13))


# =============================================================================
# 6. IC50 dotplot with significance annotations
# =============================================================================

plot_data_ic50 <- ic50_strains |>
  filter(evolution_history %in% EVO_LEVELS) |>
  mutate(evolution_history = factor(evolution_history, levels = EVO_LEVELS))

gmeans_ic50 <- plot_data_ic50 |>
  summarise(
    mean_val = mean(log_ratio),
    se       = sd(log_ratio) / sqrt(n()),
    .by      = c(evolution_history, drug)
  )

y_range_ic50 <- plot_data_ic50 |>
  summarise(y_max = max(log_ratio), y_span = diff(range(log_ratio)), .by = drug)

bracket_ic50 <- stats_ic50 |>
  filter(comparison == "40 evolved vs 35 evolved") |>
  mutate(
    xmin        = "35 evolved",
    xmax        = "40 evolved",
    annotations = p_stars(p_welch_holm)
  ) |>
  left_join(y_range_ic50, by = "drug") |>
  mutate(y_position = y_max + y_span * 0.18)

anc_stars_ic50 <- stats_ic50 |>
  filter(str_detect(comparison, "vs ancestor")) |>
  mutate(
    evolution_history = factor(str_remove(comparison, " vs ancestor"),
                               levels = EVO_LEVELS),
    label             = p_stars(p_welch_holm)
  ) |>
  left_join(gmeans_ic50, by = c("drug", "evolution_history")) |>
  left_join(y_range_ic50, by = "drug") |>
  mutate(y_pos = mean_val + se + y_span * 0.10)

plot_ic50 <- ggplot(plot_data_ic50,
                    aes(x = evolution_history, y = log_ratio, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(width = 0.12, size = 2.5, alpha = 0.6) +
  geom_pointrange(
    data = gmeans_ic50,
    aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
    size = 0.8, linewidth = 1.4
  ) +
  suppressWarnings(ggsignif::geom_signif(
    data       = bracket_ic50,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual     = TRUE, tip_length = 0.02, textsize = 5.5, color = "black"
  )) +
  geom_text(
    data  = anc_stars_ic50,
    aes(x = evolution_history, y = y_pos, label = label),
    color = "black", size = 5, fontface = "bold", nudge_x = 0.3
  ) +
  facet_wrap(~ drug, scales = "free_y",
             labeller = as_labeller(c(amphotericin = "Amphotericin",
                                      caspofungin  = "Caspofungin",
                                      fluconazole  = "Fluconazole"))) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
  labs(x = NULL, y = expression(log[2](IC50 / "ancestor IC50"))) +
  theme_evo() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 13))


# =============================================================================
# 7. Combine and save
# =============================================================================

combined <- (plot_tpc | plot_pca) / plot_traits / plot_ic50 +
  plot_layout(heights = c(1.3, 1, 1), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")

ggsave(file.path(FIGS, "combined-figure-21.png"),
       combined, width = 8, height = 9, dpi = 300, bg = "transparent")
