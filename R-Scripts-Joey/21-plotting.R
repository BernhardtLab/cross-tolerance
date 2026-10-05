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


# Ancestor landmark values (for TPC annotations)
anc <- tpc_params |> filter(evolution_history == "fRS585")

# Predicted y on ancestor curve at each landmark temperature
anc_curve <- tpc_preds |>
  filter(evolution_history == "fRS585") |>
  group_by(temp) |>
  summarise(pred = mean(pred), .groups = "drop")
y_topt <- approx(anc_curve$temp, anc_curve$pred, xout = anc$topt)$y
y_th   <- approx(anc_curve$temp, anc_curve$pred, xout = anc$th_c)$y
y_tmax <- approx(anc_curve$temp, anc_curve$pred, xout = anc$tmax)$y

# Evolved group means per trait (for TPC annotations)
grp_means <- tpc_params |>
  filter(evolution_history %in% EVO_LEVELS) |>
  group_by(evolution_history) |>
  summarise(across(c(topt, tmax, th_c), mean)) |>
  mutate(evolution_history = factor(evolution_history, levels = EVO_LEVELS))


# =============================================================================
# 3. TPC curves plot
# =============================================================================

mean_curves <- tpc_preds |>
  filter(evolution_history %in% c(EVO_LEVELS, "fRS585"), temp >= 23, temp <= 45) |>
  group_by(evolution_history, temp) |>
  summarise(pred = mean(pred, na.rm = TRUE), .groups = "drop")

obs_means <- well_metrics |>
  filter(!is.na(auc_gc), evolution_history %in% c(EVO_LEVELS, "fRS585")) |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(auc_gc = mean(auc_gc, na.rm = TRUE), .groups = "drop")

# Bracket geometry: one bracket per trait spanning its 3 group ticks
brac_y <- -0.12   # y of horizontal bar
tip_h  <-  0.025  # height of end ticks (upward)
lbl_y  <- -0.155  # y of label
pad    <-  0.22   # padding beyond outermost tick

trait_brackets <- tibble(
  label = c("T[opt]", "T[h]", "T[max]"),
  xs = list(
    c(grp_means$topt, anc$topt),
    c(grp_means$th_c, anc$th_c),
    c(grp_means$tmax, anc$tmax)
  )
) |>
  mutate(
    xmin = map_dbl(xs, min) - pad,
    xmax = map_dbl(xs, max) + pad,
    xmid = (xmin + xmax) / 2
  )

plot_tpc <- ggplot() +
  geom_line(
    data = tpc_preds |>
      filter(evolution_history %in% EVO_LEVELS, temp >= 23, temp <= 45) |>
      mutate(pred = ifelse(pred < 0, NA, pred)),
    aes(x = temp, y = pred, group = strain, color = evolution_history),
    alpha = 0.2, linewidth = 0.8
  ) +
  # Evolved group mean curves
  geom_line(
    data = mean_curves |> filter(evolution_history %in% EVO_LEVELS),
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 2.0
  ) +
  # Ancestor reference curve — dashed to distinguish from evolved means
  geom_line(
    data = mean_curves |> filter(evolution_history == "fRS585"),
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 1.5
  ) +
  geom_point(
    data = obs_means,
    aes(x = test_temperature, y = auc_gc, fill = evolution_history),
    shape = 21, size = 2.5, alpha = 0.8, color = "black"
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL,
                     labels = c("35 evolved" = "35 evolved",
                                "40 evolved" = "40 evolved",
                                "fRS585"     = "Ancestor")) +
  scale_fill_manual(values = EVO_COLORS, guide = "none") +
  # Arrows pointing to ancestor curve features
  annotate("segment",
           x = anc$topt + 1.2, xend = anc$topt + 0.2,
           y = y_topt + 0.08,  yend = y_topt + 0.01,
           arrow = arrow(length = unit(0.2, "cm")), color = "grey30") +
  annotate("text", x = anc$topt + 1.3, y = y_topt + 0.09,
           label = "T[opt]", hjust = 0, size = 3.5, color = "grey20", parse = TRUE) +
  annotate("segment",
           x = anc$th_c + 1.2, xend = anc$th_c + 0.2,
           y = y_th + 0.08,    yend = y_th + 0.01,
           arrow = arrow(length = unit(0.2, "cm")), color = "grey30") +
  annotate("text", x = anc$th_c + 1.3, y = y_th + 0.09,
           label = "T[h]", hjust = 0, size = 3.5, color = "grey20", parse = TRUE) +
  annotate("segment",
           x = anc$tmax + 0.8, xend = anc$tmax + 0.1,
           y = y_tmax + 0.12,  yend = y_tmax + 0.03,
           arrow = arrow(length = unit(0.2, "cm")), color = "grey30") +
  annotate("text", x = anc$tmax + 0.9, y = y_tmax + 0.13,
           label = "T[max]", hjust = 0, size = 3.5, color = "grey20", parse = TRUE) +
  # Coloured ticks on x-axis
  annotate("segment",
           x    = c(grp_means$topt, grp_means$th_c, grp_means$tmax,
                    anc$topt, anc$th_c, anc$tmax),
           xend = c(grp_means$topt, grp_means$th_c, grp_means$tmax,
                    anc$topt, anc$th_c, anc$tmax),
           y = 0.02, yend = -0.08,
           color = c(EVO_COLORS[rep(as.character(grp_means$evolution_history), 3)],
                     rep("#000000", 3)),
           linewidth = 0.8) +
  # Grouping brackets under ticks
  annotate("segment",
           x    = trait_brackets$xmin, xend = trait_brackets$xmax,
           y    = brac_y,              yend = brac_y,
           color = "grey50", linewidth = 0.5) +
  annotate("segment",
           x    = trait_brackets$xmin, xend = trait_brackets$xmin,
           y    = brac_y,              yend = brac_y + tip_h,
           color = "grey50", linewidth = 0.5) +
  annotate("segment",
           x    = trait_brackets$xmax, xend = trait_brackets$xmax,
           y    = brac_y,              yend = brac_y + tip_h,
           color = "grey50", linewidth = 0.5) +
  annotate("text",
           x = trait_brackets$xmid, y = lbl_y,
           label = trait_brackets$label,
           size = 3, color = "grey40", hjust = 0.5, parse = TRUE) +
  coord_cartesian(xlim = c(23, 45), clip = "off") +
  labs(x = "Temperature (\u00b0C)", y = "Growth performance (OD\u00b7day)") +
  theme_evo() +
  theme(
    legend.position        = c(0.03, 0.03),
    legend.justification   = c("left", "bottom"),
    legend.background      = element_rect(fill = "white", colour = NA),
    legend.key.size        = unit(0.45, "cm"),
    plot.margin            = margin(5, 5, 20, 5, "pt")
  )


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

plot_pca <- ggplot(pca_scores, aes(x = PC1, y = PC2, fill = evolution_history)) +
  geom_point(shape = 21, size = 2.5, alpha = 0.8, color = "black") +
  geom_segment(
    data        = pca_loadings,
    aes(x = 0, y = 0, xend = PC1_scaled, yend = PC2_scaled),
    inherit.aes = FALSE,
    arrow       = arrow(length = unit(0.25, "cm")),
    color = "grey30", linewidth = 0.6
  ) +
  geom_text(
    data        = pca_loadings |>
      mutate(label = recode(variable,
                            topt         = "T[opt]",
                            tmax         = "T[max]",
                            th_c         = "T[h]",
                            caspofungin  = "Casp",
                            fluconazole  = "Fluc",
                            amphotericin = "Amph")),
    aes(x = PC1_scaled * 1.12, y = PC2_scaled * 1.12, label = label),
    inherit.aes = FALSE,
    size = 3.2, color = "grey20", parse = TRUE
  ) +
  scale_fill_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x = sprintf("PC1 (%.1f%%)", pc1_pct),
    y = sprintf("PC2 (%.1f%%)", pc2_pct)
  ) +
  theme_evo() +
  theme(legend.position = "none")


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
  mutate(
    y_pos    = mean_val + se + y_span * 0.10,
    txt_face = if_else(label == "ns", "plain", "bold"),
    txt_size = if_else(label == "ns", 3.5, 5)
  )

plot_traits <- ggplot(plot_data_traits,
                      aes(x = evolution_history, y = value, fill = evolution_history)) +
  geom_hline(data = anc_ref_traits, aes(yintercept = anc_val),
             linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(shape = 21, width = 0.12, size = 2.5, alpha = 0.6, color = "black") +
  geom_errorbar(
    data = group_means_traits,
    aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
    linewidth = 1.2, width = 0.15, color = "black",
    position = position_nudge(x = -0.3)
  ) +
  geom_point(
    data  = group_means_traits,
    aes(y = mean_val, fill = evolution_history),
    shape = 21, size = 2, color = "black", stroke = 1.2,
    position = position_nudge(x = -0.3)
  ) +
  suppressWarnings(ggsignif::geom_signif(
    data        = bracket_traits,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual      = TRUE, inherit.aes = FALSE,
    tip_length  = 0.02, textsize = 4.6, color = "black"
  )) +
  geom_text(
    data  = anc_stars_traits,
    aes(x = evolution_history, y = y_pos, label = label,
        size = txt_size, fontface = txt_face),
    color = "black", nudge_x = -0.3
  ) +
  facet_wrap(~ trait, scales = "free_y",
             labeller = as_labeller(c(topt = "T[opt]", tmax = "T[max]", th_c = "T[h]"),
                                   label_parsed)) +
  scale_fill_manual(values = EVO_COLORS, guide = "none") +
  scale_size_identity() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(x = NULL, y = "Temperature (\u00b0C)") +
  theme_evo() +
  theme(
    strip.background  = element_blank(),
    strip.text        = element_text(size = 13),
    legend.position   = "none",
    axis.text.x       = element_blank(),
    axis.ticks.x      = element_blank(),
    axis.line.x       = element_blank(),
    axis.title.x      = element_blank()
  )


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
  mutate(
    y_pos    = mean_val + se + y_span * 0.10,
    txt_face = if_else(label == "ns", "plain", "bold"),
    txt_size = if_else(label == "ns", 3.5, 5)
  )

plot_ic50 <- ggplot(plot_data_ic50,
                    aes(x = evolution_history, y = log_ratio, fill = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(shape = 21, width = 0.12, size = 2.5, alpha = 0.6, color = "black") +
  geom_errorbar(
    data = gmeans_ic50,
    aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
    linewidth = 1.2, width = 0.15, color = "black",
    position = position_nudge(x = -0.3)
  ) +
  geom_point(
    data  = gmeans_ic50,
    aes(y = mean_val, fill = evolution_history),
    shape = 21, size = 2, color = "black", stroke = 1.2,
    position = position_nudge(x = -0.3)
  ) +
  suppressWarnings(ggsignif::geom_signif(
    data        = bracket_ic50,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual      = TRUE, inherit.aes = FALSE,
    tip_length  = 0.02, textsize = 4.6, color = "black"
  )) +
  geom_text(
    data  = anc_stars_ic50,
    aes(x = evolution_history, y = y_pos, label = label,
        size = txt_size, fontface = txt_face),
    color = "black", nudge_x = -0.3
  ) +
  facet_wrap(~ drug, scales = "free_y",
             labeller = as_labeller(c(amphotericin = "Amphotericin",
                                      caspofungin  = "Caspofungin",
                                      fluconazole  = "Fluconazole"))) +
  scale_fill_manual(values = EVO_COLORS, guide = "none") +
  scale_size_identity() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(x = "Evolution history", y = expression(log[2](IC[50] / "ancestor IC"[50]))) +
  theme_evo() +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(size = 13),
    legend.position  = "none"
  )


# =============================================================================
# 7. Combine and save
# =============================================================================

combined <- ((plot_tpc | plot_pca) + plot_layout(widths = c(3, 2))) /
  plot_traits /
  plot_ic50 +
  plot_layout(heights = c(1.1, 1, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(axis.title   = element_text(size = 12),
        axis.text    = element_text(size = 11),
        plot.margin  = margin(0, 2, 0, 2, "pt"))

ggsave(file.path(FIGS, "combined-figure-21.png"),
       combined, width = 7, height = 8, dpi = 300, bg = "transparent")
