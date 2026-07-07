# =============================================================================
# composite-figure.R
# 11-panel composite figure:
#   A  — TPC curves (per-strain + group mean + observed points)
#   B–D — Topt, Tmax, Th dotplots with significance brackets
#   E–F — AUC at 41°C and 42°C dotplots with significance brackets
#   G–I — Fluconazole, Caspofungin, Amphotericin IC50 dotplots
#   J   — Within-group PCA biplot
#   K   — Within-group (group-mean-centred) Deming regression
#
# Session requirements (from scripts 17, 19, 11c):
#   tpc_ribbon_df, tpc_pt_data, tpc_boot_se, tpc_stats,
#   deming_centered, within_lines_clipped, label_df,
#   pca_c, wide_centered,
#   fluc_strain, casp_strain, amph_strain, plot_df_amph, gmeans_amph,
#   plot_df_mic, gmeans_mic, plot_df_casp, gmeans_casp, results_amph
# =============================================================================

library(tidyverse)
library(patchwork)
library(ggsignif)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")
BS <- 9  # base_size for all panels

p_stars <- function(p) case_when(
  p < 0.001 ~ "***",
  p < 0.01  ~ "**",
  p < 0.05  ~ "*",
  TRUE      ~ "ns"
)

# =============================================================================
# Panel A: TPC curves
# =============================================================================

mean_curves <- tpc_ribbon_df |>
  group_by(evolution_history, temperature) |>
  summarise(fitted = mean(fitted), .groups = "drop")

obs_means <- tpc_pt_data |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(auc = mean(auc_gc, na.rm = TRUE), .groups = "drop")

p_tpc <- ggplot() +
  geom_line(
    data  = tpc_ribbon_df,
    aes(x = temperature, y = fitted, group = strain, color = evolution_history),
    alpha = 0.2, linewidth = 0.55
  ) +
  geom_line(
    data  = mean_curves,
    aes(x = temperature, y = fitted, color = evolution_history),
    linewidth = 1.6
  ) +
  geom_point(
    data  = obs_means,
    aes(x = test_temperature, y = auc, color = evolution_history),
    size = 1.3, alpha = 0.55
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  coord_cartesian(xlim = c(23, 48)) +
  labs(x = "Temperature (\u00b0C)", y = "AUC (empirical)") +
  theme_bw(base_size = BS) +
  theme(
    legend.position      = c(0.02, 0.97),
    legend.justification = c(0, 1),
    legend.background    = element_blank(),
    legend.key.size      = unit(0.35, "cm"),
    legend.text          = element_text(size = 8)
  )

# =============================================================================
# Panels B–D: Topt, Tmax, Th dotplots
# =============================================================================

evolved_bs <- tpc_boot_se |>
  filter(evolution_history %in% c("35 evolved", "40 evolved")) |>
  mutate(evolution_history = factor(evolution_history,
                                    levels = c("35 evolved", "40 evolved")))

anc_topt <- tpc_boot_se |> filter(evolution_history == "fRS585") |> pull(topt)
anc_tmax <- tpc_boot_se |> filter(evolution_history == "fRS585") |> pull(tmax)
anc_th_c <- tpc_boot_se |> filter(evolution_history == "fRS585") |> pull(th_c)

make_trait_panel <- function(trait_name, anc_val, y_label) {
  dat <- evolved_bs |> mutate(value = .data[[trait_name]])
  y_max  <- max(dat$value, na.rm = TRUE)
  y_span <- diff(range(dat$value, na.rm = TRUE))

  gmeans <- dat |>
    group_by(evolution_history) |>
    summarise(m = mean(value), se = sd(value) / sqrt(n()), .groups = "drop")

  stat_b2b <- tpc_stats |>
    filter(trait == trait_name, comparison == "35 evolved vs 40 evolved")

  stat_anc <- tpc_stats |>
    filter(trait == trait_name, str_detect(comparison, "vs ancestor")) |>
    mutate(
      evolution_history = factor(str_remove(comparison, " vs ancestor"),
                                 levels = c("35 evolved", "40 evolved")),
      label = p_stars(p_welch_holm)
    ) |>
    left_join(gmeans, by = "evolution_history") |>
    mutate(y_pos = m + se + y_span * 0.05)

  bracket_dat <- tibble(
    xmin        = "35 evolved",
    xmax        = "40 evolved",
    y_position  = y_max + y_span * 0.10,
    annotations = p_stars(stat_b2b$p_welch_holm)
  )

  ggplot(dat, aes(x = evolution_history, y = value, color = evolution_history)) +
    geom_hline(yintercept = anc_val, linetype = "dashed",
               color = "#000000", linewidth = 0.45) +
    geom_jitter(width = 0.12, size = 1.2, alpha = 0.6) +
    geom_pointrange(
      data = gmeans,
      aes(y = m, ymin = m - se, ymax = m + se),
      size = 0.4, linewidth = 0.8
    ) +
    suppressWarnings(geom_signif(
      data = bracket_dat,
      aes(xmin = xmin, xmax = xmax, annotations = annotations,
          y_position = y_position),
      manual = TRUE, tip_length = 0.02, textsize = 3, color = "black"
    )) +
    geom_text(
      data      = stat_anc,
      aes(x = evolution_history, y = y_pos, label = label),
      color     = "black", size = 2.8, fontface = "bold", nudge_x = 0.3
    ) +
    scale_color_manual(values = EVO_COLORS) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
    labs(x = NULL, y = y_label) +
    theme_bw(base_size = BS) +
    theme(legend.position = "none")
}

p_topt <- make_trait_panel("topt", anc_topt, "Topt (\u00b0C)")
p_tmax <- make_trait_panel("tmax", anc_tmax, "Tmax (\u00b0C)")
p_th   <- make_trait_panel("th_c", anc_th_c, "Th (\u00b0C)")

# =============================================================================
# Panels E–F: Observed AUC at 41°C and 42°C
# =============================================================================

obs_auc_hi <- tpc_pt_data |>
  filter(test_temperature %in% c(41, 42)) |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(auc = mean(auc_gc, na.rm = TRUE), .groups = "drop")

anc_auc_41 <- obs_auc_hi |>
  filter(evolution_history == "fRS585", test_temperature == 41) |> pull(auc)
anc_auc_42 <- obs_auc_hi |>
  filter(evolution_history == "fRS585", test_temperature == 42) |> pull(auc)

make_auc_panel <- function(temp_val, anc_val) {
  dat <- obs_auc_hi |>
    filter(test_temperature == temp_val,
           evolution_history %in% c("35 evolved", "40 evolved")) |>
    mutate(evolution_history = factor(evolution_history,
                                      levels = c("35 evolved", "40 evolved")))

  g35 <- dat |> filter(evolution_history == "35 evolved") |> pull(auc)
  g40 <- dat |> filter(evolution_history == "40 evolved") |> pull(auc)

  raw_ps <- c(
    t.test(g35, g40)$p.value,
    t.test(g35, mu = anc_val)$p.value,
    t.test(g40, mu = anc_val)$p.value
  )
  adj_ps <- p.adjust(raw_ps, method = "holm")

  gmeans <- dat |>
    group_by(evolution_history) |>
    summarise(m = mean(auc), se = sd(auc) / sqrt(n()), .groups = "drop")

  y_max  <- max(dat$auc, na.rm = TRUE)
  y_span <- diff(range(dat$auc, na.rm = TRUE))

  anc_stars <- tibble(
    evolution_history = factor(c("35 evolved", "40 evolved"),
                               levels = c("35 evolved", "40 evolved")),
    p_holm = adj_ps[2:3]
  ) |>
    left_join(gmeans, by = "evolution_history") |>
    mutate(y_pos = m + se + y_span * 0.05, label = p_stars(p_holm))

  bracket_dat <- tibble(
    xmin        = "35 evolved",
    xmax        = "40 evolved",
    y_position  = y_max + y_span * 0.10,
    annotations = p_stars(adj_ps[1])
  )

  ggplot(dat, aes(x = evolution_history, y = auc, color = evolution_history)) +
    geom_hline(yintercept = anc_val, linetype = "dashed",
               color = "#000000", linewidth = 0.45) +
    geom_jitter(width = 0.12, size = 1.2, alpha = 0.6) +
    geom_pointrange(
      data = gmeans,
      aes(y = m, ymin = m - se, ymax = m + se),
      size = 0.4, linewidth = 0.8
    ) +
    suppressWarnings(geom_signif(
      data = bracket_dat,
      aes(xmin = xmin, xmax = xmax, annotations = annotations,
          y_position = y_position),
      manual = TRUE, tip_length = 0.02, textsize = 3, color = "black"
    )) +
    geom_text(
      data  = anc_stars,
      aes(x = evolution_history, y = y_pos, label = label),
      color = "black", size = 2.8, fontface = "bold", nudge_x = 0.3
    ) +
    scale_color_manual(values = EVO_COLORS) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
    labs(x = NULL, y = sprintf("AUC at %d\u00b0C", temp_val)) +
    theme_bw(base_size = BS) +
    theme(legend.position = "none")
}

p_auc41 <- make_auc_panel(41, anc_auc_41)
p_auc42 <- make_auc_panel(42, anc_auc_42)

# =============================================================================
# Panels G–I: IC50 dotplots
# (uses plot_df_mic/casp/amph and gmeans_mic/casp/amph from session)
# =============================================================================

make_ic50_panel <- function(plot_df, gmeans_df, anc_val,
                             g35_vals, g40_vals, y_label,
                             amph_p_between = NULL, amph_anc_stars = NULL) {
  # Compute stats fresh unless amph override provided (emmeans case)
  if (is.null(amph_p_between)) {
    raw_ps <- c(
      t.test(log(g35_vals), log(g40_vals))$p.value,
      t.test(log(g35_vals), mu = log(anc_val))$p.value,
      t.test(log(g40_vals), mu = log(anc_val))$p.value
    )
    adj_ps <- p.adjust(raw_ps, method = "holm")
    p_between <- adj_ps[1]
    anc_stars <- tibble(
      evolution_history = factor(c("35 evolved", "40 evolved"),
                                 levels = c("35 evolved", "40 evolved")),
      p_holm = adj_ps[2:3]
    ) |>
      left_join(gmeans_df, by = "evolution_history") |>
      mutate(y_pos = ymax * 1.05, label = p_stars(p_holm))
  } else {
    p_between <- amph_p_between
    anc_stars  <- amph_anc_stars
  }

  y_max      <- max(plot_df$ic50)
  bracket_y  <- y_max * 1.14
  tip_bottom <- y_max * 1.05

  ggplot(plot_df, aes(x = evolution_history, y = ic50, color = evolution_history)) +
    geom_hline(yintercept = anc_val, linetype = "dashed",
               color = "#000000", linewidth = 0.45) +
    geom_jitter(width = 0.12, size = 1.2, alpha = 0.6) +
    geom_pointrange(
      data = gmeans_df,
      aes(y = mean_val, ymin = ymin, ymax = ymax),
      size = 0.4, linewidth = 0.8
    ) +
    # Manual bracket (ggsignif unreliable on log axes)
    annotate("segment", x = 1, xend = 1, y = tip_bottom, yend = bracket_y) +
    annotate("segment", x = 2, xend = 2, y = tip_bottom, yend = bracket_y) +
    annotate("segment", x = 1, xend = 2, y = bracket_y, yend = bracket_y) +
    annotate("text", x = 1.5, y = bracket_y * 1.07,
             label = p_stars(p_between), size = 3, color = "black") +
    geom_text(
      data  = anc_stars,
      aes(x = evolution_history, y = y_pos, label = label),
      color = "black", size = 2.8, fontface = "bold", nudge_x = 0.3
    ) +
    scale_y_log10(limits = c(min(plot_df$ic50) * 0.7, y_max * 1.4)) +
    scale_color_manual(values = EVO_COLORS) +
    labs(x = NULL, y = y_label) +
    theme_bw(base_size = BS) +
    theme(legend.position = "none")
}

# ── Fluconazole ───────────────────────────────────────────────────────────────
anc_fluc  <- fluc_strain |> filter(evolution_history == "fRS585") |> pull(ic50)
g35_fluc  <- fluc_strain |> filter(evolution_history == "35 evolved") |> pull(ic50)
g40_fluc  <- fluc_strain |> filter(evolution_history == "40 evolved") |> pull(ic50)

p_flu <- make_ic50_panel(
  plot_df  = plot_df_mic,
  gmeans_df = gmeans_mic,
  anc_val  = anc_fluc,
  g35_vals = g35_fluc,
  g40_vals = g40_fluc,
  y_label  = "Fluconazole IC50 (\u03bcg/mL)"
)

# ── Caspofungin ───────────────────────────────────────────────────────────────
anc_casp_val <- casp_strain |> filter(evolution_history == "fRS585") |> pull(ic50)
g35_casp  <- casp_strain |> filter(evolution_history == "35 evolved") |> pull(ic50)
g40_casp  <- casp_strain |> filter(evolution_history == "40 evolved") |> pull(ic50)

p_cas <- make_ic50_panel(
  plot_df  = plot_df_casp,
  gmeans_df = gmeans_casp,
  anc_val  = anc_casp_val,
  g35_vals = g35_casp,
  g40_vals = g40_casp,
  y_label  = "Caspofungin IC50 (\u03bcg/mL)"
)

# ── Amphotericin (emmeans p-values, month-adjusted) ──────────────────────────
anc_amph_val <- amph_strain |>
  filter(evolution_history == "fRS585") |>
  summarise(anc = exp(mean(log(ic50)))) |>
  pull(anc)

amph_p_between <- results_amph |>
  filter(str_detect(comparison, "40 evolved") & str_detect(comparison, "35 evolved")) |>
  pull(p_holm)

amph_anc_stars <- results_amph |>
  filter(str_detect(comparison, "fRS585")) |>
  mutate(
    evolution_history = factor(str_remove(comparison, " - fRS585"),
                               levels = c("35 evolved", "40 evolved")),
    label = p_stars(p_holm)
  ) |>
  left_join(gmeans_amph, by = "evolution_history") |>
  mutate(y_pos = ymax * 1.05)

p_amph <- make_ic50_panel(
  plot_df         = plot_df_amph,
  gmeans_df       = gmeans_amph,
  anc_val         = anc_amph_val,
  g35_vals        = NULL,
  g40_vals        = NULL,
  y_label         = "Amphotericin B IC50 (\u03bcg/mL)",
  amph_p_between  = amph_p_between,
  amph_anc_stars  = amph_anc_stars
)

# =============================================================================
# Panel J: Within-group PCA biplot
# =============================================================================

scores_c <- as.data.frame(pca_c$x) |>
  mutate(
    population        = wide_centered$population,
    evolution_history = wide_centered$evolution_history
  )

loadings_c <- as.data.frame(pca_c$rotation[, 1:2])
loadings_c$variable <- rownames(loadings_c)

score_range_c <- max(abs(scores_c$PC1), abs(scores_c$PC2))
arrow_range_c  <- max(sqrt(loadings_c$PC1^2 + loadings_c$PC2^2))
arrow_scale_c  <- 0.70 * score_range_c / arrow_range_c

loadings_c <- loadings_c |>
  mutate(
    xend     = PC1 * arrow_scale_c,
    yend     = PC2 * arrow_scale_c,
    xlabel   = PC1 * arrow_scale_c * 1.18,
    ylabel   = PC2 * arrow_scale_c * 1.18,
    var_type = if_else(str_starts(variable, "log_ic50"), "Drug IC50", "Thermal trait"),
    label    = case_when(
      variable == "log_ic50_amphotericin" ~ "AmB",
      variable == "log_ic50_fluconazole"  ~ "Flu",
      variable == "log_ic50_caspofungin"  ~ "Cas",
      TRUE ~ variable
    )
  )

pct_c <- summary(pca_c)$importance["Proportion of Variance", ] * 100

p_pca <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey80") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
  stat_ellipse(data = scores_c,
               aes(PC1, PC2, color = evolution_history), linewidth = 0.5) +
  geom_point(data = scores_c,
             aes(PC1, PC2, color = evolution_history),
             size = 1.8, alpha = 0.85) +
  geom_segment(data = loadings_c,
               aes(x = 0, y = 0, xend = xend, yend = yend, linetype = var_type),
               arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
               color = "grey30") +
  geom_text(data = loadings_c,
            aes(x = xlabel, y = ylabel, label = label),
            size = 2.6, color = "grey20", fontface = "italic") +
  scale_color_manual(values = EVO_COLORS, name = "Evolution\nhistory") +
  scale_linetype_manual(
    values = c("Drug IC50" = "solid", "Thermal trait" = "22"),
    name   = "Variable"
  ) +
  labs(
    x = sprintf("PC1 (%.1f%%)", pct_c[1]),
    y = sprintf("PC2 (%.1f%%)", pct_c[2])
  ) +
  theme_bw(base_size = BS) +
  theme(
    legend.position  = "right",
    legend.key.size  = unit(0.3, "cm"),
    legend.text      = element_text(size = 7),
    legend.title     = element_text(size = 7)
  )

# =============================================================================
# Panel K: Within-group Deming regression (full-width, 3 drug facets)
# =============================================================================

p_deming <- ggplot(deming_centered,
       aes(x = th_c_c, y = log_ic50_c, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey60", linewidth = 0.35) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey60", linewidth = 0.35) +
  geom_errorbar(
    aes(ymin = log_ic50_c - se_log_ic50, ymax = log_ic50_c + se_log_ic50),
    width = 0, alpha = 0.4, linewidth = 0.35
  ) +
  geom_errorbarh(
    aes(xmin = th_c_c - se_th, xmax = th_c_c + se_th),
    height = 0, alpha = 0.4, linewidth = 0.35
  ) +
  geom_point(size = 1.5, alpha = 0.85) +
  geom_line(
    data        = within_lines_clipped,
    aes(x = th_c_c, y = log_ic50_hat),
    inherit.aes = FALSE,
    color = "black", linewidth = 0.9
  ) +
  geom_text(
    data        = label_df,
    aes(x = x_pos, y = y_pos, label = label),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1, size = 2.4, color = "black", lineheight = 1.2
  ) +
  facet_wrap(~ drug, scales = "free") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x = "\u0394Th (\u00b0C, group-mean centred)",
    y = "\u0394log(IC50) (group-mean centred)"
  ) +
  theme_bw(base_size = BS) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = BS, face = "bold")
  )

# =============================================================================
# Assemble with patchwork
# =============================================================================

design <- "
AAAAAAAAAA
BBCCDDEEFF
GGHHIIJJJJ
KKKKKKKKKK
"

composite <- (
  p_tpc +
  p_topt + p_tmax + p_th + p_auc41 + p_auc42 +
  p_flu  + p_cas  + p_amph + p_pca +
  p_deming
) +
  plot_layout(design = design, heights = c(1.4, 1.0, 1.0, 1.0)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(size = 11, face = "bold"))
  )

ggsave(
  "figures/composite-figure.png",
  composite,
  width  = 20,
  height = 20,
  dpi    = 300
)
