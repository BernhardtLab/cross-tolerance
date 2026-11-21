# =============================================================================
# 43-combined-figure-v2.R
# New version of combined-figure-21.png. Built from the same inputs as
# 21-plotting.R, which is left unchanged so the old figure is kept.
#
# Layout (panel letters assigned automatically, in reading order):
#   Row 1: A  TPC curves  |  B  PCA biplot
#   Row 2: C  Thermal trait dot plots (T_opt, T_max, T_h)
#   Row 3: D  IC50 dot plots, one panel per drug
#   Row 4: E  York vs OLS scatter, one panel per drug (full width, bottom)
#   (The ancestor-window panel is built but not in the layout for now.)
#
# Inputs:  as 21-plotting.R, plus
#          data-processed/tpc-window-boot-40.csv        (Script 40)
#          data-processed/york-regression-40.csv        (Script 40)
#          data-processed/normalised-ic50-per-plate.csv (for the York panel)
# Output:  figures/combined-figure-21-v2.png
# =============================================================================


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)
library(ggsignif)
library(patchwork)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")
EVO_LEVELS <- c("35 evolved", "40 evolved")
DRUG_LABELS <- c(fluconazole = "Fluconazole", caspofungin = "Caspofungin",
                 amphotericin = "Amphotericin B")
DRUG_ORDER <- c("Fluconazole", "Amphotericin B", "Caspofungin")

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

integrate_window <- function(temp, pred, lo, hi) {
  x <- sort(unique(c(lo, hi, temp[temp > lo & temp < hi])))
  y <- approx(temp, pred, xout = x)$y
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}


# =============================================================================
# 2. Load data
# =============================================================================

tpc_preds    <- read_csv(file.path(OUT_TPC, "tpc-predictions-auc-gcplyr-17.csv"), show_col_types = FALSE)
well_metrics <- read_csv(file.path(OUT_TPC, "gcplyr-metrics-per-well-17.csv"), show_col_types = FALSE)
tpc_params   <- read_csv(file.path(OUT_TPC, "tpc-params-auc-gcplyr-17.csv"), show_col_types = FALSE)
stats_tpc    <- read_csv(file.path(OUT_TPC, "stats-results-auc-gcplyr-17.csv"), show_col_types = FALSE)

ic50_strains <- read_csv(file.path(OUT, "normalised-ic50-per-strain.csv"), show_col_types = FALSE)
stats_ic50   <- read_csv(file.path(OUT, "stats-results-normalised-ic50.csv"), show_col_types = FALSE)

pca_scores   <- read_csv(file.path(OUT, "pca-scores-within-22.csv"), show_col_types = FALSE) |>
  mutate(evolution_history = factor(evolution_history, levels = EVO_LEVELS))
pca_loadings <- read_csv(file.path(OUT, "pca-loadings-within-22.csv"), show_col_types = FALSE)
pca_pct      <- read_csv(file.path(OUT, "pca-pct-within-22.csv"), show_col_types = FALSE)

anc       <- tpc_params |> filter(evolution_history == "fRS585")
anc_curve <- tpc_preds |>
  filter(evolution_history == "fRS585") |>
  group_by(temp) |>
  summarise(pred = mean(pred), .groups = "drop")
y_topt <- approx(anc_curve$temp, anc_curve$pred, xout = anc$topt)$y
y_th   <- approx(anc_curve$temp, anc_curve$pred, xout = anc$th_c)$y
y_tmax <- approx(anc_curve$temp, anc_curve$pred, xout = anc$tmax)$y


# =============================================================================
# 3. TPC curves (as in 21-plotting.R, section 3)
# =============================================================================

mean_curves <- tpc_preds |>
  filter(evolution_history %in% c(EVO_LEVELS, "fRS585"), temp >= 23, temp <= 45) |>
  group_by(evolution_history, temp) |>
  summarise(pred = mean(pred, na.rm = TRUE), .groups = "drop")

obs_means <- well_metrics |>
  filter(!is.na(auc_gc), evolution_history %in% c(EVO_LEVELS, "fRS585")) |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(auc_gc = mean(auc_gc, na.rm = TRUE), .groups = "drop")

plot_tpc <- ggplot() +
  geom_line(
    data = tpc_preds |>
      filter(evolution_history %in% EVO_LEVELS, temp >= 23, temp <= 45) |>
      mutate(pred = ifelse(pred < 0, NA, pred)),
    aes(x = temp, y = pred, group = strain, color = evolution_history),
    alpha = 0.2, linewidth = 0.8
  ) +
  geom_line(
    data = mean_curves |> filter(evolution_history %in% EVO_LEVELS),
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 2.0
  ) +
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
  coord_cartesian(xlim = c(23, 45), clip = "off") +
  labs(x = "Temperature (\u00b0C)", y = "Growth performance (OD\u00b7day)") +
  theme_evo() +
  theme(
    legend.position        = "top",
    legend.justification   = "center",
    legend.background      = element_rect(fill = "white", colour = NA),
    legend.key.size        = unit(0.45, "cm"),
    plot.margin            = margin(5, 5, 20, 5, "pt")
  )


# =============================================================================
# 4. Ancestor-window mean growth (39.4-42.2 C), by treatment
# =============================================================================

anc_win <- tpc_params |> filter(evolution_history == "fRS585") |> select(topt, tmax)
stopifnot(nrow(anc_win) == 1)

win_dat <- tpc_preds |>
  filter(evolution_history %in% c(EVO_LEVELS, "fRS585")) |>
  group_by(strain, evolution_history) |>
  summarise(wm = integrate_window(temp, pred, anc_win$topt, anc_win$tmax) /
              (anc_win$tmax - anc_win$topt),
            .groups = "drop")

win_anc_val <- win_dat |> filter(evolution_history == "fRS585") |> pull(wm)
stopifnot(length(win_anc_val) == 1)

win_evo <- win_dat |>
  filter(evolution_history %in% EVO_LEVELS) |>
  mutate(evolution_history = factor(evolution_history, levels = EVO_LEVELS))

win_means <- win_evo |>
  summarise(mean_val = mean(wm), se = sd(wm) / sqrt(n()), .by = evolution_history)

x35 <- win_evo |> filter(evolution_history == "35 evolved") |> pull(wm)
x40 <- win_evo |> filter(evolution_history == "40 evolved") |> pull(wm)
p_vs_anc_35 <- t.test(x35, mu = win_anc_val)$p.value
p_vs_anc_40 <- t.test(x40, mu = win_anc_val)$p.value
p_35_40     <- t.test(x35, x40)$p.value

win_y_max  <- max(win_evo$wm)
win_y_span <- diff(range(win_evo$wm))

win_brackets <- tibble(xmin = "35 evolved", xmax = "40 evolved",
                       annotations = p_stars(p_35_40),
                       y_position = win_y_max + win_y_span * 0.18)

win_stars <- tibble(
  evolution_history = factor(c("35 evolved", "40 evolved"), levels = EVO_LEVELS),
  label = c(p_stars(p_vs_anc_35), p_stars(p_vs_anc_40))
) |>
  left_join(win_means, by = "evolution_history") |>
  mutate(y_pos = mean_val + se + win_y_span * 0.10,
         txt_face = if_else(label == "ns", "plain", "bold"),
         txt_size = if_else(label == "ns", 3.5, 5))

plot_window <- ggplot(win_evo, aes(x = evolution_history, y = wm, fill = evolution_history)) +
  geom_hline(yintercept = win_anc_val, linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(shape = 21, width = 0.12, size = 2.5, alpha = 0.6, color = "black") +
  geom_errorbar(data = win_means,
                aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
                linewidth = 1.2, width = 0.15, color = "black",
                position = position_nudge(x = -0.3)) +
  geom_point(data = win_means, aes(y = mean_val, fill = evolution_history),
             shape = 21, size = 2, color = "black", stroke = 1.2,
             position = position_nudge(x = -0.3)) +
  suppressWarnings(ggsignif::geom_signif(
    data = win_brackets,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual = TRUE, inherit.aes = FALSE, tip_length = 0.02, textsize = 4.6, color = "black"
  )) +
  geom_text(data = win_stars,
            aes(x = evolution_history, y = y_pos, label = label,
                size = txt_size, fontface = txt_face),
            color = "black", nudge_x = -0.3) +
  scale_fill_manual(values = EVO_COLORS, guide = "none") +
  scale_size_identity() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(x = NULL, y = "Mean growth") +
  theme_evo() +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.line.x = element_blank(), axis.title.x = element_blank())


# =============================================================================
# 5. PCA biplot (as in 21-plotting.R, section 4)
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
      mutate(
        label = recode(variable,
                       topt         = "T[opt]",
                       tmax         = "T[max]",
                       th_c         = "T[h]",
                       caspofungin  = "Casp",
                       fluconazole  = "Fluc",
                       amphotericin = "Amph"),
        y_nudge = case_when(variable == "fluconazole" ~  0.22,
                            variable == "th_c"        ~ -0.22,
                            TRUE                      ~  0),
        hjust = case_when(PC1_scaled < -0.5 ~ 1,
                          PC1_scaled >  0.5 ~ 0,
                          TRUE              ~ 0.5)
      ),
    aes(x = PC1_scaled * 1.08, y = PC2_scaled * 1.08 + y_nudge,
        label = label, hjust = hjust),
    inherit.aes = FALSE,
    size = 3.2, color = "grey20", parse = TRUE
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.12, 0.05))) +
  scale_fill_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x = sprintf("PC1 (%.1f%%)", pc1_pct),
    y = sprintf("PC2 (%.1f%%)", pc2_pct)
  ) +
  theme_evo() +
  theme(legend.position = "none")


# =============================================================================
# 6. Thermal trait dot plots (as in 21-plotting.R, section 5)
# =============================================================================

TRAIT_LEVELS <- c("topt", "tmax", "th_c")

plot_data_traits <- tpc_params |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(strain, evolution_history, topt, tmax, th_c) |>
  pivot_longer(c(topt, tmax, th_c), names_to = "trait", values_to = "value") |>
  mutate(trait = factor(trait, levels = TRAIT_LEVELS),
         evolution_history = factor(evolution_history, levels = EVO_LEVELS))

anc_ref_traits <- tpc_params |>
  filter(evolution_history == "fRS585") |>
  select(topt, tmax, th_c) |>
  pivot_longer(everything(), names_to = "trait", values_to = "anc_val") |>
  mutate(trait = factor(trait, levels = TRAIT_LEVELS))

group_means_traits <- plot_data_traits |>
  summarise(mean_val = mean(value), se = sd(value) / sqrt(n()),
            .by = c(evolution_history, trait))

y_range_traits <- plot_data_traits |>
  summarise(y_max = max(value), y_span = diff(range(value)), .by = trait)

bracket_traits <- stats_tpc |>
  filter(comparison == "35 evolved vs 40 evolved") |>
  mutate(trait = factor(trait, levels = TRAIT_LEVELS), xmin = "35 evolved",
         xmax = "40 evolved", annotations = p_stars(p_welch)) |>
  filter(!is.na(trait)) |>
  left_join(y_range_traits, by = "trait") |>
  mutate(y_position = y_max + y_span * 0.18)

anc_stars_traits <- stats_tpc |>
  filter(str_detect(comparison, "vs ancestor")) |>
  mutate(trait = factor(trait, levels = TRAIT_LEVELS),
         evolution_history = factor(str_remove(comparison, " vs ancestor"), levels = EVO_LEVELS),
         label = p_stars(p_welch)) |>
  filter(!is.na(trait)) |>
  left_join(group_means_traits, by = c("trait", "evolution_history")) |>
  left_join(y_range_traits, by = "trait") |>
  mutate(y_pos = mean_val + se + y_span * 0.10,
         txt_face = if_else(label == "ns", "plain", "bold"),
         txt_size = if_else(label == "ns", 3.5, 5))

plot_traits <- ggplot(plot_data_traits,
                      aes(x = evolution_history, y = value, fill = evolution_history)) +
  geom_hline(data = anc_ref_traits, aes(yintercept = anc_val),
             linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(shape = 21, width = 0.12, size = 2.5, alpha = 0.6, color = "black") +
  geom_errorbar(data = group_means_traits,
                aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
                linewidth = 1.2, width = 0.15, color = "black",
                position = position_nudge(x = -0.3)) +
  geom_point(data = group_means_traits, aes(y = mean_val, fill = evolution_history),
             shape = 21, size = 2, color = "black", stroke = 1.2,
             position = position_nudge(x = -0.3)) +
  suppressWarnings(ggsignif::geom_signif(
    data = bracket_traits,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual = TRUE, inherit.aes = FALSE, tip_length = 0.02, textsize = 4.6, color = "black"
  )) +
  geom_text(data = anc_stars_traits,
            aes(x = evolution_history, y = y_pos, label = label,
                size = txt_size, fontface = txt_face),
            color = "black", nudge_x = -0.3) +
  facet_wrap(~ trait, scales = "free_y",
             labeller = as_labeller(c(topt = "T[opt]", tmax = "T[max]", th_c = "T[h]"),
                                    label_parsed)) +
  scale_fill_manual(values = EVO_COLORS, guide = "none") +
  scale_size_identity() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(x = NULL, y = "Temperature (\u00b0C)") +
  theme_evo() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 13),
        legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.line.x = element_blank(), axis.title.x = element_blank())


# =============================================================================
# 7. York vs OLS scatter, one panel per drug (as in 41-york-vs-ols-plot.R)
# =============================================================================

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

x_dat <- tpc_params |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(strain, evolution_history) |>
  mutate(x = map2_dbl(strain, evolution_history, \(s, g) {
    cv <- tpc_preds |> filter(strain == s) |> arrange(temp)
    integrate_window(cv$temp, cv$pred, anc_win$topt, anc_win$tmax) /
      (anc_win$tmax - anc_win$topt)
  }))

boot_se <- read_csv(file.path(OUT, "tpc-window-boot-40.csv"), show_col_types = FALSE) |>
  select(strain, evolution_history, sx = se_fixed)

dat_york <- y_dat |>
  inner_join(x_dat, by = c("strain", "evolution_history")) |>
  inner_join(boot_se, by = c("strain", "evolution_history")) |>
  mutate(drug_l = factor(DRUG_LABELS[drug], levels = DRUG_ORDER))

york_res <- read_csv(file.path(OUT, "york-regression-40.csv"), show_col_types = FALSE) |>
  filter(x_error == "replicate SD (Script 40)") |>
  mutate(drug_l = factor(drug, levels = DRUG_ORDER))

rng_y <- dat_york |>
  group_by(drug_l, evolution_history) |>
  summarise(x1 = min(x), x2 = max(x), .groups = "drop")

ols_band <- dat_york |>
  group_by(drug_l, evolution_history) |>
  group_modify(\(d, k) {
    m  <- lm(y ~ x, data = d)
    nd <- tibble(x = seq(min(d$x), max(d$x), length.out = 60))
    pr <- predict(m, newdata = nd, interval = "confidence")
    nd |> mutate(fit = pr[, "fit"], lo = pr[, "lwr"], hi = pr[, "upr"])
  }) |>
  ungroup()

york_band <- york_res |>
  left_join(rng_y, by = c("drug_l", "evolution_history")) |>
  rowwise() |>
  reframe(drug_l = drug_l, evolution_history = evolution_history,
          x = seq(x1, x2, length.out = 60),
          fit = york_pivot_y + york_slope * (x - york_pivot_x),
          lo = fit - (york_hi - york_lo) / 2 * abs(x - york_pivot_x),
          hi = fit + (york_hi - york_lo) / 2 * abs(x - york_pivot_x)) |>
  ungroup()

york_seg <- york_res |>
  left_join(rng_y, by = c("drug_l", "evolution_history")) |>
  transmute(drug_l, evolution_history, x1, x2,
            y1 = york_intercept + york_slope * x1,
            y2 = york_intercept + york_slope * x2)

plot_york <- ggplot(dat_york, aes(x = x, y = y, color = evolution_history)) +
  geom_ribbon(data = ols_band,
              aes(x = x, ymin = lo, ymax = hi, group = evolution_history),
              fill = "grey60", alpha = 0.25, color = NA, inherit.aes = FALSE) +
  geom_ribbon(data = york_band,
              aes(x = x, ymin = lo, ymax = hi, fill = evolution_history,
                  group = evolution_history),
              alpha = 0.2, color = NA, inherit.aes = FALSE) +
  geom_line(data = ols_band, aes(x = x, y = fit, color = evolution_history),
            linetype = "dashed", linewidth = 0.6, inherit.aes = FALSE) +
  geom_segment(data = york_seg,
               aes(x = x1, xend = x2, y = y1, yend = y2, color = evolution_history),
               linewidth = 0.8, inherit.aes = FALSE) +
  geom_errorbar(aes(ymin = y - sy, ymax = y + sy), width = 0, alpha = 0.35) +
  geom_errorbar(aes(xmin = x - sx, xmax = x + sx), orientation = "y", width = 0, alpha = 0.35) +
  geom_point(aes(fill = evolution_history), shape = 21, size = 2, color = "black", stroke = 0.4) +
  facet_wrap(~ drug_l, scales = "free", nrow = 1) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_fill_manual(values = EVO_COLORS, guide = "none") +
  labs(x = "Growth, 39.4\u201342.2 \u00b0C (mean; error bars: SE)",
       y = expression(log[2] ~ "IC"[50] ~ "fold change")) +
  theme_evo() +
  theme(legend.position = "none", strip.background = element_blank(),
        strip.text = element_text(size = 12))


# =============================================================================
# 8. IC50 dot plots (as in 21-plotting.R, section 6)
# =============================================================================

plot_data_ic50 <- ic50_strains |>
  filter(evolution_history %in% EVO_LEVELS) |>
  mutate(evolution_history = factor(evolution_history, levels = EVO_LEVELS))

gmeans_ic50 <- plot_data_ic50 |>
  summarise(mean_val = mean(log_ratio), se = sd(log_ratio) / sqrt(n()),
            .by = c(evolution_history, drug))

y_range_ic50 <- plot_data_ic50 |>
  summarise(y_max = max(log_ratio), y_span = diff(range(log_ratio)), .by = drug)

bracket_ic50 <- stats_ic50 |>
  filter(comparison == "40 evolved vs 35 evolved") |>
  mutate(xmin = "35 evolved", xmax = "40 evolved", annotations = p_stars(p_welch)) |>
  left_join(y_range_ic50, by = "drug") |>
  mutate(y_position = y_max + y_span * 0.18)

anc_stars_ic50 <- stats_ic50 |>
  filter(str_detect(comparison, "vs ancestor")) |>
  mutate(evolution_history = factor(str_remove(comparison, " vs ancestor"), levels = EVO_LEVELS),
         label = p_stars(p_welch)) |>
  left_join(gmeans_ic50, by = c("drug", "evolution_history")) |>
  left_join(y_range_ic50, by = "drug") |>
  mutate(y_pos = mean_val + se + y_span * 0.10,
         txt_face = if_else(label == "ns", "plain", "bold"),
         txt_size = if_else(label == "ns", 3.5, 5))

plot_ic50 <- ggplot(plot_data_ic50,
                    aes(x = evolution_history, y = log_ratio, fill = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(shape = 21, width = 0.12, size = 2.5, alpha = 0.6, color = "black") +
  geom_errorbar(data = gmeans_ic50,
                aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
                linewidth = 1.2, width = 0.15, color = "black",
                position = position_nudge(x = -0.3)) +
  geom_point(data = gmeans_ic50, aes(y = mean_val, fill = evolution_history),
             shape = 21, size = 2, color = "black", stroke = 1.2,
             position = position_nudge(x = -0.3)) +
  suppressWarnings(ggsignif::geom_signif(
    data = bracket_ic50,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual = TRUE, inherit.aes = FALSE, tip_length = 0.02, textsize = 4.6, color = "black"
  )) +
  geom_text(data = anc_stars_ic50,
            aes(x = evolution_history, y = y_pos, label = label,
                size = txt_size, fontface = txt_face),
            color = "black", nudge_x = -0.3) +
  facet_wrap(~ factor(drug, levels = c("fluconazole", "amphotericin", "caspofungin"),
                       labels = c("Fluconazole", "Amphotericin", "Caspofungin")), scales = "free_y") +
  scale_fill_manual(values = EVO_COLORS, guide = "none") +
  scale_size_identity() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(x = "Evolution history", y = expression(log[2](IC[50] / "ancestor IC"[50]))) +
  theme_evo() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 13),
        legend.position = "none")


# =============================================================================
# 9. Combine and save
# =============================================================================

# Panel B (ancestor-window mean, plot_window) is removed from the layout for now.
# Its code above is kept so it can be reinstated.
# Ancestor-window inset (plot_window) in the lower-left of panel A, where the legend was.
# annotation_custom rather than inset_element, so the inset does not get its own tag letter.
top_tpc <- plot_tpc +
  annotation_custom(ggplotGrob(plot_window + theme(plot.background = element_rect(fill = "white", colour = NA))),
                    xmin = 23.2, xmax = 35.0, ymin = -0.03, ymax = 0.52)

combined <- ((top_tpc | plot_pca) + plot_layout(widths = c(1, 1))) /
  plot_traits /
  plot_ic50 /
  plot_york +
  plot_layout(heights = c(1.35, 1, 1, 1.1)) +
  plot_annotation(tag_levels = "A") &
  theme(axis.title  = element_text(size = 12),
        axis.text   = element_text(size = 11),
        plot.margin = margin(0, 2, 0, 2, "pt"),
        plot.tag.position = c(0.005, 0.94),
        plot.tag = element_text(hjust = 0, vjust = 1))

ggsave(file.path(FIGS, "combined-figure-21-v2.png"),
       combined, width = 8.5, height = 11.5, dpi = 300, bg = "transparent")
