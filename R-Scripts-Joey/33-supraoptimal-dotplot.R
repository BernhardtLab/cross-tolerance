# =============================================================================
# 33-supraoptimal-dotplot.R
# Dotplot of r_max and supra-optimal growth performance by evolution
# treatment, next to the thermal-trait dotplots (T_opt, T_max, T_h) in the
# style of panel D of the combined figure (21-plotting.R).
#
# r_max = peak of the fitted TPC (rmax in tpc-params-auc-gcplyr-17.csv).
#
# Supra-optimal AUC (= auc_above_topt in Script 32): integral of each
# strain's fitted TPC from its own T_opt to its own T_max (growth x degC).
# Supra-optimal mean (= mean_above_topt in Script 32): that integral divided
# by (T_max - T_opt) (growth units). Both computed the same way for the
# ancestor (fRS585).
# Mean, ancestor window: mean fitted growth over the ancestor's T_opt to T_max
# (39.4-42.2 degC), the same temperatures for every strain.
#
# Stats for r_max and supra-optimal performance follow Script 17 (as used for the
# thermal-trait stars in 21-plotting.R): Welch t-test 35 vs 40 evolved;
# one-sample t-test of each group against the ancestor value. Stars use
# unadjusted p-values. Thermal-trait stars use p_welch from Script 17.
#
# Inputs:  data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv
#          data-processed/gcplyr/tpc-predictions-auc-gcplyr-17.csv
#          data-processed/gcplyr/stats-results-auc-gcplyr-17.csv
# Outputs: data-processed/stats-supraoptimal-33.csv
#          figures/supraoptimal-dotplot-33.png
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
# 2. Load data and compute supra-optimal performance
# =============================================================================

tpc_params <- read_csv(file.path(OUT_TPC, "tpc-params-auc-gcplyr-17.csv"), show_col_types = FALSE)
tpc_preds  <- read_csv(file.path(OUT_TPC, "tpc-predictions-auc-gcplyr-17.csv"), show_col_types = FALSE)
stats_tpc  <- read_csv(file.path(OUT_TPC, "stats-results-auc-gcplyr-17.csv"), show_col_types = FALSE)

integrate_window <- function(temp, pred, lo, hi) {
  x <- sort(unique(c(lo, hi, temp[temp > lo & temp < hi])))
  y <- approx(temp, pred, xout = x)$y
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

# Fixed common window: the ancestor's T_opt to T_max
anc_window <- tpc_params |> filter(evolution_history == "fRS585") |> select(topt, tmax)
stopifnot(nrow(anc_window) == 1)

supra <- tpc_preds |>
  left_join(tpc_params |> select(strain, topt, tmax), by = "strain") |>
  group_by(strain, evolution_history, topt, tmax) |>
  summarise(
    supra_auc   = integrate_window(temp, pred, first(topt), first(tmax)),
    supra_fixed = integrate_window(temp, pred, anc_window$topt, anc_window$tmax) /
      (anc_window$tmax - anc_window$topt),
    .groups = "drop"
  ) |>
  mutate(supra = supra_auc / (tmax - topt))

stopifnot(sum(supra$evolution_history == "fRS585") == 1)


# Growth traits: r_max (fitted TPC peak), supra-optimal mean, mean over the
# ancestor window (all growth units), and supra-optimal AUC (growth x degC)
GROWTH_TRAITS <- c("rmax", "supra", "supra_fixed", "supra_auc")

growth_traits <- supra |>
  left_join(tpc_params |> select(strain, rmax), by = "strain") |>
  select(strain, evolution_history, all_of(GROWTH_TRAITS))

anc_growth <- growth_traits |>
  filter(evolution_history == "fRS585") |>
  select(all_of(GROWTH_TRAITS)) |>
  pivot_longer(everything(), names_to = "trait", values_to = "anc_val")


# =============================================================================
# 3. Stats for growth traits (as in Script 17)
# =============================================================================

growth_stats <- function(x35, x40, anc) {
  tt_groups <- t.test(x35, x40)
  tt_35     <- t.test(x35, mu = anc)
  tt_40     <- t.test(x40, mu = anc)
  tibble(
    comparison = c("35 evolved vs 40 evolved", "35 evolved vs ancestor", "40 evolved vs ancestor"),
    mean_x     = c(mean(x35), mean(x35), mean(x40)),
    ref        = c(mean(x40), anc, anc),
    diff       = mean_x - ref,
    ci_lo      = c(tt_groups$conf.int[1], tt_35$conf.int[1], tt_40$conf.int[1]),
    ci_hi      = c(tt_groups$conf.int[2], tt_35$conf.int[2], tt_40$conf.int[2]),
    p_welch    = c(tt_groups$p.value, tt_35$p.value, tt_40$p.value),
    p_wilcox   = c(wilcox.test(x35, x40)$p.value,
                   wilcox.test(x35, mu = anc)$p.value,
                   wilcox.test(x40, mu = anc)$p.value)
  )
}

stats_growth <- map_dfr(GROWTH_TRAITS, \(tr) {
  growth_stats(
    growth_traits |> filter(evolution_history == "35 evolved") |> pull(all_of(tr)),
    growth_traits |> filter(evolution_history == "40 evolved") |> pull(all_of(tr)),
    anc_growth |> filter(trait == tr) |> pull(anc_val)
  ) |>
    mutate(trait = tr, .before = 1)
})

print(stats_growth)
write_csv(stats_growth, file.path(OUT, "stats-supraoptimal-33.csv"))


# =============================================================================
# 4. Dotplot builder (same style as 21-plotting.R panel D)
# =============================================================================

TRAIT_LABELS <- c(topt = "T[opt]", tmax = "T[max]", th_c = "T[h]",
                  rmax = "r[max]", supra = "'Supra-optimal mean'",
                  supra_fixed = "'Mean, ancestor window'",
                  supra_auc = "'Supra-optimal AUC'")

trait_dotplot <- function(dat, anc_ref, stats, y_lab) {
  group_means <- dat |>
    summarise(mean_val = mean(value), se = sd(value) / sqrt(n()),
              .by = c(evolution_history, trait))

  y_range <- dat |> summarise(y_max = max(value), y_span = diff(range(value)), .by = trait)

  brackets <- stats |>
    filter(comparison == "35 evolved vs 40 evolved") |>
    mutate(trait = factor(trait, levels = levels(dat$trait)),
           xmin = "35 evolved", xmax = "40 evolved",
           annotations = p_stars(p_welch)) |>
    left_join(y_range, by = "trait") |>
    mutate(y_position = y_max + y_span * 0.18)

  anc_stars <- stats |>
    filter(str_detect(comparison, "vs ancestor")) |>
    mutate(trait = factor(trait, levels = levels(dat$trait)),
           evolution_history = factor(str_remove(comparison, " vs ancestor"), levels = EVO_LEVELS),
           label = p_stars(p_welch)) |>
    left_join(group_means, by = c("trait", "evolution_history")) |>
    left_join(y_range, by = "trait") |>
    mutate(y_pos = mean_val + se + y_span * 0.10,
           txt_face = if_else(label == "ns", "plain", "bold"),
           txt_size = if_else(label == "ns", 3.5, 5))

  ggplot(dat, aes(x = evolution_history, y = value, fill = evolution_history)) +
    geom_hline(data = anc_ref, aes(yintercept = anc_val),
               linetype = "dashed", color = "#000000", linewidth = 0.6) +
    geom_jitter(shape = 21, width = 0.12, size = 2.5, alpha = 0.6, color = "black") +
    geom_errorbar(data = group_means,
                  aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
                  linewidth = 1.2, width = 0.15, color = "black",
                  position = position_nudge(x = -0.3)) +
    geom_point(data = group_means, aes(y = mean_val, fill = evolution_history),
               shape = 21, size = 2, color = "black", stroke = 1.2,
               position = position_nudge(x = -0.3)) +
    suppressWarnings(ggsignif::geom_signif(
      data = brackets,
      aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
      manual = TRUE, inherit.aes = FALSE, tip_length = 0.02, textsize = 4.6, color = "black"
    )) +
    geom_text(data = anc_stars,
              aes(x = evolution_history, y = y_pos, label = label,
                  size = txt_size, fontface = txt_face),
              color = "black", nudge_x = -0.3) +
    facet_wrap(~ trait, scales = "free_y",
               labeller = as_labeller(TRAIT_LABELS, label_parsed)) +
    scale_fill_manual(values = EVO_COLORS, guide = "none") +
    scale_size_identity() +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
    labs(x = NULL, y = y_lab) +
    theme_evo() +
    theme(strip.background = element_blank(),
          strip.text       = element_text(size = 13),
          legend.position  = "none",
          axis.text.x      = element_blank(),
          axis.ticks.x     = element_blank(),
          axis.line.x      = element_blank(),
          axis.title.x     = element_blank())
}


# =============================================================================
# 5. Build panels
# =============================================================================

TEMP_TRAITS <- c("topt", "tmax", "th_c")

temp_dat <- tpc_params |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(strain, evolution_history, all_of(TEMP_TRAITS)) |>
  pivot_longer(all_of(TEMP_TRAITS), names_to = "trait", values_to = "value") |>
  mutate(trait = factor(trait, levels = TEMP_TRAITS),
         evolution_history = factor(evolution_history, levels = EVO_LEVELS))

temp_anc <- tpc_params |>
  filter(evolution_history == "fRS585") |>
  select(all_of(TEMP_TRAITS)) |>
  pivot_longer(everything(), names_to = "trait", values_to = "anc_val") |>
  mutate(trait = factor(trait, levels = TEMP_TRAITS))

p_temp <- trait_dotplot(
  temp_dat, temp_anc,
  stats_tpc |> filter(trait %in% TEMP_TRAITS),
  "Temperature (\u00b0C)"
)

growth_block <- function(traits, y_lab) {
  dat <- growth_traits |>
    filter(evolution_history %in% EVO_LEVELS) |>
    pivot_longer(all_of(traits), names_to = "trait", values_to = "value") |>
    mutate(trait = factor(trait, levels = traits),
           evolution_history = factor(evolution_history, levels = EVO_LEVELS))
  trait_dotplot(
    dat,
    anc_growth |> filter(trait %in% traits) |> mutate(trait = factor(trait, levels = traits)),
    stats_growth |> filter(trait %in% traits),
    y_lab
  )
}

# r_max and supra-optimal mean share growth units; the AUC is growth x degC
p_growth <- growth_block(c("rmax", "supra", "supra_fixed"), "Growth (AUC)")
p_auc    <- growth_block("supra_auc", "Integrated growth (AUC \u00d7 \u00b0C)")

p_temp + p_growth + p_auc + plot_layout(widths = c(3, 3, 1))

ggsave(file.path(FIGS, "supraoptimal-dotplot-33.png"),
       width = 20, height = 4, dpi = 300, bg = "transparent")
