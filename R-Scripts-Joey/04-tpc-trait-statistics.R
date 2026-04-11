### Statistical tests for TPC traits among evolution treatments
#
# Author: Joey Bernhardt
# Input:  data-processed/all-SS-traits-sliding-window.csv
# Output: console output (ANOVA tables + Tukey HSD results)
#         figures/tpc-traits-stats-*.png
#
# Tests whether topt, tmax, and breadth differ among evolution treatments,
# comparing 35 evolved, 40 evolved, and the ancestor (fRS585).
# Fluconazole evolved and caspofungin evolved are excluded.


# packages ----------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(emmeans)
library(plotrix)

theme_set(theme_cowplot())


# read and filter data ----------------------------------------------------

traits <- read_csv("data-processed/all-SS-traits-sliding-window.csv") |>
  filter(evolution_history %in% c("35 evolved", "40 evolved", "fRS585")) |>
  mutate(evolution_history = factor(evolution_history,
                                    levels = c("fRS585", "35 evolved", "40 evolved")))


# linear models -----------------------------------------------------------
# One model per trait with evolution_history as the predictor (fRS585 is
# the reference level, so model coefficients give contrasts vs ancestor directly)

mod_topt    <- lm(topt    ~ evolution_history, data = traits)
mod_tmax    <- lm(tmax    ~ evolution_history, data = traits)
mod_breadth <- lm(breadth ~ evolution_history, data = traits)

summary(mod_topt)


# three pairwise contrasts of interest ------------------------------------
# 1. 35 evolved vs 40 evolved  (did opposite thermal directions diverge?)
# 2. 35 evolved vs fRS585      (did cold-direction evolution shift traits?)
# 3. 40 evolved vs fRS585      (did warm-direction evolution shift traits?)

print_contrasts <- function(model, trait_name) {
  em <- emmeans(model, ~ evolution_history)
  contrasts <- contrast(em, method = list(
    "35 evolved vs 40 evolved" = c(0,  1, -1),
    "35 evolved vs fRS585"     = c(-1, 1,  0),
    "40 evolved vs fRS585"     = c(-1, 0,  1)
  ), adjust = "bonferroni")

  cat("\n====== Contrasts:", trait_name, "======\n")
  print(summary(contrasts))
}

print_contrasts(mod_topt,    "topt")
print_contrasts(mod_tmax,    "tmax")
print_contrasts(mod_breadth, "breadth")


# compact letter display (for adding to plots) ----------------------------

cld_topt    <- multcomp::cld(emmeans(mod_topt,    ~ evolution_history), Letters = letters)
cld_tmax    <- multcomp::cld(emmeans(mod_tmax,    ~ evolution_history), Letters = letters)
cld_breadth <- multcomp::cld(emmeans(mod_breadth, ~ evolution_history), Letters = letters)


# plot helper -------------------------------------------------------------
# Shows individual points, mean ± SE, and Tukey grouping letters

plot_trait_stats <- function(trait_col, cld_df, ylabel) {

  trait_summary <- traits |>
    group_by(evolution_history) |>
    summarise(
      mean_val = mean(.data[[trait_col]], na.rm = TRUE),
      se_val   = std.error(.data[[trait_col]]),
      .groups  = "drop"
    )

  letter_df <- as_tibble(cld_df) |>
    mutate(evolution_history = factor(evolution_history,
                                      levels = levels(traits$evolution_history))) |>
    left_join(trait_summary, by = "evolution_history") |>
    mutate(letter_y = mean_val + se_val + 0.3)  # position letters above error bar

  # .group is the column name in emmeans >= 1.8; fall back to .letter for older versions
  letter_col <- if (".group" %in% names(letter_df)) ".group" else ".letter"

  ggplot() +
    geom_jitter(
      data  = traits,
      aes(x = evolution_history, y = .data[[trait_col]]),
      width = 0.15, alpha = 0.5, size = 2
    ) +
    geom_pointrange(
      data = trait_summary,
      aes(x    = evolution_history,
          y    = mean_val,
          ymin = mean_val - se_val,
          ymax = mean_val + se_val),
      size = 0.6
    ) +
    geom_text(
      data = letter_df,
      aes(x = evolution_history, y = letter_y, label = trimws(.data[[letter_col]])),
      size = 4
    ) +
    labs(x = NULL, y = ylabel) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}


# plots -------------------------------------------------------------------

p_topt    <- plot_trait_stats("topt",    cld_topt,    "Topt (°C)")
p_tmax    <- plot_trait_stats("tmax",    cld_tmax,    "Tmax (°C)")
p_breadth <- plot_trait_stats("breadth", cld_breadth, "Thermal breadth (°C)")

plot_grid(p_topt, p_tmax, p_breadth, nrow = 1)
ggsave("figures/tpc-traits-stats.png", width = 12, height = 5)
