# =============================================================================
# 24-quadrant.R
# Quadrant plots: change in thermal traits (T_opt, T_max, T_h) vs change in
# drug tolerance relative to the ancestor (fRS585). Follows Fig. 1b / Fig. 5
# framing in Bernhardt et al. 2020 Phil Trans B (improve both / trade-off /
# worse both). Quadrants are assigned from the signs of the point estimates;
# no CIs are used.
#
# Inputs:
#   data-processed/normalised-ic50-per-strain.csv  (from Script 11d)
#   data-processed/gcplyr/tpc-boot-se-19.csv       (from Script 19)
#
# Changes:
#   Drug:    per-strain mean log2(IC50 / same-plate ancestor IC50)
#   Thermal: strain value - ancestor value
#
# Outputs — data-processed/: quadrant-changes-24.csv, quadrant-counts-24.csv
# Outputs — figures/:        quadrant-th-drug-24.png (T_h only)
#                            quadrant-thermal-drug-24.png (all three traits)
# =============================================================================


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")
EVO_LEVELS <- c("35 evolved", "40 evolved")
DRUG_LABELS <- c(fluconazole = "Fluconazole", caspofungin = "Caspofungin",
                 amphotericin = "Amphotericin B")
THERMAL <- tribble(
  ~trait, ~label,
  "topt", "Delta~T[opt]~(degree*C)",
  "tmax", "Delta~T[max]~(degree*C)",
  "th_c", "Delta~T[h]~(degree*C)"
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
# 2. Load data
# =============================================================================

ic50_strain <- read_csv(file.path(OUT, "normalised-ic50-per-strain.csv"),
                        show_col_types = FALSE)
tpc_se      <- read_csv(file.path(OUT, "gcplyr/tpc-boot-se-19.csv"),
                        show_col_types = FALSE)


# =============================================================================
# 3. Per-strain changes and quadrants
# =============================================================================

anc <- tpc_se |> filter(evolution_history == "fRS585")

thermal_change <- tpc_se |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(strain, evolution_history, all_of(THERMAL$trait)) |>
  mutate(across(all_of(THERMAL$trait), \(x) x - anc[[cur_column()]])) |>
  pivot_longer(all_of(THERMAL$trait), names_to = "trait", values_to = "d_th")

drug_change <- ic50_strain |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(strain = population, drug, d_drug = log_ratio)

changes <- drug_change |>
  inner_join(thermal_change, by = "strain", relationship = "many-to-many") |>
  mutate(
    evolution_history = factor(evolution_history, levels = EVO_LEVELS),
    drug  = factor(drug, levels = names(DRUG_LABELS), labels = DRUG_LABELS),
    trait = factor(trait, levels = THERMAL$trait),
    quadrant = case_when(
      d_th > 0 & d_drug > 0 ~ "both up",
      d_th < 0 & d_drug < 0 ~ "both down",
      d_th > 0 & d_drug < 0 ~ "thermal up, drug down",
      d_th < 0 & d_drug > 0 ~ "thermal down, drug up",
      .default = "on axis"
    )
  )

quadrant_counts <- changes |>
  count(trait, drug, evolution_history, quadrant) |>
  pivot_wider(names_from = evolution_history, values_from = n, values_fill = 0)

print(quadrant_counts, n = Inf)


# =============================================================================
# 4. Plots
# =============================================================================

plot_quadrant <- function(dat) {
  ggplot(dat, aes(x = d_th, y = d_drug, color = evolution_history)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_vline(xintercept = 0, color = "grey60") +
    geom_point(size = 2, alpha = 0.9) +
    scale_color_manual(values = EVO_COLORS, name = NULL) +
    labs(y = expression(paste("Change in drug tolerance, ", log[2], "(", IC[50], " / ancestor ", IC[50], ")"))) +
    theme_evo() +
    theme(legend.position = "bottom")
}

plot_quadrant(changes |> filter(trait == "th_c")) +
  facet_wrap(~ drug, nrow = 1) +
  labs(x = expression(paste("Change in ", T[h], " relative to ancestor (", degree, "C)")))

ggsave(file.path(FIGS, "quadrant-th-drug-24.png"),
       width = 10, height = 5.5, dpi = 300, bg = "transparent")

trait_labeller <- as_labeller(setNames(THERMAL$label, THERMAL$trait), label_parsed)

plot_quadrant(changes) +
  facet_grid(drug ~ trait, scales = "free_x",
             labeller = labeller(trait = trait_labeller)) +
  labs(x = "Change in thermal trait relative to ancestor")

ggsave(file.path(FIGS, "quadrant-thermal-drug-24.png"),
       width = 10, height = 10, dpi = 300, bg = "transparent")


# =============================================================================
# 5. Export
# =============================================================================

write_csv(changes, file.path(OUT, "quadrant-changes-24.csv"))
write_csv(quadrant_counts, file.path(OUT, "quadrant-counts-24.csv"))
