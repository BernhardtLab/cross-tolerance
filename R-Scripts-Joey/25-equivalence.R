# =============================================================================
# 25-equivalence.R
# Equivalence tests (TOST) for 40 vs 35 evolved drug tolerance.
# Response: strain-level log2(IC50 / same-plate ancestor IC50).
#
# Margins (set a priori):
#   Primary:     +/- 1 log2 (2-fold) = one doubling dilution, the standard
#                reproducibility limit for antifungal susceptibility testing.
#   Sensitivity: +/- 0.5 log2 (~1.41-fold).
#
# Test: Welch two one-sided t-tests; equivalence if the 90% CI of the
# 40 - 35 difference lies within the margin. Several groups are non-normal
# (outliers; fluconazole March Set2 cluster), so a stratified percentile
# bootstrap 90% CI is reported as a robustness check.
#
# Inputs:  data-processed/normalised-ic50-per-strain.csv (from Script 11d)
# Outputs: data-processed/equivalence-tost-25.csv
#          data-processed/equivalence-tost-fluconazole-no-set2-25.csv
#          figures/equivalence-tost-25.png
# =============================================================================


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)

EVO_LEVELS  <- c("35 evolved", "40 evolved")
DRUG_LABELS <- c(fluconazole = "Fluconazole", caspofungin = "Caspofungin",
                 amphotericin = "Amphotericin B")
MARGINS <- c(primary = 1, sensitivity = 0.5)
N_BOOT  <- 9999
FIGS    <- "figures"
OUT     <- "data-processed"

theme_evo <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(
      panel.background  = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA),
      plot.background   = element_rect(fill = "transparent", colour = NA)
    )
}


# =============================================================================
# 2. Data
# =============================================================================

ic50_strain <- read_csv(file.path(OUT, "normalised-ic50-per-strain.csv"),
                        show_col_types = FALSE)

eq_input <- ic50_strain |> filter(evolution_history %in% EVO_LEVELS)

eq_input |>
  group_by(drug, evolution_history) |>
  summarise(n = n(), mean = mean(log_ratio), sd = sd(log_ratio),
            shapiro_p = shapiro.test(log_ratio)$p.value, .groups = "drop") |>
  print()


# =============================================================================
# 3. TOST
# =============================================================================

tost_welch <- function(x, y, margin) {
  lo   <- t.test(x, y, mu = -margin, alternative = "greater")
  hi   <- t.test(x, y, mu =  margin, alternative = "less")
  ci90 <- t.test(x, y, conf.level = 0.90)$conf.int
  tibble(
    diff    = mean(x) - mean(y),
    ci90_lo = ci90[1],
    ci90_hi = ci90[2],
    margin  = margin,
    t_lower = unname(lo$statistic), p_lower = lo$p.value,
    t_upper = unname(hi$statistic), p_upper = hi$p.value,
    df      = unname(lo$parameter),
    p_tost  = max(lo$p.value, hi$p.value)
  )
}

boot_ci90 <- function(x, y, n_boot = N_BOOT) {
  b <- replicate(n_boot, mean(sample(x, replace = TRUE)) - mean(sample(y, replace = TRUE)))
  tibble(boot_ci90_lo = unname(quantile(b, 0.05)),
         boot_ci90_hi = unname(quantile(b, 0.95)))
}

set.seed(2861)
tost_results <- eq_input |>
  group_by(drug) |>
  group_modify(\(d, k) {
    x <- d$log_ratio[d$evolution_history == "40 evolved"]
    y <- d$log_ratio[d$evolution_history == "35 evolved"]
    map_dfr(MARGINS, \(m) tost_welch(x, y, m), .id = "margin_type") |>
      bind_cols(boot_ci90(x, y))
  }) |>
  ungroup() |>
  mutate(
    smallest_equiv_margin = pmax(abs(ci90_lo), abs(ci90_hi)),
    equivalent            = p_tost < 0.05,
    boot_equivalent       = boot_ci90_lo > -margin & boot_ci90_hi < margin,
    fold_diff             = 2^diff,
    fold_ci90_lo          = 2^ci90_lo,
    fold_ci90_hi          = 2^ci90_hi
  )

print(tost_results |> select(drug, margin_type, margin, diff, ci90_lo, ci90_hi,
                             p_tost, boot_ci90_lo, boot_ci90_hi, equivalent,
                             boot_equivalent, smallest_equiv_margin))


# =============================================================================
# 4. Plot: 90% CIs against equivalence margins
# =============================================================================

plot_dat <- tost_results |>
  filter(margin_type == "primary") |>
  mutate(drug = factor(drug, levels = rev(names(DRUG_LABELS)), labels = rev(DRUG_LABELS)))

ggplot(plot_dat, aes(x = diff, y = drug)) +
  annotate("rect", xmin = -MARGINS["primary"], xmax = MARGINS["primary"],
           ymin = -Inf, ymax = Inf, fill = "grey92") +
  annotate("rect", xmin = -MARGINS["sensitivity"], xmax = MARGINS["sensitivity"],
           ymin = -Inf, ymax = Inf, fill = "grey82") +
  geom_vline(xintercept = 0, color = "grey50") +
  geom_errorbar(aes(xmin = ci90_lo, xmax = ci90_hi), orientation = "y",
                width = 0.15, linewidth = 0.7) +
  geom_point(size = 2.5) +
  scale_x_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, 0.5)) +
  labs(
    x = expression(paste("Difference in ", log[2], "(", IC[50], " / ancestor), 40 \u2212 35 evolved")),
    y = NULL,
    caption = paste(
      "Points: mean difference; bars: Welch 90% CI (TOST at \u03b1 = 0.05).",
      "Shading: \u00b10.5 (dark) and \u00b11 (light) log2 equivalence margins.",
      sep = "\n"
    )
  ) +
  theme_evo() +
  theme(plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "equivalence-tost-25.png"),
       width = 7, height = 3.5, dpi = 300, bg = "transparent")


# =============================================================================
# 5. Export
# =============================================================================

write_csv(tost_results, file.path(OUT, "equivalence-tost-25.csv"))


# =============================================================================
# 6. Sensitivity: fluconazole excluding March Set2 35-evolved strains
# =============================================================================
# March Set2 log-ratios depend on which ancestor replicate is used (see
# analysis_notes); these strains are the extremes of the 35-evolved
# fluconazole distribution in both directions.

ic50_plate <- read_csv(file.path(OUT, "normalised-ic50-per-plate.csv"),
                       show_col_types = FALSE)

set2_strains <- ic50_plate |>
  filter(month == "March", set == "Set2", evolution_history == "35 evolved") |>
  distinct(population) |>
  pull()

fl <- eq_input |> filter(drug == "fluconazole")

tost_fl_no_set2 <- map_dfr(MARGINS, \(m) tost_welch(
  fl |> filter(evolution_history == "40 evolved") |> pull(log_ratio),
  fl |> filter(evolution_history == "35 evolved", !population %in% set2_strains) |> pull(log_ratio),
  m
), .id = "margin_type") |>
  mutate(drug = "fluconazole", excluded = paste(set2_strains, collapse = ";"), .before = 1)

print(tost_fl_no_set2)
write_csv(tost_fl_no_set2, file.path(OUT, "equivalence-tost-fluconazole-no-set2-25.csv"))
