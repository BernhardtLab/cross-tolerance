# =============================================================================
# 23-rda.R
# Redundancy analysis (RDA): thermal traits + IC50 log-ratios constrained by
# evolution history. Traits are NOT centred within group (cf. 22-pca.R), so
# this asks how much multivariate trait variation selection temperature explains.
# Follows the approach in Bernhardt et al. 2020 Phil Trans B (Fig. 3), using
# vegan::rda on scaled traits (equivalent to capscale with Euclidean distance).
#
# Inputs:
#   data-processed/normalised-ic50-per-strain.csv  (from Script 11d)
#   data-processed/gcplyr/tpc-boot-se-19.csv       (from Script 19)
#
# Outputs — data-processed/:
#   rda-scores-23.csv
#   rda-loadings-23.csv
#   rda-summary-23.csv
#   partial-rda-summary-23.csv
#   partial-rda-loadings-23.csv
#
# Outputs — figures/:
#   rda-biplot-23.png
# =============================================================================


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)
library(vegan)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")
EVO_LEVELS <- c("35 evolved", "40 evolved")
FIGS       <- "figures"
OUT        <- "data-processed"

theme_evo <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(
      panel.background  = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA),
      plot.background   = element_rect(fill = "transparent", colour = NA)
    )
}

RDA_VARS <- c("topt", "tmax", "th_c", "fluconazole", "caspofungin", "amphotericin")
N_PERM   <- 9999


# =============================================================================
# 2. Load data
# =============================================================================

ic50_strain <- read_csv(file.path(OUT, "normalised-ic50-per-strain.csv"),
                        show_col_types = FALSE)
tpc_se      <- read_csv(file.path(OUT, "gcplyr/tpc-boot-se-19.csv"),
                        show_col_types = FALSE)


# =============================================================================
# 3. Build RDA input (evolved strains only; no within-group centring)
# =============================================================================

ic50_wide <- ic50_strain |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(population, drug, log_ratio) |>
  pivot_wider(names_from = drug, values_from = log_ratio)

rda_input <- tpc_se |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(strain, evolution_history, topt, tmax, th_c) |>
  inner_join(ic50_wide, by = c("strain" = "population")) |>
  drop_na() |>
  mutate(evolution_history = factor(evolution_history, levels = EVO_LEVELS))

cat(sprintf("RDA input: %d strains x %d traits\n", nrow(rda_input), length(RDA_VARS)))

traits_z <- scale(rda_input[, RDA_VARS])


# =============================================================================
# 4. Fit RDA and permutation tests
# =============================================================================

rda_fit <- rda(traits_z ~ evolution_history, data = rda_input)
print(rda_fit)

set.seed(6618)
rda_anova <- anova(rda_fit, permutations = N_PERM)
print(rda_anova)

rda_r2 <- RsquareAdj(rda_fit)
print(rda_r2)

# With two groups there is only one constrained axis (RDA1); the second plotted
# axis is the first unconstrained (residual) PC.
eig     <- eigenvals(rda_fit)
pct_tot <- eig / sum(eig) * 100

site_sc <- scores(rda_fit, display = "wa",    choices = 1:2, scaling = 2)
spp_sc  <- scores(rda_fit, display = "species", choices = 1:2, scaling = 2)

scores_df <- as_tibble(site_sc) |>
  bind_cols(rda_input |> select(strain, evolution_history))

loadings_df <- as.data.frame(spp_sc) |>
  rownames_to_column("variable")

group_means <- scores_df |>
  group_by(evolution_history) |>
  summarise(across(c(RDA1, PC1), list(mean = mean, se = ~ sd(.) / sqrt(n()))))

print(loadings_df)
print(group_means)


# =============================================================================
# 5. Project ancestor onto RDA1
# =============================================================================
# Ancestor drug log-ratios are 0 by construction. RDA1 site scores are a linear
# function of the scaled traits, so the ancestor is projected with the same map.
# The residual axis (PC1) is undefined for the ancestor (it has no group).

anc_traits <- tpc_se |>
  filter(evolution_history == "fRS585") |>
  mutate(fluconazole = 0, caspofungin = 0, amphotericin = 0) |>
  select(all_of(RDA_VARS))

anc_z <- scale(anc_traits,
               center = attr(traits_z, "scaled:center"),
               scale  = attr(traits_z, "scaled:scale"))

rda1_map <- qr.solve(traits_z, site_sc[, "RDA1", drop = FALSE])
stopifnot(isTRUE(all.equal(c(traits_z %*% rda1_map), unname(site_sc[, "RDA1"]))))
anc_rda1 <- c(anc_z %*% rda1_map)
cat(sprintf("Ancestor RDA1 score: %.3f\n", anc_rda1))


# =============================================================================
# 6. Plot
# =============================================================================

ggplot(scores_df, aes(x = RDA1, y = PC1, color = evolution_history)) +
  geom_hline(yintercept = 0, color = "grey85") +
  geom_vline(xintercept = 0, color = "grey85") +
  geom_vline(xintercept = anc_rda1, linetype = "dashed", color = "black") +
  annotate("text", x = anc_rda1, y = max(scores_df$PC1), label = "fRS585",
           hjust = -0.1, size = 3.2) +
  geom_point(size = 2.3, alpha = 0.7) +
  geom_errorbar(
    data = group_means,
    aes(x = RDA1_mean, ymin = PC1_mean - PC1_se, ymax = PC1_mean + PC1_se),
    inherit.aes = FALSE, width = 0, linewidth = 0.8,
    color = EVO_COLORS[as.character(group_means$evolution_history)]
  ) +
  geom_errorbar(
    data = group_means,
    aes(y = PC1_mean, xmin = RDA1_mean - RDA1_se, xmax = RDA1_mean + RDA1_se),
    inherit.aes = FALSE, orientation = "y", width = 0, linewidth = 0.8,
    color = EVO_COLORS[as.character(group_means$evolution_history)]
  ) +
  geom_segment(
    data = loadings_df,
    aes(x = 0, y = 0, xend = RDA1, yend = PC1),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.25, "cm")),
    color = "grey30", linewidth = 0.6
  ) +
  geom_text(
    data = loadings_df,
    aes(x = RDA1 * 1.15, y = PC1 * 1.15, label = variable),
    inherit.aes = FALSE, size = 3.5, color = "grey20"
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x = sprintf("RDA1 (%.1f%% of total variation)", pct_tot["RDA1"]),
    y = sprintf("Residual PC1 (%.1f%% of total variation)", pct_tot["PC1"]),
    caption = sprintf(
      "Scaled traits constrained by evolution history; permutation F = %.2f, p = %.4f.\nGroup means \u00b1 SE. Dashed line: ancestor projected onto RDA1.",
      rda_anova$F[1], rda_anova$`Pr(>F)`[1]
    )
  ) +
  theme_evo() +
  theme(
    legend.position = "bottom",
    plot.caption    = element_text(size = 9, color = "grey40")
  )

ggsave(file.path(FIGS, "rda-biplot-23.png"),
       width = 5.5, height = 5, dpi = 300, bg = "transparent")


# =============================================================================
# 7. Export
# =============================================================================

write_csv(scores_df, file.path(OUT, "rda-scores-23.csv"))
write_csv(loadings_df, file.path(OUT, "rda-loadings-23.csv"))
write_csv(
  tibble(
    n_strains       = nrow(rda_input),
    constrained_var = rda_fit$CCA$tot.chi,
    total_var       = rda_fit$tot.chi,
    r2              = rda_r2$r.squared,
    adj_r2          = rda_r2$adj.r.squared,
    F               = rda_anova$F[1],
    p_perm          = rda_anova$`Pr(>F)`[1],
    n_perm          = N_PERM,
    pct_RDA1        = pct_tot["RDA1"],
    pct_PC1         = pct_tot["PC1"],
    anc_RDA1        = anc_rda1
  ),
  file.path(OUT, "rda-summary-23.csv")
)

cat("\nAll outputs written.\n")


# =============================================================================
# 8. Partial RDA: drug tolerance ~ thermal traits | evolution history
# =============================================================================
# Direct test of within-group cross-tolerance: do thermal traits explain drug
# tolerance once group means are removed? Permutations are restricted within
# evolution history (blocks). T_h is nearly a linear combination of T_opt and
# T_max (VIF > 100), so per-term tests are not interpretable; a model without
# T_h is fitted as a sensitivity check.

DRUG_VARS    <- c("fluconazole", "caspofungin", "amphotericin")
THERMAL_VARS <- c("topt", "tmax", "th_c")

prda_dat <- rda_input |>
  mutate(across(all_of(THERMAL_VARS), \(x) as.numeric(scale(x))))
drug_z <- scale(rda_input[, DRUG_VARS])

perm_within <- how(nperm = N_PERM, blocks = rda_input$evolution_history)

prda_fit  <- rda(drug_z ~ topt + tmax + th_c + Condition(evolution_history), data = prda_dat)
prda_noth <- rda(drug_z ~ topt + tmax + Condition(evolution_history), data = prda_dat)

print(prda_fit)
print(vif.cca(prda_fit))

set.seed(4417)
prda_anova <- anova(prda_fit, permutations = perm_within)
set.seed(4417)
prda_axes  <- anova(prda_fit, by = "axis", permutations = perm_within)
set.seed(4417)
prda_noth_anova <- anova(prda_noth, permutations = perm_within)

print(prda_anova)
print(prda_axes)
print(prda_noth_anova)

prda_summary <- map2_dfr(
  list(prda_fit, prda_noth), list(prda_anova, prda_noth_anova),
  \(fit, aov) tibble(
    r2_partial     = RsquareAdj(fit)$r.squared,
    adj_r2_partial = RsquareAdj(fit)$adj.r.squared,
    F              = aov$F[1],
    p_perm         = aov$`Pr(>F)`[1]
  )
) |>
  mutate(model = c("topt + tmax + th_c", "topt + tmax"), n_perm = N_PERM, .before = 1)

prda_loadings <- bind_rows(
  as.data.frame(scores(prda_fit, display = "species", choices = 1:2, scaling = 2)) |>
    rownames_to_column("variable") |> mutate(type = "response (drug)"),
  as.data.frame(scores(prda_fit, display = "bp", choices = 1:2, scaling = 2)) |>
    rownames_to_column("variable") |> mutate(type = "constraint (thermal)")
)

write_csv(prda_summary,  file.path(OUT, "partial-rda-summary-23.csv"))
write_csv(prda_loadings, file.path(OUT, "partial-rda-loadings-23.csv"))
