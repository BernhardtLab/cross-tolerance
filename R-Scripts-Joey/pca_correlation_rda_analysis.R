# ============================================================
# IC50 (amphotericin, fluconazole, caspofungin) vs thermal traits
# (e, eh, th, b80): PCA, correlation, and RDA
# (tmax excluded -- it's derived from th/e/eh in the Sharpe-Schoolfield
# model, so it's largely redundant with th already in this variable set)
#
# Data: all-traits.csv -- long format, one row per population x drug
# (111 rows = 37 populations x 3 drugs). Trait columns are duplicated
# across each population's 3 drug-rows; only ic50 varies row to row.
# evolution_history: "35 evolved" (n=18), "40 evolved" (n=18),
# "fRS585" (n=1, presumably ancestral/reference -- too small for any
# group-level test, excluded from group comparisons below).
# ============================================================

library(tidyverse)
library(vegan)     # install.packages("vegan") if needed

dat <- read.csv("data-processed/all-traits.csv")

# ------------------------------------------------------------
# 1. Reshape long -> wide (one row per population)
# ------------------------------------------------------------
traits <- dat %>%
  distinct(population, evolution_history, e, eh, th, b80, tmax)

ic50_wide <- dat %>%
  select(population, drug, ic50) %>%
  pivot_wider(names_from = drug, values_from = ic50, names_prefix = "ic50_")

wide <- traits %>%
  left_join(ic50_wide, by = "population") %>%
  mutate(
    log_ic50_amphotericin = log(ic50_amphotericin),
    log_ic50_fluconazole  = log(ic50_fluconazole),
    log_ic50_caspofungin  = log(ic50_caspofungin)
  )

table(wide$evolution_history)   # confirm: 35 evolved=18, 40 evolved=18, fRS585=1

vars   <- c("log_ic50_amphotericin", "log_ic50_fluconazole", "log_ic50_caspofungin",
            "e", "eh", "th", "b80", "tmax")
drugs  <- c("log_ic50_amphotericin", "log_ic50_fluconazole", "log_ic50_caspofungin")
traits_v <- c("b80", "e", "eh", "th", "tmax")

# ------------------------------------------------------------
# 2. PCA (unconstrained), all 37 populations
# ------------------------------------------------------------
pca <- prcomp(wide[, vars], center = TRUE, scale. = TRUE)
summary(pca)        # variance explained per axis
pca$rotation         # loadings

screeplot(pca, type = "lines", main = "PCA scree plot")
biplot(pca, scale = 0, cex = 0.6)

scores <- as.data.frame(pca$x) %>%
  mutate(population = wide$population,
         evolution_history = wide$evolution_history)

ggplot(scores, aes(PC1, PC2, color = evolution_history)) +
  geom_point(size = 2) +
  stat_ellipse() +
  theme_minimal() +
  labs(title = "PCA: IC50 (3 drugs) + thermal traits")



ggplot(scores, aes(PC1, PC2, color = evolution_history)) +
  geom_point(size = 2) +
  stat_ellipse() +
  scale_color_manual(values = c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")) +
  theme_minimal() +
  labs(title = "PCA: IC50 (3 drugs) + thermal traits")



library(tidyverse)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")

# Scale arrows to fit the score cloud
loadings <- as.data.frame(pca$rotation[, 1:2])
loadings$variable <- rownames(loadings)

# Compute a multiplier so arrows reach ~70% of the score cloud extent
score_range <- max(abs(scores$PC1), abs(scores$PC2))
arrow_range  <- max(sqrt(loadings$PC1^2 + loadings$PC2^2))
arrow_scale  <- 0.70 * score_range / arrow_range

loadings <- loadings |>
  mutate(
    xend = PC1 * arrow_scale,
    yend = PC2 * arrow_scale,
    # nudge labels slightly beyond arrowhead
    xlabel = PC1 * arrow_scale * 1.12,
    ylabel = PC2 * arrow_scale * 1.12,
    var_type = if_else(str_starts(variable, "log_ic50"), "Drug IC50", "Thermal trait"),
    label = case_when(
      variable == "log_ic50_amphotericin" ~ "AmB IC50",
      variable == "log_ic50_fluconazole"  ~ "Flu IC50",
      variable == "log_ic50_caspofungin"  ~ "Cas IC50",
      TRUE ~ variable
    )
  )

ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey80") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
  stat_ellipse(data = dplyr::filter(scores, evolution_history != "fRS585"),
               aes(PC1, PC2, color = evolution_history)) +
  geom_point(data = scores, aes(PC1, PC2, color = evolution_history), size = 2.2, alpha = 0.85) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = xend, yend = yend, linetype = var_type),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               color = "grey30") +
  geom_text(data = loadings,
            aes(x = xlabel, y = ylabel, label = label),
            size = 3, color = "grey20", fontface = "italic") +
  scale_color_manual(values = EVO_COLORS, name = "Evolution history") +
  scale_linetype_manual(values = c("Drug IC50" = "solid", "Thermal trait" = "22"),
                        name = "Variable type") +
  labs(
    x = "PC1 (38.0%)",
    y = "PC2 (26.3%)",
    title = "PCA biplot: drug IC50 + thermal traits"
  ) +
  theme_minimal() +
  theme(legend.position = "right")#
ggsave("figures/PCA-biplot.png", width = 8, height = 6)


# ------------------------------------------------------------
# 2b. Within-group PCA (group-mean centered)
#
# Group-mean centering removes the between-group shift from every
# variable before running PCA. The resulting ordination reflects
# only within-group covariation — the same logic as the group-mean
# centering used in the Deming regression. fRS585 (n=1) is excluded
# because centering a single observation produces all zeros.
#
# center = FALSE and scale. = TRUE: we already removed group means
# manually, so prcomp only needs to scale to unit within-group SD.
# ------------------------------------------------------------

wide_centered <- wide |>
  dplyr::filter(evolution_history != "fRS585") |>
  dplyr::group_by(evolution_history) |>
  dplyr::mutate(dplyr::across(dplyr::all_of(vars), \(x) x - mean(x, na.rm = TRUE))) |>
  dplyr::ungroup()

pca_c <- prcomp(wide_centered[, vars], center = FALSE, scale. = TRUE)

cat("=== Within-group PCA: variance explained ===\n")
print(summary(pca_c)$importance[, 1:5])
cat("\n=== Loadings (PC1-PC3) ===\n")
print(round(pca_c$rotation[, 1:3], 3))

scores_c <- as.data.frame(pca_c$x) %>%
  mutate(population       = wide_centered$population,
         evolution_history = wide_centered$evolution_history)

loadings_c <- as.data.frame(pca_c$rotation[, 1:2])
loadings_c$variable <- rownames(loadings_c)

score_range_c <- max(abs(scores_c$PC1), abs(scores_c$PC2))
arrow_range_c  <- max(sqrt(loadings_c$PC1^2 + loadings_c$PC2^2))
arrow_scale_c  <- 0.70 * score_range_c / arrow_range_c

loadings_c <- loadings_c %>%
  mutate(
    xend     = PC1 * arrow_scale_c,
    yend     = PC2 * arrow_scale_c,
    xlabel   = PC1 * arrow_scale_c * 1.14,
    ylabel   = PC2 * arrow_scale_c * 1.14,
    var_type = if_else(str_starts(variable, "log_ic50"), "Drug IC50", "Thermal trait"),
    label    = case_when(
      variable == "log_ic50_amphotericin" ~ "AmB IC50",
      variable == "log_ic50_fluconazole"  ~ "Flu IC50",
      variable == "log_ic50_caspofungin"  ~ "Cas IC50",
      TRUE ~ variable
    )
  )

pct_c <- summary(pca_c)$importance["Proportion of Variance", ] * 100

ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey80") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
  stat_ellipse(data = scores_c,
               aes(PC1, PC2, color = evolution_history)) +
  geom_point(data = scores_c,
             aes(PC1, PC2, color = evolution_history), size = 2.2, alpha = 0.85) +
  geom_segment(data = loadings_c,
               aes(x = 0, y = 0, xend = xend, yend = yend, linetype = var_type),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               color = "grey30") +
  geom_text(data = loadings_c,
            aes(x = xlabel, y = ylabel, label = label),
            size = 3, color = "grey20", fontface = "italic") +
  scale_color_manual(values = EVO_COLORS, name = "Evolution history") +
  scale_linetype_manual(values = c("Drug IC50" = "solid", "Thermal trait" = "22"),
                        name = "Variable type") +
  labs(
    x       = sprintf("PC1 (%.1f%%)", pct_c[1]),
    y       = sprintf("PC2 (%.1f%%)", pct_c[2]),
    title   = "Within-group PCA biplot: drug IC50 + thermal traits",
    caption = paste(
      "Variables group-mean centered within each evolution history before PCA.",
      "\nBetween-group separation removed; axes reflect within-group covariation only."
    )
  ) +
  theme_minimal() +
  theme(legend.position = "right",
        plot.caption = element_text(size = 8, color = "grey40"))

ggsave("figures/PCA-biplot-within.png", width = 8, height = 6, dpi = 300)


------------------------------------------------------------
# 3. Is IC50 positively associated with b80, e, eh, th?
#    Pooled AND within evolution_history group. Pooling alone can
#    manufacture a strong correlation purely from between-group
#    separation (35 vs 40 evolved differ on both axes) even when
#    no real within-group relationship exists -- the same artifact
#    diagnosed earlier with the SMA fits. fRS585 (n=1) dropped from
#    the within-group tests.
# ------------------------------------------------------------
cor_table <- expand.grid(drug = drugs, trait = traits_v, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(
    r_pooled = cor.test(wide[[drug]], wide[[trait]])$estimate,
    p_pooled = cor.test(wide[[drug]], wide[[trait]])$p.value
  ) %>%
  ungroup()
print(cor_table, n = Inf)

for (grp in c("35 evolved", "40 evolved")) {
  sub <- filter(wide, evolution_history == grp)
  cat("\n---", grp, "(n =", nrow(sub), ") ---\n")
  for (d in drugs) {
    for (t in traits_v) {
      ct <- cor.test(sub[[d]], sub[[t]])
      cat(sprintf("%-24s vs %-4s: r=%+.3f  p=%.4f\n", d, t, ct$estimate, ct$p.value))
    }
  }
}
# NOTE: this runs 12 pooled + 24 within-group tests -- treat isolated
# p<0.05 hits as exploratory, not confirmatory (~1-2 expected by chance alone).

# ------------------------------------------------------------
# 4. RDA: how much of the variation in IC50+traits is associated
#    with evolution_history? (constrained ordination -- the
#    multivariate analogue of "% variance explained" by group;
#    fRS585 excluded, n=1 can't be tested)
# ------------------------------------------------------------
rda_dat <- wide %>% filter(evolution_history != "fRS585")

mod <- rda(rda_dat[, vars] ~ evolution_history, data = rda_dat, scale = TRUE)

summary(mod)
RsquareAdj(mod)                     # unbiased proportion of variance explained
anova.cca(mod, permutations = 999)  # permutation significance test

# ------------------------------------------------------------
# RDA triplot (ggplot) with response-variable arrows, so direction
# (positive/negative association with evolution_history) is easy
# to read off directly.
# ------------------------------------------------------------
site_sc <- as.data.frame(scores(mod, display = "sites", scaling = 2)) %>%
  mutate(evolution_history = rda_dat$evolution_history)

spp_sc <- as.data.frame(scores(mod, display = "species", scaling = 2))
spp_sc$trait <- rownames(spp_sc)

# scale arrows to a readable length relative to the site cloud
arrow_mul <- 0.8 * max(abs(site_sc$RDA1), abs(site_sc$PC1)) /
                    max(abs(spp_sc$RDA1), abs(spp_sc$PC1))
spp_sc <- spp_sc %>% mutate(RDA1 = RDA1 * arrow_mul, PC1 = PC1 * arrow_mul)

# sign of RDA1 is arbitrary (depends on dummy coding) -- check directly
# which group sits on the positive side rather than assume
site_sc %>% group_by(evolution_history) %>% summarise(mean_RDA1 = mean(RDA1))

# the actual answer to "which traits are positive/negative" -- read this
# table, not the arrow angles in the plot (the plot's y-axis is PC1,
# which is irrelevant to the evolution_history question; only each
# trait's RDA1 value matters). A trait with positive RDA1 is elevated
# in whichever group has the positive mean_RDA1 above; negative RDA1
# means elevated in the other group.
scores(mod, display = "species", scaling = 2)[, "RDA1"] %>%
  sort(decreasing = TRUE)

ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  geom_point(data = site_sc, aes(RDA1, PC1, color = evolution_history), size = 2.5) +
  geom_segment(data = spp_sc, aes(x = 0, y = 0, xend = RDA1, yend = PC1),
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_text(data = spp_sc, aes(RDA1, PC1, label = trait),
            hjust = -0.15, vjust = -0.15, size = 3.5, color = "black") +
  theme_minimal() +
  labs(title = "RDA triplot: IC50+traits ~ evolution_history",
       subtitle = "Arrows point toward the group with higher values of that trait",
       x = paste0("RDA1 (", round(100 * mod$CCA$eig[1] / mod$tot.chi, 1), "% of variance, constrained)"),
       y = paste0("PC1 residual (", round(100 * mod$CA$eig[1] / mod$tot.chi, 1), "% of variance)"))
ggsave("figures/RDA-plot.png", width = 8, height = 6)
# Cross-check computed directly on the raw data (between-group SS /
# total SS on the same standardized variables) gives ~33% of total
# variance associated with evolution_history (33.3% incl. fRS585,
# 33.1% with just 35 vs 40 evolved) -- RsquareAdj() should land close
# to this, adjusted downward slightly for the 1 d.f. used by the model.
