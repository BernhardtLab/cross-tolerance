# =============================================================================
# 30-gam-difference-curves.R
# Where along the concentration axis do 40- and 35-evolved strains differ?
#
# Response (per well): log2 fold change in relative growth vs the same-plate
# ancestor at the same concentration:
#   log2_fc = log2((rel_strain + EPS) / (rel_anc + EPS)),
#   rel = OD / zero-drug OD in the same row.
# EPS = 0.05 (5% of no-drug growth): ~45-55% of caspofungin / amphotericin
# wells are fully or nearly fully inhibited, so wells where both strain and
# ancestor are inhibited give log2_fc ~ 0 rather than a ratio of noise.
# Sensitivity: EPS = 0.02 and 0.1.
#
# Model (one per drug):
#   log2_fc ~ grp40 + s(lc) + s(lc, by = grp40) + s(lc, population, bs = "fs")
# lc = log2(concentration); grp40 = ordered factor, so s(lc, by = grp40) is the
# 40 - 35 difference smooth; strain-level factor smooths make strains the
# replicates. Primary model has no month term: per-plate ancestor
# normalisation is assumed to remove month effects. A sensitivity model adds a
# month term (month + s(lc, by = march)); it is not the primary analysis.
#
# CIs: 95% pointwise and simultaneous (simulation, max |t| over the curve).
#
# Inputs:  data-processed/dr-relative-growth-27.csv (from Script 27)
# Outputs: data-processed/gam-difference-curves-30.csv
#          data-processed/gam-summaries-30.csv
#          data-processed/gam-sig-regions-30.csv
#          figures/gam-difference-curves-30.png
#          figures/gam-difference-curves-sensitivity-30.png
# =============================================================================

library(tidyverse)
library(mgcv)

DRUG_LABELS <- c(fluconazole = "Fluconazole", caspofungin = "Caspofungin",
                 amphotericin = "Amphotericin B")
SHEET_KEYS <- c("drug", "month", "set", "rep")
EPS_PRIMARY <- 0.05
EPS_SENS    <- c(0.02, 0.1)
N_SIM <- 10000
FIGS  <- "figures"
OUT   <- "data-processed"

theme_evo <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(panel.background = element_blank(),
          legend.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          strip.background = element_blank())
}


# =============================================================================
# 1. Plate-level data
# =============================================================================

rel <- read_csv(file.path(OUT, "dr-relative-growth-27.csv"), show_col_types = FALSE) |>
  filter(concentration > 0)

anc_rel <- rel |>
  filter(population == "fRS585") |>
  select(all_of(SHEET_KEYS), concentration, rel_anc = rel_growth)

gam_dat <- rel |>
  filter(population != "fRS585") |>
  left_join(anc_rel, by = c(SHEET_KEYS, "concentration")) |>
  mutate(
    lc         = log2(concentration),
    population = factor(population),
    grp40      = ordered(evolution_history == "40 evolved", levels = c(FALSE, TRUE)),
    march      = ordered(month == "March", levels = c(FALSE, TRUE)),
    month      = factor(month)
  )

stopifnot(!anyNA(gam_dat$rel_anc))


# =============================================================================
# 2. Fit GAMs and difference curves
# =============================================================================

fit_diff <- function(d, eps, with_month) {
  d <- droplevels(d) |> mutate(y = log2((rel_growth + eps) / (rel_anc + eps)))

  f <- if (with_month) {
    y ~ grp40 + month + s(lc, k = 8) + s(lc, by = grp40, k = 8) +
      s(lc, by = march, k = 8) + s(lc, population, bs = "fs", m = 1, k = 5)
  } else {
    y ~ grp40 + s(lc, k = 8) + s(lc, by = grp40, k = 8) +
      s(lc, population, bs = "fs", m = 1, k = 5)
  }
  m <- bam(f, data = d, method = "fREML", discrete = TRUE)

  grid <- tibble(lc = seq(min(d$lc), max(d$lc), length.out = 200))
  nd <- \(g) grid |> mutate(
    grp40 = ordered(g, levels = c(FALSE, TRUE)),
    march = ordered(TRUE, levels = c(FALSE, TRUE)),
    month = factor("March", levels = levels(d$month)),
    population = levels(d$population)[1]
  )
  excl <- "s(lc,population)"
  Xd <- predict(m, nd(TRUE),  type = "lpmatrix", exclude = excl) -
        predict(m, nd(FALSE), type = "lpmatrix", exclude = excl)

  b   <- coef(m); V <- vcov(m, unconditional = TRUE)
  est <- drop(Xd %*% b)
  se  <- sqrt(rowSums((Xd %*% V) * Xd))
  sims <- MASS::mvrnorm(N_SIM, mu = rep(0, length(b)), Sigma = V)
  crit <- unname(quantile(apply(abs((Xd %*% t(sims)) / se), 2, max), 0.95))

  s_tab <- summary(m)$s.table
  list(
    diff = grid |> mutate(
      concentration = 2^lc, diff = est, se = se,
      lo_pw = est - 1.96 * se, hi_pw = est + 1.96 * se,
      lo_sim = est - crit * se, hi_sim = est + crit * se, crit = crit
    ),
    summary = tibble(
      term = rownames(s_tab), edf = s_tab[, "edf"], F = s_tab[, "F"], p = s_tab[, "p-value"]
    ) |>
      bind_rows(tibble(term = "parametric: grp40",
                       p = summary(m)$p.table["grp40.L", "Pr(>|t|)"])) |>
      mutate(dev_expl = summary(m)$dev.expl, n = nrow(d))
  )
}

specs <- tibble(
  eps        = c(EPS_PRIMARY, EPS_PRIMARY, EPS_SENS),
  with_month = c(FALSE, TRUE, rep(FALSE, length(EPS_SENS))),
  model      = c("primary (eps = 0.05, no month)", "eps = 0.05 + month",
                 paste0("eps = ", EPS_SENS, ", no month"))
)

set.seed(5293)
res <- specs |>
  pmap(\(eps, with_month, model) {
    gam_dat |>
      group_by(drug) |>
      group_map(\(d, k) fit_diff(d, eps, with_month) |>
                  map(\(x) mutate(x, drug = k$drug, model = model, .before = 1))) |>
      transpose() |>
      map(bind_rows)
  }) |>
  transpose() |>
  map(bind_rows)

gam_diff <- res$diff
gam_smry <- res$summary

sig_regions <- gam_diff |>
  mutate(sig = lo_sim > 0 | hi_sim < 0) |>
  group_by(drug, model) |>
  summarise(
    max_abs_log2_fc = max(abs(diff)),
    peak_conc       = concentration[which.max(abs(diff))],
    peak_fold       = 2^diff[which.max(abs(diff))],
    sig_from        = if (any(sig)) min(concentration[sig]) else NA_real_,
    sig_to          = if (any(sig)) max(concentration[sig]) else NA_real_,
    sig_direction   = if (any(sig)) paste(unique(sign(diff[sig])), collapse = ",") else NA_character_,
    .groups = "drop"
  )

print(sig_regions |> mutate(across(where(is.double), \(x) signif(x, 3))), n = Inf)
print(gam_smry |> filter(str_detect(term, "grp40|march")) |>
        mutate(across(where(is.double), \(x) signif(x, 3))), n = Inf)


# =============================================================================
# 3. Plots
# =============================================================================

plot_diff <- function(dat) {
  ggplot(dat |> mutate(drug = factor(drug, levels = names(DRUG_LABELS), labels = DRUG_LABELS)),
         aes(x = concentration, y = diff)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_ribbon(aes(ymin = lo_sim, ymax = hi_sim), fill = "grey88") +
    geom_ribbon(aes(ymin = lo_pw, ymax = hi_pw), fill = "grey70") +
    geom_line(linewidth = 0.8) +
    scale_x_log10() +
    scale_y_continuous(sec.axis = sec_axis(~ 2^., name = "Fold difference, 40 / 35 evolved",
                                           breaks = c(0.5, 0.71, 1, 1.41, 2))) +
    labs(x = "Concentration (\u00b5g/mL)",
         y = expression(paste(Delta, " ", log[2], " FC vs ancestor (40 \u2212 35)"))) +
    theme_evo(11) +
    theme(plot.caption = element_text(size = 9, color = "grey40"))
}

plot_diff(gam_diff |> filter(model == "primary (eps = 0.05, no month)")) +
  facet_wrap(~ drug, scales = "free_x") +
  labs(caption = paste(
    "GAM difference smooth of log2((rel. growth + 0.05) / (ancestor rel. growth + 0.05)), same plate;",
    "strain-level random curves. Dark: 95% pointwise CI; light: 95% simultaneous CI.",
    sep = "\n"
  ))

ggsave(file.path(FIGS, "gam-difference-curves-30.png"),
       width = 10, height = 4, dpi = 300, bg = "transparent")

plot_diff(gam_diff) +
  facet_grid(model ~ drug, scales = "free_x") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0))

ggsave(file.path(FIGS, "gam-difference-curves-sensitivity-30.png"),
       width = 12, height = 9, dpi = 300, bg = "transparent")


# =============================================================================
# 4. Export
# =============================================================================

write_csv(gam_diff,    file.path(OUT, "gam-difference-curves-30.csv"))
write_csv(gam_smry,    file.path(OUT, "gam-summaries-30.csv"))
write_csv(sig_regions, file.path(OUT, "gam-sig-regions-30.csv"))
