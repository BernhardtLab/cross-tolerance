# =============================================================================
# 24-flextpc-pca.R
# Separate analysis: flexTPC fits to per-well AUC, then within-group centred
# PCA on flexTPC thermal traits (Topt, Tmax, beta) + IC50 log-ratios.
#
# flexTPC (Cruz-Loya, Mordecai & Savage) via rTPC::flextpc_2024 (rTPC >= 1.1.0),
# fitted per strain with the standard rTPC / nls.multstart workflow.
#
# Trait definitions:
#   tmax  = flexTPC Tmax parameter (temperature where the fitted rate reaches 0)
#   topt  = tmin + alpha * (tmax - tmin)  (exact optimum of flexTPC)
#   beta  = flexTPC breadth parameter
# Note: tmax here is NOT the same quantity as the SSH CTmax in scripts 17/19
# (5% of Rmax), and tmin is extrapolated (coldest test temperature is 25 C).
#
# Inputs:
#   data-processed/gcplyr/gcplyr-metrics-per-well-17.csv   (per-well AUC, Script 17)
#   data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv     (SSH fits, for comparison)
#   data-processed/gcplyr/tpc-predictions-auc-gcplyr-17.csv
#   data-processed/normalised-ic50-per-strain.csv          (Script 11d)
#
# Outputs — data-processed/flextpc/:
#   flextpc-params-24.csv           per-strain parameters, Topt, fit stats, bound flags
#   flextpc-predictions-24.csv      fitted curves on a temperature grid
#   flextpc-vs-ssh-fit-24.csv       per-strain RSS / AICc for flexTPC vs SSH
#   flextpc-vs-ssh-traits-24.csv    correlations between flexTPC and SSH traits
#   pca-scores-flextpc-24.csv
#   pca-loadings-flextpc-24.csv
#   pca-pct-flextpc-24.csv          % variance and eigenvalues
#
# Outputs — figures/:
#   flextpc-fits-per-strain-24.png
#   flextpc-vs-ssh-traits-24.png
#   pca-biplot-flextpc-24.png
# =============================================================================


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)
library(rTPC)
library(nls.multstart)

stopifnot(packageVersion("rTPC") >= "1.1.0")

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")
EVO_LEVELS <- c("35 evolved", "40 evolved")
FIGS       <- "figures"
OUT        <- "data-processed"
OUT_FLEX   <- file.path(OUT, "flextpc")
dir.create(OUT_FLEX, showWarnings = FALSE)

PCA_VARS <- c("topt", "tmax", "beta", "fluconazole", "caspofungin", "amphotericin")

# SSH model, identical to Script 17, for computing SSH residuals
K_B    <- 8.617e-5
TREF_K <- 288.15
sharpeschoolhigh <- function(T_c, r_tref, e, eh, th) {
  T_k <- T_c + 273.15
  r_tref *
    exp((e / K_B) * (1 / TREF_K - 1 / T_k)) /
    (1 + exp((eh / K_B) * (1 / th - 1 / T_k)))
}

theme_evo <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(
      panel.background  = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA),
      plot.background   = element_rect(fill = "transparent", colour = NA)
    )
}

set.seed(5817)


# =============================================================================
# 2. Load data
# =============================================================================

wells <- read_csv(file.path(OUT, "gcplyr/gcplyr-metrics-per-well-17.csv"),
                  show_col_types = FALSE) |>
  select(strain, evolution_history, block, well, temp = test_temperature, rate = auc_gc)

ssh_params <- read_csv(file.path(OUT, "gcplyr/tpc-params-auc-gcplyr-17.csv"),
                       show_col_types = FALSE)
ssh_preds  <- read_csv(file.path(OUT, "gcplyr/tpc-predictions-auc-gcplyr-17.csv"),
                       show_col_types = FALSE)

ic50_strain <- read_csv(file.path(OUT, "normalised-ic50-per-strain.csv"),
                        show_col_types = FALSE)

cat("Strains:", n_distinct(wells$strain), " Temperatures:",
    paste(sort(unique(wells$temp)), collapse = ", "), "\n")


# =============================================================================
# 3. Fit flexTPC per strain (standard rTPC workflow)
# =============================================================================

# Bounds: rTPC defaults, except
#   tmin in [0, min test temp]  — tmin cannot be below 0 C, and must be below
#                                 the coldest temperature where growth is > 0
#   tmax >= max test temp       — AUC is > 0 at 42 C in every well
# Start ranges are sampled within these bounds (flexTPC needs many starts).
fit_flextpc <- function(d) {
  lower <- get_lower_lims(d$temp, d$rate, model_name = "flextpc_2024")
  upper <- get_upper_lims(d$temp, d$rate, model_name = "flextpc_2024")
  lower["tmin"] <- 0
  upper["tmin"] <- min(d$temp)
  lower["tmax"] <- max(d$temp)

  start_lower <- c(tmin = 0,  tmax = max(d$temp),     rmax = 0.5 * max(d$rate), alpha = 0.5,  beta = 0.05)
  start_upper <- c(tmin = min(d$temp), tmax = max(d$temp) + 8, rmax = 1.5 * max(d$rate), alpha = 0.99, beta = 1)

  fit <- nls_multstart(
    rate ~ flextpc_2024(temp = temp, tmin, tmax, rmax, alpha, beta),
    data              = d,
    iter              = 1000,
    start_lower       = start_lower,
    start_upper       = start_upper,
    lower             = lower,
    upper             = upper,
    supp_errors       = "Y",
    convergence_count = FALSE
  )
  list(fit = fit, lower = lower, upper = upper)
}

fits <- wells |>
  nest(data = c(block, well, temp, rate)) |>
  mutate(res = map(data, fit_flextpc, .progress = TRUE))

n_failed <- sum(map_lgl(fits$res, \(r) is.null(r$fit)))
cat("flexTPC fits failed:", n_failed, "of", nrow(fits), "\n")

# Flag parameters within 0.1% of the bound range of a bound
at_bound <- function(est, lower, upper) {
  tol <- 0.001 * (upper - lower)
  names(est)[abs(est - lower[names(est)]) < tol | abs(est - upper[names(est)]) < tol]
}

flex_params <- fits |>
  mutate(
    est    = map(res, \(r) coef(r$fit)),
    rss    = map2_dbl(res, data, \(r, d) sum((d$rate - fitted(r$fit))^2)),
    tss    = map_dbl(data, \(d) sum((d$rate - mean(d$rate))^2)),
    n_obs  = map_int(data, nrow),
    bounds = map_chr(res, \(r) paste(at_bound(coef(r$fit), r$lower, r$upper), collapse = ";"))
  ) |>
  unnest_wider(est) |>
  mutate(
    topt = tmin + alpha * (tmax - tmin),
    r2   = 1 - rss / tss
  ) |>
  select(strain, evolution_history, tmin, tmax, rmax, alpha, beta, topt,
         rss, r2, n_obs, at_bound = bounds)

# Sanity check of the analytical Topt against rTPC's numerical estimate
topt_check <- map_dbl(fits$res, \(r) get_topt(r$fit))
cat("Max |analytic Topt - rTPC::get_topt|:",
    signif(max(abs(flex_params$topt - topt_check)), 3), "C\n")

cat("\nParameters at a bound:\n")
print(count(flex_params, at_bound))

write_csv(flex_params, file.path(OUT_FLEX, "flextpc-params-24.csv"))


# =============================================================================
# 4. Predictions
# =============================================================================

T_PRED <- seq(15, 50, length.out = 500)

flextpc_safe <- function(temp, tmin, tmax, rmax, alpha, beta) {
  out <- flextpc_2024(temp, tmin, tmax, rmax, alpha, beta)
  out[temp <= tmin | temp >= tmax] <- 0
  out
}

flex_preds <- flex_params |>
  select(strain, evolution_history, tmin, tmax, rmax, alpha, beta) |>
  mutate(temp = list(T_PRED)) |>
  unnest(temp) |>
  mutate(pred = flextpc_safe(temp, tmin, tmax, rmax, alpha, beta)) |>
  select(strain, evolution_history, temp, pred)

write_csv(flex_preds, file.path(OUT_FLEX, "flextpc-predictions-24.csv"))


# =============================================================================
# 5. Compare with SSH (Script 17)
# =============================================================================

# 5a. Goodness of fit: AICc with Gaussian errors, k = parameters + 1 (sigma)
aicc <- function(rss, n, k_par) {
  k <- k_par + 1
  n * log(rss / n) + 2 * k + (2 * k * (k + 1)) / (n - k - 1)
}

ssh_rss <- wells |>
  inner_join(ssh_params |> select(strain, r_tref, e, eh, th), by = "strain") |>
  mutate(pred = sharpeschoolhigh(temp, r_tref, e, eh, th)) |>
  group_by(strain) |>
  summarise(rss_ssh = sum((rate - pred)^2), .groups = "drop")

fit_compare <- flex_params |>
  select(strain, evolution_history, n_obs, rss_flex = rss) |>
  inner_join(ssh_rss, by = "strain") |>
  mutate(
    aicc_flex  = aicc(rss_flex, n_obs, 5),
    aicc_ssh   = aicc(rss_ssh, n_obs, 4),
    delta_aicc = aicc_flex - aicc_ssh,
    better     = case_when(
      delta_aicc <= -2 ~ "flexTPC",
      delta_aicc >=  2 ~ "SSH",
      TRUE             ~ "similar (|dAICc| < 2)"
    )
  )

cat("\n=== flexTPC vs SSH fit (AICc) ===\n")
print(count(fit_compare, better))
cat("Median dAICc (flex - SSH):", round(median(fit_compare$delta_aicc), 2), "\n")

write_csv(fit_compare, file.path(OUT_FLEX, "flextpc-vs-ssh-fit-24.csv"))

# 5b. Trait agreement (evolved strains + ancestor)
trait_compare <- flex_params |>
  select(strain, evolution_history, topt_flex = topt, tmax_flex = tmax, beta_flex = beta) |>
  inner_join(
    ssh_params |> select(strain, topt_ssh = topt, tmax_ssh = tmax, b80_ssh = b80, th_ssh = th_c),
    by = "strain"
  )

trait_pairs <- tribble(
  ~flex,       ~ssh,       ~label,
  "topt_flex", "topt_ssh", "Topt (flexTPC) vs Topt (SSH)",
  "tmax_flex", "tmax_ssh", "Tmax (flexTPC, rate = 0) vs CTmax (SSH, 5% Rmax)",
  "tmax_flex", "th_ssh",   "Tmax (flexTPC) vs Th (SSH)",
  "beta_flex", "b80_ssh",  "beta (flexTPC) vs B80 (SSH)"
)

trait_cor <- trait_pairs |>
  mutate(
    n        = nrow(trait_compare),
    pearson  = map2_dbl(flex, ssh, \(x, y) cor(trait_compare[[x]], trait_compare[[y]])),
    spearman = map2_dbl(flex, ssh, \(x, y) cor(trait_compare[[x]], trait_compare[[y]], method = "spearman")),
    mean_diff = map2_dbl(flex, ssh, \(x, y) mean(trait_compare[[x]] - trait_compare[[y]]))
  )

cat("\n=== flexTPC vs SSH trait agreement (all strains incl. ancestor) ===\n")
print(trait_cor |> mutate(across(where(is.double), \(x) round(x, 3))))

write_csv(trait_cor, file.path(OUT_FLEX, "flextpc-vs-ssh-traits-24.csv"))


# =============================================================================
# 6. Figures: fits and trait comparison
# =============================================================================

curve_overlay <- bind_rows(
  flex_preds |> mutate(model = "flexTPC"),
  ssh_preds  |> mutate(model = "SSH")
) |>
  filter(temp >= 20, temp <= 46)

ggplot(wells, aes(temp, rate)) +
  geom_point(aes(color = evolution_history), size = 1, alpha = 0.7) +
  geom_line(data = curve_overlay, aes(y = pred, linetype = model)) +
  facet_wrap(~ strain, ncol = 7) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_linetype_manual(values = c(flexTPC = "solid", SSH = "dashed"), name = NULL) +
  labs(x = "Temperature (°C)", y = "Growth performance (OD·day)") +
  theme_evo(base_size = 9) +
  theme(legend.position = "bottom")

ggsave(file.path(FIGS, "flextpc-fits-per-strain-24.png"),
       width = 11, height = 10, dpi = 300, bg = "white")

trait_long <- trait_pairs |>
  mutate(data = map2(flex, ssh, \(x, y) tibble(
    strain = trait_compare$strain,
    evolution_history = trait_compare$evolution_history,
    flex_val = trait_compare[[x]],
    ssh_val  = trait_compare[[y]]
  ))) |>
  select(label, data) |>
  unnest(data)

ggplot(trait_long, aes(ssh_val, flex_val, color = evolution_history)) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~ label, scales = "free", ncol = 2) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(x = "SSH trait (Script 17)", y = "flexTPC trait") +
  theme_evo(base_size = 10) +
  theme(legend.position = "bottom")

ggsave(file.path(FIGS, "flextpc-vs-ssh-traits-24.png"),
       width = 8, height = 7, dpi = 300, bg = "white")


# =============================================================================
# 7. Within-group centred PCA (same approach as Script 22)
# =============================================================================

ic50_wide <- ic50_strain |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(population, drug, log_ratio) |>
  pivot_wider(names_from = drug, values_from = log_ratio)

pca_input <- flex_params |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(strain, evolution_history, topt, tmax, beta) |>
  inner_join(ic50_wide, by = c("strain" = "population")) |>
  drop_na()

cat(sprintf("\nPCA input: %d strains x %d variables\n", nrow(pca_input), length(PCA_VARS)))

wide_centered <- pca_input |>
  group_by(evolution_history) |>
  mutate(across(all_of(PCA_VARS), \(x) x - mean(x))) |>
  ungroup()

cat("\n=== Within-group correlations among PCA inputs ===\n")
print(round(cor(wide_centered[, PCA_VARS]), 2))

pca_f <- prcomp(wide_centered[, PCA_VARS], center = FALSE, scale. = TRUE)

pct_f <- tibble(
  PC         = colnames(pca_f$x),
  eigenvalue = pca_f$sdev^2,
  pct        = 100 * pca_f$sdev^2 / sum(pca_f$sdev^2),
  cum_pct    = cumsum(pct)
)

cat("\n=== flexTPC PCA: eigenvalues and variance explained ===\n")
print(pct_f |> mutate(across(where(is.double), \(x) round(x, 3))))

cat("\n=== flexTPC PCA: loadings (PC1-PC3) ===\n")
print(round(pca_f$rotation[, 1:3], 3))

scores_f <- as.data.frame(pca_f$x) |>
  bind_cols(pca_input |> select(strain, evolution_history)) |>
  mutate(evolution_history = factor(evolution_history, levels = EVO_LEVELS))

loadings_f <- as.data.frame(pca_f$rotation[, 1:3]) |>
  rownames_to_column("variable") |>
  mutate(
    PC1_scaled = PC1 * max(abs(scores_f$PC1)),
    PC2_scaled = PC2 * max(abs(scores_f$PC2))
  )

ggplot(scores_f, aes(x = PC1, y = PC2, color = evolution_history)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_segment(
    data        = loadings_f,
    aes(x = 0, y = 0, xend = PC1_scaled, yend = PC2_scaled),
    inherit.aes = FALSE,
    arrow       = arrow(length = unit(0.25, "cm")),
    color = "grey30", linewidth = 0.6
  ) +
  geom_text(
    data        = loadings_f,
    aes(x = PC1_scaled * 1.12, y = PC2_scaled * 1.12, label = variable),
    inherit.aes = FALSE,
    size = 3.5, color = "grey20"
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x       = sprintf("PC1 (%.1f%%)", pct_f$pct[1]),
    y       = sprintf("PC2 (%.1f%%)", pct_f$pct[2]),
    caption = paste(
      "flexTPC traits. Variables group\u2013mean centered within each",
      "evolution history before PCA."
    )
  ) +
  theme_evo() +
  theme(
    legend.position = "bottom",
    plot.caption    = element_text(size = 9, color = "grey40")
  )

ggsave(file.path(FIGS, "pca-biplot-flextpc-24.png"),
       width = 5.5, height = 5, dpi = 300, bg = "transparent")


# =============================================================================
# 8. Export PCA
# =============================================================================

write_csv(scores_f |> select(strain, evolution_history, PC1, PC2, PC3),
          file.path(OUT_FLEX, "pca-scores-flextpc-24.csv"))
write_csv(loadings_f |> select(variable, PC1, PC2, PC3),
          file.path(OUT_FLEX, "pca-loadings-flextpc-24.csv"))
write_csv(pct_f, file.path(OUT_FLEX, "pca-pct-flextpc-24.csv"))

cat("\nAll outputs written.\n")
