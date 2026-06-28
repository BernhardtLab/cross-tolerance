# =============================================================================
# 15-growth-rates.R
#
# Author: Joey Bernhardt
# Created: June 2026
#
# Estimates growth metrics from OD curves using the growthcurver package,
# then fits Sharpe-Schoolfield High (SSH) thermal performance curves per strain
# using the intrinsic growth rate (r) and maximum absolute growth rate
# (mu = r × K / 4) as performance metrics. Carrying capacity (K) is summarised
# and plotted without TPC fitting.
#
# Input:  data-processed/all-blocks-tpc-experiment.csv
# Output: data-processed/growthcurver/
#         figures/
#
# Sections:
#   1.  Setup
#   2.  SSH model functions
#   3.  Load data
#   4.  Fit growthcurver per well
#   5.  Fit SSH TPC per strain — r
#   6.  Fit SSH TPC per strain — mu (r × K / 4)
#   7.  Carrying capacity (K) — summary and plot
#   8.  Statistical tests (r and mu)
#   9.  Plots
#   10. Export
# =============================================================================


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)
library(growthcurver)
library(minpack.lm)
library(ggsignif)
library(cowplot)
theme_set(theme_cowplot())

DATA_PATH <- "data-processed/all-blocks-tpc-experiment.csv"
OUT       <- "data-processed/growthcurver"
FIGS      <- "figures"


TARGET_EVO <- c("35 evolved", "40 evolved", "fRS585")

EVO_COLORS <- c(
  "40 evolved" = "#FA3208",
  "35 evolved" = "#0E63FF",
  "fRS585"     = "#000000"
)

K_B    <- 8.617333e-5   # Boltzmann constant (eV K⁻¹)
TREF_K <- 288.15        # Reference temperature: 15°C in Kelvin

N_STARTS <- 500
TMAX_CAP <- 50.0        # Fallback Tmax if uniroot fails

T_PRED <- seq(15, 50, length.out = 500)

set.seed(7421)


# =============================================================================
# 2. SSH model functions
# =============================================================================

# Sharpe-Schoolfield High model:
#
#   perf(T) = r_tref * exp((e/kB)  * (1/Tref - 1/T))
#                    / (1 + exp((eh/kB) * (1/th   - 1/T)))
#
# Parameters:
#   r_tref = performance at reference temperature Tref (15°C)
#   e      = activation energy (eV)
#   eh     = deactivation energy (eV)
#   th     = temperature of half-deactivation (K)

sharpeschoolhigh <- function(T_c, r_tref, e, eh, th) {
  T_k <- T_c + 273.15
  r_tref *
    exp((e  / K_B) * (1 / TREF_K - 1 / T_k)) /
    (1 + exp((eh / K_B) * (1 / th - 1 / T_k)))
}

# Extract Topt, Tmax, Rmax, B80 from fitted SSH parameters.
# Topt via Brent optimisation; Tmax via uniroot at 5% Rmax above Topt.

calc_tpc_traits <- function(r_tref, e, eh, th,
                             T_range = seq(0, 55, length.out = 10000)) {
  na_result <- c(topt = NA_real_, tmax = NA_real_, rmax = NA_real_, b80 = NA_real_)
  preds <- sharpeschoolhigh(T_range, r_tref, e, eh, th)
  valid <- is.finite(preds)
  if (!any(valid)) return(na_result)

  opt <- optim(
    par    = T_range[which.max(preds)],
    fn     = function(T) -sharpeschoolhigh(T, r_tref, e, eh, th),
    method = "Brent",
    lower  = min(T_range),
    upper  = max(T_range)
  )
  topt <- opt$par
  rmax <- -opt$value

  thresh_fn <- function(T) sharpeschoolhigh(T, r_tref, e, eh, th) - 0.05 * rmax
  tmax <- tryCatch(
    uniroot(thresh_fn, lower = topt, upper = max(T_range), tol = 1e-8)$root,
    error = function(e) TMAX_CAP
  )

  above_80 <- T_range[preds >= 0.8 * rmax]
  b80      <- if (length(above_80) >= 2) max(above_80) - min(above_80) else NA_real_

  c(topt = topt, tmax = tmax, rmax = rmax, b80 = b80)
}

# Multistart SSH fitter. Bounds passed explicitly so different metrics
# can use appropriately scaled r_tref bounds.

fit_ssh_multistart <- function(temps, rates, bounds_lo, bounds_hi,
                                start_lo, start_hi) {
  starts <- mapply(
    function(a, b, c, d) c(a, b, c, d),
    runif(N_STARTS, start_lo["r_tref"], start_hi["r_tref"]),
    runif(N_STARTS, start_lo["e"],      start_hi["e"]),
    runif(N_STARTS, start_lo["eh"],     start_hi["eh"]),
    runif(N_STARTS, start_lo["th"],     start_hi["th"]),
    SIMPLIFY = FALSE
  )

  best_popt <- NULL
  best_rss  <- Inf

  for (p0 in starts) {
    tryCatch({
      fit <- suppressWarnings(nlsLM(
        rates ~ sharpeschoolhigh(temps, r_tref, e, eh, th),
        start   = list(r_tref = p0[1], e = p0[2], eh = p0[3], th = p0[4]),
        lower   = bounds_lo,
        upper   = bounds_hi,
        control = nls.lm.control(maxiter = 200, maxfev = 5000)
      ))
      rss <- sum(residuals(fit)^2)
      if (rss < best_rss) { best_rss <- rss; best_popt <- coef(fit) }
    }, error = function(e) NULL)
  }

  if (is.null(best_popt)) return(NULL)

  ss_tot <- sum((rates - mean(rates))^2)
  r2     <- if (ss_tot > 0) 1 - best_rss / ss_tot else NA_real_
  traits <- calc_tpc_traits(best_popt[["r_tref"]], best_popt[["e"]],
                             best_popt[["eh"]],    best_popt[["th"]])

  list(params = best_popt, r2 = r2,
       topt  = traits[["topt"]],  tmax = traits[["tmax"]],
       rmax  = traits[["rmax"]],  b80  = traits[["b80"]])
}


# =============================================================================
# 3. Load data
# =============================================================================

cat("Loading data...\n")

od_data <- read_csv(DATA_PATH, show_col_types = FALSE) |>
  filter(evolution_history %in% TARGET_EVO)

cat(sprintf("  %d rows | %d strains | %d temperatures | %d blocks\n",
            nrow(od_data),
            n_distinct(od_data$strain),
            n_distinct(od_data$test_temperature),
            n_distinct(od_data$block)))


# =============================================================================
# 4. Fit growthcurver per well
# =============================================================================

cat("\nFitting growthcurver to each well...\n")

safe_val <- function(f, field) {
  if (is.null(f)) return(NA_real_)
  v <- f$vals[[field]]
  if (length(v) == 0) NA_real_ else as.double(v)
}

safe_note <- function(f) {
  if (is.null(f)) return("fit_failed")
  v <- f$vals$note
  if (length(v) == 0) NA_character_ else as.character(v)
}

gc_fits <- od_data |>
  group_by(well, block, test_temperature, strain, evolution_history) |>
  arrange(days, .by_group = TRUE) |>
  summarise(
    fit = list(
      tryCatch(SummarizeGrowth(days, od, bg_correct = "min"), error = function(e) NULL)
    ),
    .groups = "drop"
  ) |>
  mutate(
    k     = map_dbl(fit, safe_val, "k"),
    n0    = map_dbl(fit, safe_val, "n0"),
    r     = map_dbl(fit, safe_val, "r"),
    t_mid = map_dbl(fit, safe_val, "t_mid"),
    t_gen = map_dbl(fit, safe_val, "t_gen"),
    sigma = map_dbl(fit, safe_val, "sigma"),
    note  = map_chr(fit, safe_note),
    # Maximum absolute growth rate from logistic model: dN/dt at N = K/2
    mu    = r * k / 4
  ) |>
  select(-fit)

cat(sprintf("  Wells fitted:  %d\n", nrow(gc_fits)))
cat(sprintf("  Fit failures:  %d\n", sum(gc_fits$note == "fit_failed")))

cat("\nMean metrics by temperature:\n")
gc_fits |>
  group_by(test_temperature) |>
  summarise(
    n       = n(),
    mean_r  = round(mean(r,  na.rm = TRUE), 2),
    mean_k  = round(mean(k,  na.rm = TRUE), 3),
    mean_mu = round(mean(mu, na.rm = TRUE), 3),
    .groups = "drop"
  ) |>
  print()

# Note: mean r at 42°C can appear anomalously elevated (higher than 41°C) because
# growthcurver attributes near-complete growth inhibition to very low K rather than
# low r. mu = r*K/4 integrates both parameters: the sharp K drop at 42°C pulls mu
# down sharply even when r is artefactually high, giving a more biologically
# plausible thermal response.


# =============================================================================
# 5. Fit SSH TPC per strain — r
# =============================================================================

cat("\nFitting SSH TPCs for r...\n")

# r values at 42°C may be artefactually high in some strains (see note above);
# interpret r SSH thermal traits with caution and compare against mu results.

BOUNDS_R <- list(
  lo   = c(r_tref = 1e-6, e = 0.01, eh =  0.5, th = 303.0),
  hi   = c(r_tref = 50.0, e =  3.0, eh = 50.0, th = 335.0),
  s_lo = c(r_tref = 0.10, e =  0.1, eh =  0.5, th = 305.0),
  s_hi = c(r_tref = 15.0, e =  2.0, eh = 20.0, th = 325.0)
)

d_r <- gc_fits |>
  filter(note != "fit_failed", !is.na(r), is.finite(r)) |>
  select(strain, evolution_history, test_temperature, r)

strains_r <- d_r |>
  group_by(strain, evolution_history) |>
  summarise(n_temps = n_distinct(test_temperature), .groups = "drop") |>
  filter(n_temps >= 3)

cat(sprintf("  Strains with ≥3 temperatures: %d\n", nrow(strains_r)))

params_r_list <- vector("list", nrow(strains_r))
preds_r_list  <- vector("list", nrow(strains_r))

for (i in seq_len(nrow(strains_r))) {
  sid <- strains_r$strain[i]
  evo <- strains_r$evolution_history[i]
  d   <- d_r |> filter(strain == sid)

  res <- fit_ssh_multistart(
    d$test_temperature, d$r,
    BOUNDS_R$lo, BOUNDS_R$hi, BOUNDS_R$s_lo, BOUNDS_R$s_hi
  )

  if (is.null(res)) { cat(sprintf("  FAILED: %s\n", sid)); next }

  p <- res$params
  params_r_list[[i]] <- tibble(
    strain = sid, evolution_history = evo,
    r_tref = p["r_tref"], e = p["e"], eh = p["eh"], th = p["th"],
    topt = res$topt, tmax = res$tmax, rmax = res$rmax, b80 = res$b80,
    r2 = res$r2, n_obs = nrow(d)
  )
  preds_r_list[[i]] <- tibble(
    strain = sid, evolution_history = evo,
    temp = T_PRED,
    pred = sharpeschoolhigh(T_PRED, p["r_tref"], p["e"], p["eh"], p["th"])
  )
  if (i %% 10 == 0) cat(sprintf("  %d / %d\n", i, nrow(strains_r)))
}

tpc_params_r <- bind_rows(params_r_list)
tpc_preds_r  <- bind_rows(preds_r_list)

cat("\n=== Topt, Tmax, B80 by evolution history (r) ===\n")
tpc_params_r |>
  group_by(evolution_history) |>
  summarise(
    n         = n(),
    mean_topt = round(mean(topt, na.rm = TRUE), 2),
    se_topt   = round(sd(topt,   na.rm = TRUE) / sqrt(n()), 2),
    mean_tmax = round(mean(tmax, na.rm = TRUE), 2),
    se_tmax   = round(sd(tmax,   na.rm = TRUE) / sqrt(n()), 2),
    .groups   = "drop"
  ) |>
  print()
### noting here that some of the tmaxes for r are artificially high because the TPC fitting function can't find a place on the interval of 0-55C where the function drops below 5% of its max, so it falls back to 50C. We should remove these if we are going to use the TPC for r for anything.

# =============================================================================
# 6. Fit SSH TPC per strain — mu (r × K / 4)
# =============================================================================

cat("\nFitting SSH TPCs for mu...\n")

BOUNDS_MU <- list(
  lo   = c(r_tref = 1e-6, e = 0.01, eh =  0.5, th = 303.0),
  hi   = c(r_tref = 20.0, e =  3.0, eh = 50.0, th = 335.0),
  s_lo = c(r_tref = 0.01, e =  0.1, eh =  0.5, th = 305.0),
  s_hi = c(r_tref =  5.0, e =  2.0, eh = 20.0, th = 325.0)
)

d_mu <- gc_fits |>
  filter(note != "fit_failed", !is.na(mu), is.finite(mu)) |>
  select(strain, evolution_history, test_temperature, mu)

strains_mu <- d_mu |>
  group_by(strain, evolution_history) |>
  summarise(n_temps = n_distinct(test_temperature), .groups = "drop") |>
  filter(n_temps >= 3)

cat(sprintf("  Strains with ≥3 temperatures: %d\n", nrow(strains_mu)))

params_mu_list <- vector("list", nrow(strains_mu))
preds_mu_list  <- vector("list", nrow(strains_mu))

for (i in seq_len(nrow(strains_mu))) {
  sid <- strains_mu$strain[i]
  evo <- strains_mu$evolution_history[i]
  d   <- d_mu |> filter(strain == sid)

  res <- fit_ssh_multistart(
    d$test_temperature, d$mu,
    BOUNDS_MU$lo, BOUNDS_MU$hi, BOUNDS_MU$s_lo, BOUNDS_MU$s_hi
  )

  if (is.null(res)) { cat(sprintf("  FAILED: %s\n", sid)); next }

  p <- res$params
  params_mu_list[[i]] <- tibble(
    strain = sid, evolution_history = evo,
    r_tref = p["r_tref"], e = p["e"], eh = p["eh"], th = p["th"],
    topt = res$topt, tmax = res$tmax, rmax = res$rmax, b80 = res$b80,
    r2 = res$r2, n_obs = nrow(d)
  )
  preds_mu_list[[i]] <- tibble(
    strain = sid, evolution_history = evo,
    temp = T_PRED,
    pred = sharpeschoolhigh(T_PRED, p["r_tref"], p["e"], p["eh"], p["th"])
  )
  if (i %% 10 == 0) cat(sprintf("  %d / %d\n", i, nrow(strains_mu)))
}

tpc_params_mu <- bind_rows(params_mu_list)
tpc_preds_mu  <- bind_rows(preds_mu_list)

cat("\n=== Topt, Tmax, B80 by evolution history (mu) ===\n")
tpc_params_mu |>
  group_by(evolution_history) |>
  summarise(
    n         = n(),
    mean_topt = round(mean(topt, na.rm = TRUE), 2),
    se_topt   = round(sd(topt,   na.rm = TRUE) / sqrt(n()), 2),
    mean_tmax = round(mean(tmax, na.rm = TRUE), 2),
    se_tmax   = round(sd(tmax,   na.rm = TRUE) / sqrt(n()), 2),
    .groups   = "drop"
  ) |>
  print()


# =============================================================================
# 7. Carrying capacity (K) — summary and plot
# =============================================================================

obs_means_k <- gc_fits |>
  filter(note != "fit_failed", !is.na(k)) |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(k = mean(k, na.rm = TRUE), .groups = "drop")

mean_k <- obs_means_k |>
  group_by(evolution_history, test_temperature) |>
  summarise(k = mean(k, na.rm = TRUE), .groups = "drop")

p_k <- ggplot() +
  geom_line(
    data  = obs_means_k |> filter(evolution_history != "fRS585"),
    aes(x = test_temperature, y = k,
        group = strain, color = evolution_history),
    alpha = 0.25, linewidth = 0.5
  ) +
  geom_line(
    data  = mean_k |> filter(evolution_history != "fRS585"),
    aes(x = test_temperature, y = k, color = evolution_history),
    linewidth = 2.0
  ) +
  geom_line(
    data      = obs_means_k |> filter(evolution_history == "fRS585"),
    aes(x = test_temperature, y = k, color = evolution_history),
    linewidth = 1.5, linetype = "dashed"
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x     = "Temperature (°C)",
    y     = "Carrying capacity K (OD)",
    title = "Carrying capacity (K) across test temperatures"
  ) +
  theme(
    legend.position      = c(0.02, 0.05),
    legend.justification = c(0, 0),
    legend.background    = element_blank()
  )

ggsave(file.path(FIGS, "k-by-temperature.png"), p_k, width = 8, height = 5, dpi = 300)
cat("Saved: k-by-temperature.png\n")


# =============================================================================
# 8. Statistical tests — r and mu
# =============================================================================

# Helper: Welch two-sample or one-sample t-test + Wilcoxon, return tidy row.
# CIs for one-sample tests are expressed as differences from mu₀.

run_tests2 <- function(x, y = NULL, mu = NULL, label_x, label_y = "ancestor", trait) {
  if (!is.null(y)) {
    t_res <- t.test(x, y)
    w_res <- wilcox.test(x, y, exact = FALSE)
    ref   <- mean(y)
  } else {
    t_res <- t.test(x, mu = mu)
    w_res <- wilcox.test(x, mu = mu, exact = FALSE)
    ref   <- mu
  }
  tibble(
    trait      = trait,
    comparison = paste(label_x, "vs", label_y),
    mean_x     = mean(x),
    ref        = ref,
    diff       = mean(x) - ref,
    ci_lo      = if (!is.null(y)) t_res$conf.int[1] else t_res$conf.int[1] - mu,
    ci_hi      = if (!is.null(y)) t_res$conf.int[2] else t_res$conf.int[2] - mu,
    p_welch    = t_res$p.value,
    p_wilcox   = w_res$p.value
  )
}

p_stars <- function(p) dplyr::case_when(
  p < 0.001 ~ "***",
  p < 0.01  ~ "**",
  p < 0.05  ~ "*",
  TRUE      ~ "ns"
)

# Run all 3 comparisons (35 vs 40, 35 vs ancestor, 40 vs ancestor) for topt
# and tmax, with Holm correction applied within each trait.

run_trait_tests <- function(params_df) {
  g35      <- params_df |> filter(evolution_history == "35 evolved")
  g40      <- params_df |> filter(evolution_history == "40 evolved")
  anc_topt <- params_df |> filter(evolution_history == "fRS585") |> pull(topt) |> unname()
  anc_tmax <- params_df |> filter(evolution_history == "fRS585") |> pull(tmax) |> unname()

  bind_rows(
    run_tests2(g35$topt, g40$topt,      label_x = "35 evolved", label_y = "40 evolved", trait = "topt"),
    run_tests2(g35$topt, mu = anc_topt, label_x = "35 evolved",                         trait = "topt"),
    run_tests2(g40$topt, mu = anc_topt, label_x = "40 evolved",                         trait = "topt"),
    run_tests2(g35$tmax, g40$tmax,      label_x = "35 evolved", label_y = "40 evolved", trait = "tmax"),
    run_tests2(g35$tmax, mu = anc_tmax, label_x = "35 evolved",                         trait = "tmax"),
    run_tests2(g40$tmax, mu = anc_tmax, label_x = "40 evolved",                         trait = "tmax")
  ) |>
    group_by(trait) |>
    mutate(
      p_welch_holm  = p.adjust(p_welch,  method = "holm"),
      p_wilcox_holm = p.adjust(p_wilcox, method = "holm")
    ) |>
    ungroup()
}

# Shapiro-Wilk normality check on evolved groups

sw_check <- function(params_df, metric_label) {
  purrr::map_dfr(c("topt", "tmax"), function(tr) {
    purrr::map_dfr(c("35 evolved", "40 evolved"), function(grp) {
      vals <- params_df |> filter(evolution_history == grp) |> pull(tr)
      sw   <- shapiro.test(vals)
      tibble(metric = metric_label, trait = tr, group = grp,
             W = round(sw$statistic, 3), p_sw = round(sw$p.value, 4))
    })
  })
}

cat("\n=== Shapiro-Wilk normality tests ===\n")
bind_rows(sw_check(tpc_params_r, "r"), sw_check(tpc_params_mu, "mu")) |> print()

results_r  <- run_trait_tests(tpc_params_r)
results_mu <- run_trait_tests(tpc_params_mu)

cat("\n=== Statistical results — r ===\n")
results_r |>
  select(trait, comparison, diff, ci_lo, ci_hi, p_welch_holm, p_wilcox_holm) |>
  mutate(across(where(is.numeric), \(x) round(x, 4))) |>
  print()

cat("\n=== Statistical results — mu ===\n")
results_mu |>
  select(trait, comparison, diff, ci_lo, ci_hi, p_welch_holm, p_wilcox_holm) |>
  mutate(across(where(is.numeric), \(x) round(x, 4))) |>
  print()


# =============================================================================
# 9. Plots
# =============================================================================

# ── Helper: dotplot with significance annotations ────────────────────────────
# Matches the style of thermal-traits-dotplot-sig-auc-gcplyr.png from script 16.

make_traits_dotplot_sig <- function(params_df, results_df,
                                     trait_labels_map, plot_title = NULL) {
  anc_ref <- params_df |>
    filter(evolution_history == "fRS585") |>
    select(topt, tmax) |>
    pivot_longer(everything(), names_to = "trait", values_to = "anc_val") |>
    mutate(trait = factor(trait, levels = names(trait_labels_map)))

  plot_df <- params_df |>
    filter(evolution_history != "fRS585") |>
    select(strain, evolution_history, topt, tmax) |>
    pivot_longer(c(topt, tmax), names_to = "trait", values_to = "value") |>
    mutate(trait = factor(trait, levels = names(trait_labels_map)))

  gmeans <- plot_df |>
    summarise(mean_val = mean(value), se = sd(value) / sqrt(n()),
              .by = c(evolution_history, trait))

  y_range <- plot_df |>
    summarise(y_max = max(value), y_span = diff(range(value)), .by = trait)

  bracket_df <- results_df |>
    filter(comparison == "35 evolved vs 40 evolved") |>
    mutate(
      trait       = factor(trait, levels = names(trait_labels_map)),
      xmin        = "35 evolved",
      xmax        = "40 evolved",
      annotations = p_stars(p_welch_holm)
    ) |>
    left_join(y_range, by = "trait") |>
    mutate(y_position = y_max + y_span * 0.06)

  anc_star_df <- results_df |>
    filter(stringr::str_detect(comparison, "vs ancestor")) |>
    mutate(
      trait             = factor(trait, levels = names(trait_labels_map)),
      evolution_history = stringr::str_remove(comparison, " vs ancestor"),
      label             = p_stars(p_welch_holm)
    ) |>
    left_join(gmeans, by = c("trait", "evolution_history")) |>
    left_join(y_range, by = "trait") |>
    mutate(y_pos = mean_val + se + y_span * 0.04)

  ggplot(plot_df, aes(x = evolution_history, y = value, color = evolution_history)) +
    geom_hline(
      data = anc_ref, aes(yintercept = anc_val),
      linetype = "dashed", color = "#000000", linewidth = 0.6
    ) +
    geom_jitter(width = 0.12, size = 1.8, alpha = 0.6) +
    geom_pointrange(
      data = gmeans,
      aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
      size = 0.7, linewidth = 1.1
    ) +
    ggsignif::geom_signif(
      data       = bracket_df,
      aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
      manual     = TRUE, tip_length = 0.02, textsize = 4.5, color = "black"
    ) +
    geom_text(
      data     = anc_star_df,
      aes(x = evolution_history, y = y_pos, label = label),
      color    = "black", size = 4, fontface = "bold", nudge_x = 0.3
    ) +
    facet_wrap(
      ~ trait, scales = "free_y",
      labeller = as_labeller(trait_labels_map)
    ) +
    scale_color_manual(values = EVO_COLORS) +
    labs(
      x       = NULL,
      y       = "Temperature (\u00b0C)",
      title   = plot_title,
      caption = paste(
        "Points: individual strains  |  Large point \u00b1 bar: mean \u00b1 SE  |",
        "Dashed line: ancestor (fRS585)\n",
        "Brackets: Welch t-test (Holm-corrected)  |",
        "Stars to right of mean: vs. ancestor (one-sample t-test)"
      )
    ) +
    theme_bw(base_size = 13) +
    theme(
      legend.position = "none",
      strip.text      = element_text(size = 13),
      plot.caption    = element_text(size = 8, color = "grey40")
    )
}

# ── Helper: TPC curve plot ────────────────────────────────────────────────────

make_tpc_plot <- function(preds_df, obs_df, obs_col, y_label, title) {
  mean_curves <- preds_df |>
    group_by(evolution_history, temp) |>
    summarise(pred = mean(pred, na.rm = TRUE), .groups = "drop")

  ggplot() +
    geom_line(
      data = preds_df |> mutate(pred = ifelse(pred < 0, NA, pred)),
      aes(x = temp, y = pred, group = strain, color = evolution_history),
      alpha = 0.2, linewidth = 0.8
    ) +
    geom_line(
      data = mean_curves,
      aes(x = temp, y = pred, color = evolution_history),
      linewidth = 2.0
    ) +
    geom_point(
      data = obs_df,
      aes(x = test_temperature, y = .data[[obs_col]], color = evolution_history),
      size = 1.8, alpha = 0.6
    ) +
    scale_color_manual(values = EVO_COLORS, name = NULL) +
    coord_cartesian(xlim = c(23, 48)) +
    labs(x = "Temperature (\u00b0C)", y = y_label, title = title) +
    theme(
      legend.position      = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background    = element_blank()
    )
}

make_tpc_plot_faceted <- function(preds_df, obs_df, obs_col, y_label, title) {
  mean_curves <- preds_df |>
    group_by(evolution_history, temp) |>
    summarise(pred = mean(pred, na.rm = TRUE), .groups = "drop")

  ggplot() +
    geom_line(
      data = preds_df |> mutate(pred = ifelse(pred < 0, NA, pred)),
      aes(x = temp, y = pred, group = strain),
      color = "grey70", alpha = 0.6, linewidth = 0.5
    ) +
    geom_line(
      data = mean_curves,
      aes(x = temp, y = pred, color = evolution_history),
      linewidth = 2.0
    ) +
    geom_point(
      data = obs_df,
      aes(x = test_temperature, y = .data[[obs_col]], color = evolution_history),
      size = 1.8, alpha = 0.7
    ) +
    scale_color_manual(values = EVO_COLORS, guide = "none") +
    coord_cartesian(xlim = c(23, 48)) +
    facet_wrap(~ evolution_history) +
    labs(
      x        = "Temperature (\u00b0C)",
      y        = y_label,
      title    = title,
      subtitle = "Grey = individual strains  |  Bold = mean prediction across strains"
    ) +
    theme(
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", size = 12)
    )
}

trait_labels <- c(topt = "Topt (\u00b0C)", tmax = "Tmax (\u00b0C)")

# ── r: observed means per strain × temperature ───────────────────────────────

obs_means_r <- gc_fits |>
  filter(note != "fit_failed", !is.na(r)) |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(r = mean(r, na.rm = TRUE), .groups = "drop")

p_tpc_r <- make_tpc_plot(
  tpc_preds_r, obs_means_r, "r",
  y_label = "Intrinsic growth rate r (day\u207b\u00b9)",
  title   = "Thermal Performance Curves \u2014 r (intrinsic growth rate)"
)

p_tpc_r_facet <- make_tpc_plot_faceted(
  tpc_preds_r, obs_means_r, "r",
  y_label = "Intrinsic growth rate r (day\u207b\u00b9)",
  title   = "Thermal Performance Curves \u2014 r (intrinsic growth rate)"
)

p_traits_r_sig <- make_traits_dotplot_sig(
  tpc_params_r, results_r, trait_labels,
  plot_title = NULL
)

ggsave(file.path(FIGS, "tpc-r.png"),              p_tpc_r,         width = 9,  height = 6, dpi = 300)
ggsave(file.path(FIGS, "tpc-r-faceted.png"),      p_tpc_r_facet,   width = 12, height = 5, dpi = 300)
ggsave(file.path(FIGS, "thermal-traits-dotplot-sig-r.png"), p_traits_r_sig, width = 8, height = 5, dpi = 300)
cat("Saved: tpc-r.png, tpc-r-faceted.png, thermal-traits-dotplot-sig-r.png\n")

# ── mu: observed means per strain × temperature ──────────────────────────────

obs_means_mu <- gc_fits |>
  filter(note != "fit_failed", !is.na(mu)) |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(mu = mean(mu, na.rm = TRUE), .groups = "drop")

p_tpc_mu <- make_tpc_plot(
  tpc_preds_mu, obs_means_mu, "mu",
  y_label = "Max growth rate \u03bc = rK/4 (OD day\u207b\u00b9)",
  title   = "Thermal Performance Curves \u2014 \u03bc (max absolute growth rate)"
)

p_tpc_mu_facet <- make_tpc_plot_faceted(
  tpc_preds_mu, obs_means_mu, "mu",
  y_label = "Max growth rate \u03bc = rK/4 (OD day\u207b\u00b9)",
  title   = "Thermal Performance Curves \u2014 \u03bc (max absolute growth rate)"
)

p_traits_mu_sig <- make_traits_dotplot_sig(
  tpc_params_mu, results_mu, trait_labels,
  plot_title = NULL
)

ggsave(file.path(FIGS, "tpc-mu.png"),              p_tpc_mu,         width = 9,  height = 6, dpi = 300)
ggsave(file.path(FIGS, "tpc-mu-faceted.png"),      p_tpc_mu_facet,   width = 12, height = 5, dpi = 300)
ggsave(file.path(FIGS, "thermal-traits-dotplot-sig-mu.png"), p_traits_mu_sig, width = 8, height = 5, dpi = 300)
cat("Saved: tpc-mu.png, tpc-mu-faceted.png, thermal-traits-dotplot-sig-mu.png\n")


# ── Logistic fit diagnostic PDF ──────────────────────────────────────────────

logistic_pred <- function(t, k, n0, r) {
  k / (1 + ((k - n0) / n0) * exp(-r * t))
}

# Join raw OD with per-well fitted parameters and compute predicted curves.
# bg_correct = "min" in SummarizeGrowth fits the logistic to (od - min(od)),
# so od_pred is on a blank-corrected scale. Subtract per-well min from raw OD
# (od_corr) so both axes are on the same scale.
well_preds <- od_data |>
  left_join(
    gc_fits |>
      select(well, block, test_temperature, strain, evolution_history, k, n0, r, note),
    by = c("well", "block", "test_temperature", "strain", "evolution_history")
  ) |>
  filter(note != "fit_failed", !is.na(k), !is.na(n0), !is.na(r)) |>
  group_by(well, block, test_temperature, strain, evolution_history) |>
  mutate(
    od_corr = od - min(od, na.rm = TRUE),
    od_pred = logistic_pred(days, k, n0, r)
  ) |>
  ungroup()

# R² per well (computed on blank-corrected scale)
r2_per_well <- well_preds |>
  group_by(well, block, test_temperature, strain, evolution_history) |>
  summarise(
    r2 = 1 - sum((od_corr - od_pred)^2) / sum((od_corr - mean(od_corr))^2),
    .groups = "drop"
  )

# Build strip labels — include well ID to guarantee uniqueness across
# technical replicates (multiple wells per strain × block × temperature)
well_preds <- well_preds |>
  left_join(
    r2_per_well |> select(well, block, test_temperature, strain, evolution_history, r2),
    by = c("well", "block", "test_temperature", "strain", "evolution_history")
  ) |>
  mutate(
    well_id   = paste(strain, test_temperature, block, well, sep = "_"),
    strip_lab = sprintf("%s | %d\u00b0C B%d [%s]\nR\u00b2=%.3f",
                        strain, test_temperature, block, well, r2)
  )

# Order wells: evolution history > strain > temperature > block > well
well_order <- well_preds |>
  distinct(well_id, strip_lab, evolution_history, strain, test_temperature, block, well) |>
  arrange(evolution_history, strain, test_temperature, block, well)

well_preds <- well_preds |>
  mutate(strip_lab = factor(strip_lab, levels = unique(well_order$strip_lab)))

# Paginate at 6 columns × 5 rows = 30 wells per page
N_COLS     <- 6
N_ROWS     <- 5
N_PER_PAGE <- N_COLS * N_ROWS
all_labs   <- levels(well_preds$strip_lab)
n_pages    <- ceiling(length(all_labs) / N_PER_PAGE)

pdf(file.path(FIGS, "logistic-fits-per-well.pdf"), width = 16, height = 12)
for (pg in seq_len(n_pages)) {
  idx  <- seq((pg - 1) * N_PER_PAGE + 1, min(pg * N_PER_PAGE, length(all_labs)))
  labs <- all_labs[idx]

  df_pg <- well_preds |>
    filter(strip_lab %in% labs) |>
    mutate(strip_lab = factor(strip_lab, levels = labs))

  p_pg <- ggplot(df_pg, aes(x = days)) +
    geom_point(aes(y = od_corr), size = 0.4, alpha = 0.35, color = "grey40") +
    geom_line(
      aes(y = od_pred, color = evolution_history, group = well_id),
      linewidth = 0.7
    ) +
    facet_wrap(~ strip_lab, ncol = N_COLS, scales = "free_y") +
    scale_color_manual(values = EVO_COLORS, name = NULL) +
    labs(x = "Time (days)", y = "OD600 (blank-corrected)") +
    theme(
      strip.background = element_blank(),
      strip.text       = element_text(size = 6),
      axis.text        = element_text(size = 5),
      axis.title       = element_text(size = 9),
      legend.position  = "bottom"
    )

  print(p_pg)
}
dev.off()
cat(sprintf("Saved: logistic-fits-per-well.pdf (%d pages)\n", n_pages))


# =============================================================================
# 10. Export
# =============================================================================

write_csv(gc_fits,       file.path(OUT, "growthcurver-metrics-per-well.csv"))
write_csv(tpc_params_r,  file.path(OUT, "tpc-params-r.csv"))
write_csv(tpc_preds_r,   file.path(OUT, "tpc-predictions-r.csv"))
write_csv(tpc_params_mu, file.path(OUT, "tpc-params-mu.csv"))
write_csv(tpc_preds_mu,  file.path(OUT, "tpc-predictions-mu.csv"))
write_csv(results_r,     file.path(OUT, "stats-results-r.csv"))
write_csv(results_mu,    file.path(OUT, "stats-results-mu.csv"))

cat("\nAll outputs saved to", OUT, "\n")
cat("Files:\n")
for (f in sort(list.files(OUT, recursive = FALSE))) cat(sprintf("  %s\n", f))
cat("\nDone.\n")
