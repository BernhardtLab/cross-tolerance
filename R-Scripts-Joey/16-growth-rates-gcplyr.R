# =============================================================================
# 16-growth-rates-gcplyr.R
#
# Author: Joey Bernhardt
# Created: June 2026
#
# Estimates growth metrics from OD curves using the gcplyr package
# (model-free approach), then fits Sharpe-Schoolfield High (SSH) thermal
# performance curves per strain using:
#   - mu_max  (max per-capita growth rate) as the primary metric
#   - auc     (empirical AUC, trapezoid rule) for comparison
#
# Input:  data-processed/all-blocks-tpc-experiment.csv
# Output: data-processed/gcplyr/
#         figures/
#
# Sections:
#   1. Setup
#   2. SSH model functions
#   3. Load data
#   4. Estimate growth metrics per well with gcplyr
#   5. Fit SSH TPC per strain — AUC
#   6. Plots
#   7. Export
# =============================================================================


# od_data_raw <- read_csv("data-processed/all-blocks-tpc-experiment.csv")
# unique(od_data_raw$evolution_history)


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)
library(gcplyr)
library(minpack.lm)
library(cowplot)
theme_set(theme_cowplot())

DATA_PATH <- "data-processed/all-blocks-tpc-experiment.csv"
OUT       <- "data-processed/gcplyr"
FIGS      <- "figures"
dir.create(OUT, showWarnings = FALSE)

TARGET_EVO <- c("35 evolved", "40 evolved", "fRS585")

EVO_COLORS <- c(
  "40 evolved" = "#FA3208",
  "35 evolved" = "#0E63FF",
  "fRS585"     = "#000000"
)

# Sharpeschoolfield High model constants
K_B    <- 8.617333e-5   # Boltzmann constant (eV K⁻¹)
TREF_K <- 288.15        # Reference temperature: 15°C in Kelvin

# SSH fitting
N_STARTS  <- 500
TMAX_CAP <- 50.0

# gcplyr smoothing: moving-average window (number of time points)
# Data resolution is ~15 min; window of 5 = 75-min smoothing; I tried windows of 3, 5 and 11... 5 seems to do best, but I can do a formal check of this by comparing within strain CV and finding which smoothing window minimizes this.
SMOOTH_N <- 5

# TPC prediction range
T_PRED <- seq(15, 50, length.out = 500)

set.seed(7421)

# =============================================================================
# 2. SSH model functions
# =============================================================================

# Sharpe-Schoolfield High model
sharpeschoolhigh <- function(T_c, r_tref, e, eh, th) {
  T_k <- T_c + 273.15
  r_tref *
    exp((e  / K_B) * (1 / TREF_K - 1 / T_k)) /
    (1 + exp((eh / K_B) * (1 / th - 1 / T_k)))
}

# Extract Topt, Tmax, Rmax, B80 from SSH parameters
calc_tpc_traits <- function(r_tref, e, eh, th,
                             T_range = seq(0, 55, length.out = 10000)) {
  na_result <- c(topt = NA_real_, tmax = NA_real_, rmax = NA_real_, b80 = NA_real_)
  preds <- sharpeschoolhigh(T_range, r_tref, e, eh, th)
  valid <- is.finite(preds)
  if (!any(valid)) return(na_result)

  # Numerical optimisation for Topt 
  opt <- optim(
    par    = T_range[which.max(preds)],   # grid max as starting point
    fn     = function(T) -sharpeschoolhigh(T, r_tref, e, eh, th),
    method = "Brent",
    lower  = min(T_range),
    upper  = max(T_range)
  )
  topt <- opt$par
  rmax <- -opt$value

  # Tmax: temperature above Topt where SSH rate drops to 5% of rmax
  thresh_fn <- function(T) sharpeschoolhigh(T, r_tref, e, eh, th) - 0.05 * rmax
  tmax <- tryCatch({
    uniroot(thresh_fn, lower = topt, upper = max(T_range), tol = 1e-8)$root
  }, error = function(e) TMAX_CAP)

  above_80 <- T_range[preds >= 0.8 * rmax]
  b80      <- if (length(above_80) >= 2) max(above_80) - min(above_80) else NA_real_

  c(topt = topt, tmax = tmax, rmax = rmax, b80 = b80)
}

# Multistart SSH fitter — bounds tuned per metric (see calls below)
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
  traits <- calc_tpc_traits(best_popt["r_tref"], best_popt["e"],
                             best_popt["eh"],     best_popt["th"])

  list(params = best_popt, r2 = r2,
       topt  = traits["topt"],  tmax = traits["tmax"],
       rmax  = traits["rmax"],  b80   = traits["b80"])
}

# =============================================================================
# 3. Load data
# =============================================================================


od_data <- read_csv(DATA_PATH, show_col_types = FALSE) |>
  filter(evolution_history %in% TARGET_EVO)

# Instrument blank: minimum OD across all wells (media + noise floor)
BLANK_OD <- min(od_data$od, na.rm = TRUE)


# =============================================================================
# 4. Estimate growth metrics per well with gcplyr
# =============================================================================


gcplyr_metrics <- od_data |>
  group_by(well, block, test_temperature, strain, evolution_history) |>
  arrange(days, .by_group = TRUE) |>
  mutate(
    # Smooth OD with moving average
    od_smooth    = smooth_data(
      x = days, y = od,
      sm_method      = "moving-average",
      window_width_n = SMOOTH_N
    ),
    # Per-capita growth rate from log-transformed smoothed data
    percap_deriv = calc_deriv(
      x              = days,
      y              = od_smooth,
      percapita      = TRUE,
      blank          = BLANK_OD,
      window_width_n = SMOOTH_N,
      trans_y        = "log"
    )
  ) |>
  summarise(
    mu_max    = max(percap_deriv, na.rm = TRUE),
    doub_time = doubling_time(y = max(percap_deriv, na.rm = TRUE)),
    lag       = tryCatch(
      lag_time(x = days, y = od_smooth, deriv = percap_deriv, blank = BLANK_OD),
      error   = function(e) NA_real_,
      warning = function(w) suppressWarnings(
        lag_time(x = days, y = od_smooth, deriv = percap_deriv, blank = BLANK_OD)
      )
    ),
    auc_gc    = auc(x = days, y = od_smooth, blank = BLANK_OD),
    n_obs     = n(),
    .groups   = "drop"
  )


# =============================================================================
# 5. Fit SSH TPC per strain — AUC
# =============================================================================


BOUNDS_AUC <- list(
  lo   = c(r_tref = 1e-6, e = 0.01, eh =  0.5, th = 303.0),
  hi   = c(r_tref = 10.0, e =  3.0, eh = 50.0, th = 335.0),
  s_lo = c(r_tref = 0.01, e =  0.1, eh =  0.5, th = 305.0),
  s_hi = c(r_tref =  2.0, e =  2.0, eh = 20.0, th = 325.0)
)

d_auc <- gcplyr_metrics |>
  filter(!is.na(auc_gc), is.finite(auc_gc)) |>
  select(strain, evolution_history, test_temperature, auc_gc)

strains_auc <- d_auc |>
  group_by(strain, evolution_history) |>
  summarise(n_temps = n_distinct(test_temperature), .groups = "drop") |>
  filter(n_temps >= 3)

params_auc_list <- vector("list", nrow(strains_auc))
preds_auc_list  <- vector("list", nrow(strains_auc))

for (i in seq_len(nrow(strains_auc))) {
  sid <- strains_auc$strain[i]
  evo <- strains_auc$evolution_history[i]
  d   <- d_auc |> filter(strain == sid)

  res <- fit_ssh_multistart(
    d$test_temperature, d$auc_gc,
    BOUNDS_AUC$lo, BOUNDS_AUC$hi, BOUNDS_AUC$s_lo, BOUNDS_AUC$s_hi
  )

  if (is.null(res)) { cat(sprintf("  FAILED: %s\n", sid)); next }

  p <- res$params
  params_auc_list[[i]] <- tibble(
    strain = sid, evolution_history = evo,
    r_tref = p["r_tref"], e = p["e"], eh = p["eh"], th = p["th"],
    topt = res$topt, tmax = res$tmax, rmax = res$rmax, b80 = res$b80,
    r2 = res$r2, n_obs = nrow(d)
  )
  preds_auc_list[[i]] <- tibble(
    strain = sid, evolution_history = evo,
    temp = T_PRED,
    pred = sharpeschoolhigh(T_PRED, p["r_tref"], p["e"], p["eh"], p["th"])
  )
  if (i %% 10 == 0) cat(sprintf("  %d / %d\n", i, nrow(strains_auc)))
}

tpc_params_auc <- bind_rows(params_auc_list)
tpc_preds_auc  <- bind_rows(preds_auc_list)

cat("\n=== Topt, Tmax, B80 by evolution history (AUC) ===\n")
tpc_params_auc |>
  group_by(evolution_history) |>
  summarise(
    n          = n(),
    mean_topt  = round(mean(topt,  na.rm = TRUE), 2),
    se_topt    = round(sd(topt,    na.rm = TRUE) / sqrt(n()), 2),
    mean_tmax = round(mean(tmax, na.rm = TRUE), 2),
    se_tmax   = round(sd(tmax,   na.rm = TRUE) / sqrt(n()), 2),
    .groups    = "drop"
  ) |>
  print()

# =============================================================================
# 6. Plots
# =============================================================================

# ── TPC curves — AUC ─────────────────────────────────────────────────────────

mean_curves_auc <- tpc_preds_auc |>
  group_by(evolution_history, temp) |>
  summarise(pred = mean(pred, na.rm = TRUE), .groups = "drop")

obs_means_auc <- gcplyr_metrics |>
  filter(!is.na(auc_gc)) |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(auc_gc = mean(auc_gc, na.rm = TRUE), .groups = "drop")

p_tpc_auc <- ggplot() +
  geom_line(
    data = tpc_preds_auc |> mutate(pred = ifelse(pred < 0, NA, pred)),
    aes(x = temp, y = pred, group = strain, color = evolution_history),
    alpha = 0.2, linewidth = 0.8
  ) +
  geom_line(
    data = mean_curves_auc,
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 2.0
  ) +
  geom_point(
    data = obs_means_auc,
    aes(x = test_temperature, y = auc_gc, color = evolution_history),
    size = 1.8, alpha = 0.6
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  coord_cartesian(xlim = c(23, 48)) +
  labs(x = "Temperature (°C)", y = "AUC (empirical)",
       title = "Thermal Performance Curves — AUC (gcplyr)") +
  theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1),
        legend.background = element_blank())

ggsave(file.path(FIGS, "tpc-auc-gcplyr.png"), p_tpc_auc, width = 9, height = 6, dpi = 300)


# ── TPC curves — AUC, faceted by evolution history ───────────────────────────

p_tpc_auc_facet <- ggplot() +
  geom_line(
    data = tpc_preds_auc |> mutate(pred = ifelse(pred < 0, NA, pred)),
    aes(x = temp, y = pred, group = strain),
    color = "grey70", alpha = 0.6, linewidth = 0.5
  ) +
  geom_line(
    data = mean_curves_auc,
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 2.0
  ) +
  geom_point(
    data = obs_means_auc,
    aes(x = test_temperature, y = auc_gc, color = evolution_history),
    size = 1.8, alpha = 0.7
  ) +
  scale_color_manual(values = EVO_COLORS, guide = "none") +
  coord_cartesian(xlim = c(23, 48)) +
  facet_wrap(~ evolution_history) +
  labs(
    x = "Temperature (°C)", y = "AUC (empirical)",
    title = "Thermal Performance Curves — AUC (gcplyr)",
    subtitle = "Grey = individual strains  |  Bold = mean prediction across strains"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 12)
  )

ggsave(file.path(FIGS, "tpc-auc-gcplyr-faceted.png"), p_tpc_auc_facet,
       width = 12, height = 5, dpi = 300)


# ── Thermal traits — AUC ─────────────────────────────────────────────────────

p_traits_auc <- tpc_params_auc |>
  pivot_longer(c(topt, tmax, b80), names_to = "trait", values_to = "value") |>
  mutate(trait = factor(trait,
                        levels = c("topt", "tmax", "b80"),
                        labels = c("Topt (°C)", "Tmax (°C)", "B80 — niche breadth (°C)"))) |>
  ggplot(aes(x = evolution_history, y = value, color = evolution_history)) +
  geom_jitter(width = 0.12, size = 2.5, alpha = 0.8) +
  stat_summary(fun = mean, geom = "crossbar",
               width = 0.35, linewidth = 0.7, fatten = 1.5) +
  scale_color_manual(values = EVO_COLORS, guide = "none") +
  facet_wrap(~ trait, scales = "free_y") +
  labs(x = NULL, y = NULL,
       title = "Thermal traits by evolution history (AUC, gcplyr)",
       subtitle = "Crossbar = mean") +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 11))

ggsave(file.path(FIGS, "thermal-traits-auc-gcplyr.png"), p_traits_auc,
       width = 11, height = 5, dpi = 300)


# ── Thermal traits dot plot with ancestor reference line ─────────────────────

anc_topt <- tpc_params_auc |> filter(evolution_history == "fRS585") |> pull(topt)
anc_tmax <- tpc_params_auc |> filter(evolution_history == "fRS585") |> pull(tmax)

plot_data_dotplot <- tpc_params_auc |>
  filter(evolution_history != "fRS585") |>
  select(strain, evolution_history, topt, tmax) |>
  pivot_longer(c(topt, tmax), names_to = "trait", values_to = "value") |>
  mutate(trait = factor(trait, levels = c("topt", "tmax")))

anc_ref_dotplot <- tibble(
  trait   = factor(c("topt", "tmax"), levels = c("topt", "tmax")),
  anc_val = c(anc_topt, anc_tmax)
)

group_means_dotplot <- plot_data_dotplot |>
  summarise(mean_val = mean(value), se = sd(value) / sqrt(n()),
            .by = c(evolution_history, trait))

trait_labels_dotplot <- c(topt = "Topt (\u00b0C)", tmax = "Tmax (\u00b0C)")

p_traits_dotplot <- ggplot(plot_data_dotplot,
                           aes(x = evolution_history, y = value,
                               color = evolution_history)) +
  geom_hline(data = anc_ref_dotplot, aes(yintercept = anc_val),
             linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(width = 0.12, size = 1.8, alpha = 0.6) +
  geom_pointrange(
    data = group_means_dotplot,
    aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
    size = 0.7, linewidth = 1.1
  ) +
  facet_wrap(~ trait, scales = "free_y", labeller = as_labeller(trait_labels_dotplot)) +
  scale_color_manual(values = EVO_COLORS) +
  labs(x = NULL, y = "Temperature (\u00b0C)",
       caption = paste("Points: individual strains  |",
                       "Large point \u00b1 bar: mean \u00b1 SE  |",
                       "Dashed line: ancestor (fRS585)")) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        strip.text = element_text(size = 13),
        plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "thermal-traits-dotplot-auc-gcplyr.png"), p_traits_dotplot,
       width = 8, height = 5, dpi = 300)


# ── Statistical tests: evolved groups vs each other and vs ancestor ──────────

# Normality check (Shapiro-Wilk) on evolved groups
sw <- tibble(
  group  = rep(c("35 evolved", "40 evolved"), 2),
  trait  = c(rep("topt", 2), rep("tmax", 2)),
  W      = c(
    shapiro.test(g35$topt)$statistic, shapiro.test(g40$topt)$statistic,
    shapiro.test(g35$tmax)$statistic, shapiro.test(g40$tmax)$statistic
  ),
  p_sw   = c(
    shapiro.test(g35$topt)$p.value, shapiro.test(g40$topt)$p.value,
    shapiro.test(g35$tmax)$p.value, shapiro.test(g40$tmax)$p.value
  )
)

# Helper: run Welch two-sample or one-sample t-test + Wilcoxon, return tidy row
run_tests2 <- function(x, y = NULL, mu = NULL, label_x, label_y = "ancestor", trait) {
  if (!is.null(y)) {
    t_res <- t.test(x, y); w_res <- wilcox.test(x, y); ref <- mean(y)
  } else {
    t_res <- t.test(x, mu = mu); w_res <- wilcox.test(x, mu = mu); ref <- mu
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

g35 <- tpc_params_auc |> filter(evolution_history == "35 evolved")
g40 <- tpc_params_auc |> filter(evolution_history == "40 evolved")

results2 <- bind_rows(
  run_tests2(g35$topt, g40$topt,      label_x = "35 evolved", label_y = "40 evolved", trait = "topt"),
  run_tests2(g35$topt, mu = anc_topt, label_x = "35 evolved", trait = "topt"),
  run_tests2(g40$topt, mu = anc_topt, label_x = "40 evolved", trait = "topt"),
  run_tests2(g35$tmax, g40$tmax,      label_x = "35 evolved", label_y = "40 evolved", trait = "tmax"),
  run_tests2(g35$tmax, mu = anc_tmax, label_x = "35 evolved", trait = "tmax"),
  run_tests2(g40$tmax, mu = anc_tmax, label_x = "40 evolved", trait = "tmax")
) |>
  group_by(trait) |>
  mutate(
    p_welch_holm  = p.adjust(p_welch,  method = "holm"),
    p_wilcox_holm = p.adjust(p_wilcox, method = "holm")
  ) |>
  ungroup()


# ── Dot plot with significance annotations ───────────────────────────────────

p_stars <- function(p) dplyr::case_when(
  p < 0.001 ~ "***",
  p < 0.01  ~ "**",
  p < 0.05  ~ "*",
  TRUE      ~ "ns"
)

# y-range per trait for annotation placement
y_range <- plot_data_dotplot |>
  summarise(y_max = max(value), y_span = diff(range(value)), .by = trait)

# Between-group bracket (35 evolved vs 40 evolved)
bracket_df <- results2 |>
  filter(comparison == "35 evolved vs 40 evolved") |>
  mutate(
    trait       = factor(trait, levels = c("topt", "tmax")),
    xmin        = "35 evolved",
    xmax        = "40 evolved",
    annotations = p_stars(p_welch_holm)
  ) |>
  left_join(y_range, by = "trait") |>
  mutate(y_position = y_max + y_span * 0.06)

# vs-ancestor stars placed to the right of each group's mean ± SE bar
anc_star_df <- results2 |>
  filter(stringr::str_detect(comparison, "vs ancestor")) |>
  mutate(
    trait             = factor(trait, levels = c("topt", "tmax")),
    evolution_history = stringr::str_remove(comparison, " vs ancestor"),
    label             = p_stars(p_welch_holm)
  ) |>
  left_join(group_means_dotplot, by = c("trait", "evolution_history")) |>
  left_join(y_range, by = "trait") |>
  mutate(y_pos = mean_val + se + y_span * 0.04)

p_traits_dotplot_sig <- ggplot(plot_data_dotplot,
                               aes(x = evolution_history, y = value,
                                   color = evolution_history)) +
  geom_hline(data = anc_ref_dotplot, aes(yintercept = anc_val),
             linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(width = 0.12, size = 1.8, alpha = 0.6) +
  geom_pointrange(
    data = group_means_dotplot,
    aes(y = mean_val, ymin = mean_val - se, ymax = mean_val + se),
    size = 0.7, linewidth = 1.1
  ) +
  ggsignif::geom_signif(
    data       = bracket_df,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual     = TRUE,
    tip_length = 0.02,
    textsize   = 4.5,
    color      = "black"
  ) +
  geom_text(
    data     = anc_star_df,
    aes(x = evolution_history, y = y_pos, label = label),
    color    = "black",
    size     = 4,
    fontface = "bold",
    nudge_x  = 0.3
  ) +
  facet_wrap(~ trait, scales = "free_y", labeller = as_labeller(trait_labels_dotplot)) +
  scale_color_manual(values = EVO_COLORS) +
  labs(x = NULL, y = "Temperature (\u00b0C)",
       caption = paste(
         "Points: individual strains  |  Large point \u00b1 bar: mean \u00b1 SE  |",
         "Dashed line: ancestor (fRS585)\n",
         "Brackets: Welch t-test (Holm-corrected)  |  Stars to right of mean: vs. ancestor (one-sample t-test)"
       )) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        strip.text       = element_text(size = 13),
        plot.caption     = element_text(size = 8, color = "grey40"))

ggsave(file.path(FIGS, "thermal-traits-dotplot-sig-auc-gcplyr.png"), p_traits_dotplot_sig,
       width = 8, height = 5, dpi = 300)


# ── Per-strain TPCs with raw data, PDF ───────────────────────────────────────

# Strip labels include R² for quick fit assessment
strain_labels <- tpc_params_auc |>
  arrange(evolution_history, strain) |>
  mutate(label = sprintf("%s  (R²=%.2f)", strain, r2))

tpc_preds_labeled <- tpc_preds_auc |>
  left_join(select(strain_labels, strain, label), by = "strain") |>
  mutate(
    pred  = ifelse(pred < 0, NA, pred),
    label = factor(label, levels = strain_labels$label)
  )

data_labeled <- d_auc |>
  left_join(select(strain_labels, strain, label), by = "strain") |>
  mutate(label = factor(label, levels = strain_labels$label))

vlines <- strain_labels |>
  pivot_longer(c(topt, tmax), names_to = "trait", values_to = "temp_val") |>
  mutate(label = factor(label, levels = strain_labels$label))

p_per_strain <- ggplot() +
  geom_line(
    data = tpc_preds_labeled,
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 0.7
  ) +
  geom_point(
    data = data_labeled,
    aes(x = test_temperature, y = auc_gc, color = evolution_history),
    size = 1.5, alpha = 0.7
  ) +
  geom_vline(
    data = vlines,
    aes(xintercept = temp_val, linetype = trait),
    color = "grey40", linewidth = 0.4
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_linetype_manual(values = c(topt = "dashed", tmax = "dotted"),
                        labels = c(topt = "Topt", tmax = "Tmax")) +
  coord_cartesian(xlim = c(23, 45)) +
  facet_wrap(~ label, ncol = 6) +
  labs(x = "Temperature (°C)", y = "AUC", linetype = NULL) +
  theme(
    legend.position  = "bottom",
    strip.background = element_blank(),
    strip.text       = element_text(size = 7),
    axis.text        = element_text(size = 6)
  )

ggsave(file.path(FIGS, "tpc-per-strain-gcplyr.pdf"), p_per_strain,
       width = 16, height = 18)


# =============================================================================
# 7. Export
# =============================================================================

write_csv(gcplyr_metrics, file.path(OUT, "gcplyr-metrics-per-well.csv"))
write_csv(tpc_params_auc, file.path(OUT, "tpc-params-auc-gcplyr.csv"))
write_csv(tpc_preds_auc,  file.path(OUT, "tpc-predictions-auc-gcplyr.csv"))


