# =============================================================================
# 15-growth-rates.R
#
# Author: Joey Bernhardt
# Created: June 2026
#
# Estimates growth metrics from OD curves using the growthcurver package,
# then fits Sharpe-Schoolfield High (SSH) thermal performance curves per strain
# using empirical AUC (auc_e) as the performance metric.
#
# Input:  data-processed/all-blocks-tpc-experiment.csv
# Output: data-processed/growthcurver/
#
# Sections:
#   1. Setup
#   2. SSH model functions
#   3. Load data
#   4. Fit growthcurver per well
#   5. Fit SSH TPC per strain (auc_e)
#   6. Plots
#   7. Export
# =============================================================================

# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)
library(growthcurver)
library(minpack.lm)
library(cowplot)
theme_set(theme_cowplot())

DATA_PATH <- "data-processed/all-blocks-tpc-experiment.csv"
OUT       <- "data-processed/growthcurver"
FIGS      <- "figures"
dir.create(OUT, showWarnings = FALSE)

TARGET_EVO <- c("35 evolved", "40 evolved", "fRS585")

EVO_COLORS <- c(
  "40 evolved" = "#FA3208",
  "35 evolved" = "#0E63FF",
  "fRS585"     = "#000000"
)

# SSH model constants
K_B    <- 8.617333e-5   # Boltzmann constant (eV K⁻¹)
TREF_K <- 288.15        # Reference temperature: 15°C in Kelvin

# SSH fitting
N_STARTS  <- 500        # Random starts for multistart fitting
CTMAX_CAP <- 50.0       # Fallback CTmax if SSH doesn't cross 5% Rmax threshold

# TPC prediction range
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
#   e      = activation energy (eV); controls the rising left side
#   eh     = deactivation energy (eV); controls the falling right side
#   th     = temperature of half-deactivation (K)

sharpeschoolhigh <- function(T_c, r_tref, e, eh, th) {
  T_k <- T_c + 273.15
  r_tref *
    exp((e  / K_B) * (1 / TREF_K - 1 / T_k)) /
    (1 + exp((eh / K_B) * (1 / th - 1 / T_k)))
}

# Extract Topt, CTmax, and niche breadth (B80) from SSH parameters.
#
# Topt  = temperature at peak performance
# CTmax = temperature above Topt where performance drops to 5% of Rmax
#         (interpolated zero-crossing if curve reaches zero, otherwise 5% threshold)
# B80   = niche breadth: width of temperature range where performance >= 80% of Rmax

calc_tpc_traits <- function(r_tref, e, eh, th,
                             T_range = seq(0, 55, length.out = 10000)) {

  na_result <- c(topt = NA_real_, ctmax = NA_real_, rmax = NA_real_, b80 = NA_real_)

  preds <- sharpeschoolhigh(T_range, r_tref, e, eh, th)
  valid <- is.finite(preds)
  if (!any(valid)) return(na_result)

  # Topt and Rmax
  topt_idx <- which.max(preds[valid])
  topt     <- T_range[valid][topt_idx]
  rmax     <- preds[valid][topt_idx]

  # CTmax: interpolated crossing above Topt
  preds_right              <- preds
  preds_right[T_range <= topt] <- rmax
  sign_changes             <- which(diff(sign(preds_right)) != 0)

  if (length(sign_changes) > 0) {
    i     <- sign_changes[1]
    t1    <- T_range[i];       t2 <- T_range[i + 1]
    p1    <- preds_right[i];   p2 <- preds_right[i + 1]
    ctmax <- if (p2 != p1) t1 - p1 * (t2 - t1) / (p2 - p1) else t1
  } else {
    below <- which(T_range > topt & preds < 0.05 * rmax)
    ctmax <- if (length(below) > 0) T_range[below[1]] else CTMAX_CAP
  }

  # B80 (niche breadth): temperature range where performance >= 80% of Rmax
  above_80 <- T_range[preds >= 0.8 * rmax]
  b80      <- if (length(above_80) >= 2) max(above_80) - min(above_80) else NA_real_

  c(topt = topt, ctmax = ctmax, rmax = rmax, b80 = b80)
}

# Multistart SSH fitter: returns list(params, topt, ctmax, rmax, b80, r2)
# or NULL if all starts fail.

fit_ssh_multistart <- function(temps, rates) {

  BOUNDS_LO <- c(r_tref = 1e-6, e = 0.01, eh =  0.5, th = 303.0)
  BOUNDS_HI <- c(r_tref = 10.0, e =  3.0, eh = 50.0, th = 335.0)
  START_LO  <- c(r_tref = 0.01, e =  0.1, eh =  0.5, th = 305.0)
  START_HI  <- c(r_tref =  2.0, e =  2.0, eh = 20.0, th = 325.0)

  starts <- mapply(
    function(a, b, c, d) c(a, b, c, d),
    runif(N_STARTS, START_LO["r_tref"], START_HI["r_tref"]),
    runif(N_STARTS, START_LO["e"],      START_HI["e"]),
    runif(N_STARTS, START_LO["eh"],     START_HI["eh"]),
    runif(N_STARTS, START_LO["th"],     START_HI["th"]),
    SIMPLIFY = FALSE
  )

  best_popt <- NULL
  best_rss  <- Inf

  for (p0 in starts) {
    tryCatch({
      fit <- suppressWarnings(nlsLM(
        rates ~ sharpeschoolhigh(temps, r_tref, e, eh, th),
        start   = list(r_tref = p0[1], e = p0[2], eh = p0[3], th = p0[4]),
        lower   = BOUNDS_LO,
        upper   = BOUNDS_HI,
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
       topt  = traits["topt"],  ctmax = traits["ctmax"],
       rmax  = traits["rmax"],  b80   = traits["b80"])
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
      tryCatch(SummarizeGrowth(days, od), error = function(e) NULL)
    ),
    .groups = "drop"
  ) |>
  mutate(
    k     = map_dbl(fit, safe_val, "k"),
    n0    = map_dbl(fit, safe_val, "n0"),
    r     = map_dbl(fit, safe_val, "r"),
    t_mid = map_dbl(fit, safe_val, "t_mid"),
    t_gen = map_dbl(fit, safe_val, "t_gen"),
    auc_l = map_dbl(fit, safe_val, "auc_l"),
    auc_e = map_dbl(fit, safe_val, "auc_e"),
    sigma = map_dbl(fit, safe_val, "sigma"),
    note  = map_chr(fit, safe_note)
  ) |>
  select(-fit)

cat(sprintf("  Wells fitted:  %d\n", nrow(gc_fits)))
cat(sprintf("  Fit failures:  %d\n", sum(gc_fits$note == "fit_failed")))

cat("\nMean metrics by temperature:\n")
gc_fits |>
  group_by(test_temperature) |>
  summarise(
    n          = n(),
    mean_r     = round(mean(r,     na.rm = TRUE), 2),
    mean_k     = round(mean(k,     na.rm = TRUE), 3),
    mean_auc_l = round(mean(auc_l, na.rm = TRUE), 4),
    mean_auc_e = round(mean(auc_e, na.rm = TRUE), 4),
    mean_t_gen = round(mean(t_gen, na.rm = TRUE), 4),
    .groups    = "drop"
  ) |>
  print()

# =============================================================================
# 5. Fit SSH TPC per strain using auc_e
# =============================================================================

cat("\nFitting SSH TPCs using auc_e...\n")

d_auc <- gc_fits |>
  filter(note != "fit_failed", !is.na(auc_e)) |>
  select(strain, evolution_history, test_temperature, auc_e)

strains <- d_auc |>
  group_by(strain, evolution_history) |>
  summarise(n_temps = n_distinct(test_temperature), .groups = "drop") |>
  filter(n_temps >= 3)

cat(sprintf("  Strains with ≥3 temperatures: %d\n", nrow(strains)))

params_list <- vector("list", nrow(strains))
preds_list  <- vector("list", nrow(strains))

for (i in seq_len(nrow(strains))) {
  sid <- strains$strain[i]
  evo <- strains$evolution_history[i]
  d   <- d_auc |> filter(strain == sid)

  res <- fit_ssh_multistart(d$test_temperature, d$auc_e)

  if (is.null(res)) {
    cat(sprintf("  FAILED: %s\n", sid))
    next
  }

  p <- res$params
  params_list[[i]] <- tibble(
    strain            = sid,
    evolution_history = evo,
    r_tref = p["r_tref"], e = p["e"], eh = p["eh"], th = p["th"],
    topt  = res$topt,  ctmax = res$ctmax,
    rmax  = res$rmax,  b80   = res$b80,
    r2    = res$r2,    n_obs = nrow(d)
  )

  preds_list[[i]] <- tibble(
    strain            = sid,
    evolution_history = evo,
    temp = T_PRED,
    pred = sharpeschoolhigh(T_PRED, p["r_tref"], p["e"], p["eh"], p["th"])
  )

  if (i %% 10 == 0) cat(sprintf("  %d / %d\n", i, nrow(strains)))
}

tpc_params <- bind_rows(params_list)
tpc_preds  <- bind_rows(preds_list)

# Summary of TPC traits
cat("\n=== Topt, CTmax, B80 (niche breadth) by evolution history ===\n")
tpc_params |>
  group_by(evolution_history) |>
  summarise(
    n          = n(),
    mean_topt  = round(mean(topt,  na.rm = TRUE), 2),
    se_topt    = round(sd(topt,    na.rm = TRUE) / sqrt(n()), 2),
    mean_ctmax = round(mean(ctmax, na.rm = TRUE), 2),
    se_ctmax   = round(sd(ctmax,   na.rm = TRUE) / sqrt(n()), 2),
    mean_b80   = round(mean(b80,   na.rm = TRUE), 2),
    se_b80     = round(sd(b80,     na.rm = TRUE) / sqrt(n()), 2),
    .groups    = "drop"
  ) |>
  print()

# =============================================================================
# 6. Plots
# =============================================================================

# ── TPC curves (auc_e) ───────────────────────────────────────────────────────

# Mean SSH curve per evolution history
mean_curves <- tpc_params |>
  group_by(evolution_history) |>
  summarise(across(c(r_tref, e, eh, th), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") |>
  rowwise() |>
  mutate(curve = list(tibble(
    temp = T_PRED,
    pred = sharpeschoolhigh(T_PRED, r_tref, e, eh, th)
  ))) |>
  unnest(curve)

# Observed mean auc_e per strain × temperature
obs_means <- gc_fits |>
  filter(note != "fit_failed", !is.na(auc_e)) |>
  group_by(strain, evolution_history, test_temperature) |>
  summarise(auc_e = mean(auc_e, na.rm = TRUE), .groups = "drop")

p_tpc <- ggplot() +
  geom_line(
    data = tpc_preds |> mutate(pred = ifelse(pred < 0, NA_real_, pred)),
    aes(x = temp, y = pred, group = strain, color = evolution_history),
    alpha = 0.2, linewidth = 0.8
  ) +
  geom_line(
    data = mean_curves,
    aes(x = temp, y = pred, color = evolution_history),
    linewidth = 2.0
  ) +
  geom_point(
    data = obs_means,
    aes(x = test_temperature, y = auc_e, color = evolution_history),
    size = 1.8, alpha = 0.6
  ) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  coord_cartesian(xlim = c(23, 48)) +
  labs(
    x     = "Temperature (°C)",
    y     = "AUC (empirical)",
    title = "Thermal Performance Curves — AUC (empirical)"
  ) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background    = element_blank()
  )

ggsave(file.path(FIGS, "tpc-auc_e.png"), p_tpc, width = 9, height = 6, dpi = 300)
cat("\nSaved: tpc-auc_e.png\n")

# ── Thermal traits (Topt, CTmax, B80) ───────────────────────────────────────

p_traits <- tpc_params |>
  pivot_longer(c(topt, ctmax, b80),
               names_to  = "trait",
               values_to = "value") |>
  mutate(trait = factor(trait,
                        levels = c("topt", "ctmax", "b80"),
                        labels = c("Topt (°C)", "CTmax (°C)", "B80 — niche breadth (°C)"))) |>
  ggplot(aes(x = evolution_history, y = value, color = evolution_history)) +
  geom_jitter(width = 0.12, size = 2.5, alpha = 0.8) +
  stat_summary(fun = mean, geom = "crossbar",
               width = 0.35, linewidth = 0.7, fatten = 1.5) +
  scale_color_manual(values = EVO_COLORS, guide = "none") +
  facet_wrap(~ trait, scales = "free_y") +
  labs(x = NULL, y = NULL,
       title = "Thermal traits by evolution history (AUC empirical)",
       subtitle = "Crossbar = mean") +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 11)
  )

ggsave(file.path(FIGS, "thermal-traits-auc_e.png"), p_traits,
       width = 11, height = 5, dpi = 300)
cat("Saved: thermal-traits-auc_e.png\n")

# =============================================================================
# 7. Export
# =============================================================================

write_csv(gc_fits,    file.path(OUT, "growthcurver-metrics-per-well.csv"))
write_csv(tpc_params, file.path(OUT, "tpc-params-auc_e.csv"))
write_csv(tpc_preds,  file.path(OUT, "tpc-predictions-auc_e.csv"))

cat("\nAll outputs saved to", OUT, "\n")
cat("Files:\n")
for (f in sort(list.files(OUT, recursive = FALSE))) cat(sprintf("  %s\n", f))
cat("\nDone.\n")

gc_fits |> 
  ggplot(aes(x = auc_l, y = auc_e, color = evolution_history)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) + facet_wrap( ~ test_temperature)


