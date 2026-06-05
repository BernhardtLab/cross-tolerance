# =============================================================================
# 13b-compare-mu-formulas.R
#
# Diagnostic: compare TPCs derived from two growth rate formulas:
#   mu_r   = r          (logistic rate parameter, day⁻¹)
#   mu_rk4 = r * K / 4  (max absolute OD rate, day⁻¹)
#
# Fits SSH TPC per strain for each formula and plots them together.
# =============================================================================

library(tidyverse)
library(minpack.lm)
library(cowplot)
theme_set(theme_cowplot())

# Config ----------------------------------------------------------------------

DATA_PATH <- "data-processed/all-blocks-tpc-experiment.csv"
OUT       <- "data-processed/logistic-tpc"
dir.create(OUT, showWarnings = FALSE)

TARGET_EVO <- c("35 evolved", "40 evolved", "fRS585")
EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")

K_B    <- 8.617333e-5
TREF_K <- 288.15
N_STARTS <- 300
T_PRED   <- seq(15, 50, length.out = 500)
CTMAX_CAP <- 50.0
set.seed(42)

# Helpers ---------------------------------------------------------------------

logistic3 <- function(t, K, r, t_mid) K / (1 + exp(-r * (t - t_mid)))

sharpeschoolhigh <- function(T_c, r_tref, e, eh, th) {
  T_k <- T_c + 273.15
  r_tref * exp((e / K_B) * (1 / TREF_K - 1 / T_k)) /
    (1 + exp((eh / K_B) * (1 / th - 1 / T_k)))
}

calc_topt_ctmax <- function(r_tref, e, eh, th,
                             T_range = seq(5, 55, length.out = 5000)) {
  preds <- sharpeschoolhigh(T_range, r_tref, e, eh, th)
  valid <- is.finite(preds)
  if (!any(valid)) return(c(topt = NA_real_, ctmax = NA_real_))
  topt  <- T_range[valid][which.max(preds[valid])]
  rmax  <- max(preds[valid])
  pa    <- preds; pa[T_range <= topt] <- rmax
  sc    <- which(diff(sign(pa)) != 0)
  if (length(sc) > 0) {
    i <- sc[1]; t1 <- T_range[i]; t2 <- T_range[i+1]; p1 <- pa[i]; p2 <- pa[i+1]
    ctmax <- if (p2 != p1) t1 - p1 * (t2 - t1) / (p2 - p1) else t1
  } else {
    below <- which(T_range > topt & preds < 0.05 * rmax)
    ctmax <- if (length(below) > 0) T_range[below[1]] else CTMAX_CAP
  }
  c(topt = topt, ctmax = ctmax)
}

fit_logistic_both <- function(days, od, min_points = 6) {
  # Returns mu_r (=r) and mu_rk4 (=r*K/4), both in day⁻¹
  if (length(days) < min_points) return(c(mu_r = NA, mu_rk4 = NA, r2 = NA))
  od   <- as.numeric(od); days <- as.numeric(days)
  omax <- max(od); omin <- min(od); tr <- diff(range(days))
  grad <- diff(od) / diff(days)
  K0   <- omax * 1.05
  tm0  <- mean(days[c(which.max(grad), which.max(grad)+1)])
  r0   <- max(4 * max(grad) / K0, 0.05)

  tryCatch({
    fit <- nlsLM(
      od ~ logistic3(days, K, r, t_mid),
      start   = list(K = K0, r = r0, t_mid = tm0),
      lower   = c(K = omin, r = 1e-3, t_mid = days[1] - tr),
      upper   = c(K = omax * 3, r = 50, t_mid = tail(days,1) + tr),
      control = nls.lm.control(maxiter = 200, maxfev = 10000)
    )
    K_fit <- coef(fit)[["K"]]; r_fit <- coef(fit)[["r"]]
    if (K_fit < omin || r_fit <= 0) stop("bad params")
    pred <- predict(fit)
    r2   <- 1 - sum((od - pred)^2) / sum((od - mean(od))^2)
    c(mu_r = r_fit, mu_rk4 = r_fit * K_fit / 4, r2 = r2)
  }, error = function(e) c(mu_r = NA, mu_rk4 = NA, r2 = NA))
}

fit_ssh_multistart <- function(temps, rates, n_starts = N_STARTS) {
  BLO <- c(r_tref=1e-3, e=0.01, eh=0.5,  th=303)
  BHI <- c(r_tref=50,   e=3,    eh=50,   th=335)
  SLO <- c(r_tref=0.5,  e=0.1,  eh=0.5,  th=305)
  SHI <- c(r_tref=20,   e=2,    eh=20,   th=325)
  starts <- mapply(function(a,b,c,d) c(a,b,c,d),
                   runif(n_starts, SLO[1], SHI[1]), runif(n_starts, SLO[2], SHI[2]),
                   runif(n_starts, SLO[3], SHI[3]), runif(n_starts, SLO[4], SHI[4]),
                   SIMPLIFY = FALSE)
  best <- NULL; best_rss <- Inf
  for (p0 in starts) {
    tryCatch({
      fit <- suppressWarnings(nlsLM(
        rates ~ sharpeschoolhigh(temps, r_tref, e, eh, th),
        start = list(r_tref=p0[1], e=p0[2], eh=p0[3], th=p0[4]),
        lower = BLO, upper = BHI,
        control = nls.lm.control(maxiter=200, maxfev=5000)
      ))
      rss <- sum(residuals(fit)^2)
      if (rss < best_rss) { best_rss <- rss; best <- coef(fit) }
    }, error = function(e) NULL)
  }
  if (is.null(best)) return(NULL)
  list(params = best,
       derived = calc_topt_ctmax(best["r_tref"], best["e"], best["eh"], best["th"]))
}

# 1. Load & fit logistic ------------------------------------------------------

cat("Fitting logistic model...\n")

edata <- read_csv(DATA_PATH, show_col_types = FALSE) |>
  filter(evolution_history %in% TARGET_EVO)

gr <- edata |>
  group_by(well, block, test_temperature, strain, evolution_history) |>
  arrange(days, .by_group = TRUE) |>
  summarise(fit = list(fit_logistic_both(days, od)), .groups = "drop") |>
  mutate(
    mu_r   = map_dbl(fit, "mu_r"),
    mu_rk4 = map_dbl(fit, "mu_rk4"),
    r2     = map_dbl(fit, "r2")
  ) |>
  select(-fit)

cat(sprintf("  %d wells fit\n", nrow(gr)))

# 2. Fit SSH TPCs for each formula × aggregation combo -----------------------
#
# aggregated = TRUE:  average mu across replicate wells per strain × temp first
# aggregated = FALSE: use all replicate wells as individual data points

fit_tpcs_for <- function(gr, mu_col, aggregated) {
  label     <- sprintf("%s | %s", mu_col, ifelse(aggregated, "averaged", "all reps"))
  cat(sprintf("\nFitting SSH TPCs: %s...\n", label))
  rates_col <- sym(mu_col)

  # Optionally average across wells before fitting
  d_input <- if (aggregated) {
    gr |>
      filter(!is.na(!!rates_col)) |>
      group_by(strain, evolution_history, test_temperature) |>
      summarise(!!rates_col := mean(!!rates_col, na.rm = TRUE), .groups = "drop")
  } else {
    gr |> filter(!is.na(!!rates_col))
  }

  strains <- d_input |>
    group_by(strain, evolution_history) |>
    summarise(n_temps = n_distinct(test_temperature), .groups = "drop") |>
    filter(n_temps >= 3)

  params_list <- vector("list", nrow(strains))
  preds_list  <- vector("list", nrow(strains))

  for (i in seq_len(nrow(strains))) {
    sid <- strains$strain[i]; evo <- strains$evolution_history[i]
    d   <- d_input |> filter(strain == sid)
    res <- fit_ssh_multistart(d$test_temperature, pull(d, !!rates_col))
    if (is.null(res)) { cat(sprintf("  FAILED: %s\n", sid)); next }

    p <- res$params
    params_list[[i]] <- tibble(
      strain = sid, evolution_history = evo,
      formula = mu_col, aggregated = aggregated,
      topt = res$derived["topt"], ctmax = res$derived["ctmax"]
    )
    preds_list[[i]] <- tibble(
      strain = sid, evolution_history = evo,
      formula = mu_col, aggregated = aggregated,
      temp = T_PRED,
      pred = sharpeschoolhigh(T_PRED, p["r_tref"], p["e"], p["eh"], p["th"])
    )
    if (i %% 10 == 0) cat(sprintf("  %d/%d\n", i, nrow(strains)))
  }

  list(params = bind_rows(params_list), preds = bind_rows(preds_list))
}

# Run all four combinations
combos <- list(
  list(mu_col = "mu_r",   aggregated = FALSE),
  list(mu_col = "mu_r",   aggregated = TRUE),
  list(mu_col = "mu_rk4", aggregated = FALSE),
  list(mu_col = "mu_rk4", aggregated = TRUE)
)

results <- lapply(combos, function(x) fit_tpcs_for(gr, x$mu_col, x$aggregated))

all_params <- bind_rows(lapply(results, `[[`, "params"))
all_preds  <- bind_rows(lapply(results, `[[`, "preds"))

# Add readable labels for plotting
all_params <- all_params |>
  mutate(
    formula_label = ifelse(formula == "mu_r", "r (day⁻¹)", "r·K/4 (day⁻¹)"),
    agg_label     = ifelse(aggregated, "Averaged reps", "All reps"),
    panel_label   = paste0(formula_label, "\n", agg_label)
  )
all_preds <- all_preds |>
  mutate(
    formula_label = ifelse(formula == "mu_r", "r (day⁻¹)", "r·K/4 (day⁻¹)"),
    agg_label     = ifelse(aggregated, "Averaged reps", "All reps"),
    panel_label   = paste0(formula_label, "\n", agg_label)
  )

# 3. Print Topt / CTmax comparison -------------------------------------------

cat("\n=== Topt and CTmax by formula, aggregation, and evolution history ===\n")
all_params |>
  group_by(formula, aggregated, evolution_history) |>
  summarise(
    n          = n(),
    mean_topt  = round(mean(topt,  na.rm=TRUE), 2),
    mean_ctmax = round(mean(ctmax, na.rm=TRUE), 2),
    .groups    = "drop"
  ) |>
  arrange(evolution_history, formula, aggregated) |>
  print()

# 4. TPC overlay: 2×2 grid (formula × aggregation) ---------------------------

# Observed data points — one set per formula (y-axis scale differs)
obs_points <- bind_rows(
  gr |> filter(!is.na(mu_r)) |>
    group_by(strain, evolution_history, test_temperature) |>
    summarise(mu = mean(mu_r,   na.rm=TRUE), .groups="drop") |>
    mutate(formula_label = "r (day⁻¹)"),
  gr |> filter(!is.na(mu_rk4)) |>
    group_by(strain, evolution_history, test_temperature) |>
    summarise(mu = mean(mu_rk4, na.rm=TRUE), .groups="drop") |>
    mutate(formula_label = "r·K/4 (day⁻¹)")
)

indiv_curves <- all_preds |>
  filter(evolution_history %in% TARGET_EVO) |>
  mutate(pred = ifelse(pred < 0, NA_real_, pred))

mean_curves <- indiv_curves |>
  group_by(panel_label, formula_label, agg_label, evolution_history, temp) |>
  summarise(pred = mean(pred, na.rm=TRUE), .groups="drop")

p_tpc_grid <- ggplot() +
  geom_line(
    data = indiv_curves,
    aes(x=temp, y=pred, group=strain, color=evolution_history),
    alpha=0.18, linewidth=0.8
  ) +
  geom_line(
    data = mean_curves,
    aes(x=temp, y=pred, color=evolution_history),
    linewidth=2.0
  ) +
  geom_point(
    data = obs_points |> filter(evolution_history %in% TARGET_EVO),
    aes(x=test_temperature, y=mu, color=evolution_history),
    size=1.6, alpha=0.5
  ) +
  scale_color_manual(values=EVO_COLORS, name=NULL) +
  facet_grid(agg_label ~ formula_label, scales="free_y") +
  coord_cartesian(xlim=c(23, 48)) +
  labs(
    x     = "Temperature (°C)",
    y     = "Growth Rate",
    title = "TPC comparison: formula (columns) × aggregation (rows)"
  ) +
  theme(
    legend.position  = "bottom",
    strip.background = element_blank(),
    strip.text       = element_text(face="bold", size=12)
  )

ggsave(file.path(OUT, "13b-tpc-2x2-comparison.png"),
       p_tpc_grid, width=12, height=9, dpi=300)
cat(sprintf("\nPlot saved: %s/13b-tpc-2x2-comparison.png\n", OUT))

# 5. Topt / CTmax dot plot: all four combos ----------------------------------

p_traits <- all_params |>
  filter(evolution_history %in% TARGET_EVO) |>
  pivot_longer(c(topt, ctmax), names_to="trait", values_to="value") |>
  ggplot(aes(x=evolution_history, y=value,
             color=evolution_history, shape=agg_label)) +
  geom_jitter(width=0.12, size=2.3, alpha=0.8) +
  stat_summary(aes(group=agg_label),
               fun=mean, geom="crossbar",
               width=0.3, linewidth=0.7, fatten=1.5) +
  scale_color_manual(values=EVO_COLORS, guide="none") +
  scale_shape_manual(values=c("All reps"=16, "Averaged reps"=17), name=NULL) +
  facet_grid(
    trait ~ formula_label,
    scales="free_y",
    labeller=labeller(trait=c(topt="Topt (°C)", ctmax="CTmax (°C)"))
  ) +
  labs(x=NULL, y=NULL, title="Thermal traits: formula (columns) × aggregation (rows)") +
  theme(
    legend.position  = "bottom",
    strip.background = element_blank(),
    strip.text       = element_text(face="bold")
  )

ggsave(file.path(OUT, "13b-traits-2x2-comparison.png"),
       p_traits, width=11, height=7, dpi=300)
cat(sprintf("Plot saved: %s/13b-traits-2x2-comparison.png\n", OUT))

cat("\nDone.\n")
