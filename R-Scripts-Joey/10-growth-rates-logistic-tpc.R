# =============================================================================
# Growth Rate Estimation Using 3-Parameter Logistic Model + TPC Analysis
# 
# Adapted from nglabratus_tpc_analysis.R
# Estimates growth rates using 3-parameter logistic model
# Fits Sharpe-Schoolfield High model to estimate Topt and CTmax
# =============================================================================

library(tidyverse)
library(minpack.lm)  # nlsLM for robust nonlinear fitting
library(plotrix)     # for std.error

# Configuration -----------------------------------------------------------

DATA_PATH <- "data-processed/all-blocks-tpc-experiment.csv"
OUT <- "data-processed/thermal-traits"
dir.create(OUT, showWarnings = FALSE)

# Constants for SSH model
K_B <- 8.617333e-5      # Boltzmann constant (eV K⁻¹)
TREF_K <- 288.15        # Reference temperature (15°C in Kelvin)
CTMAX_CAP <- 50.0       # Cap for ctmax if estimation fails
N_STARTS <- 500         # Number of random starting points for multistart fitting

set.seed(42)

# Colors for visualization
EVO_COLORS <- c(
  "40 evolved" = "#FA3208",
  "35 evolved" = "#0E63FF",
  "fRS585"     = "#000000"
)

# ── Helper: Fit 3-parameter logistic model ─────────────────────────────────
# Adapted from nglabratus_tpc_analysis.R lines 202-269

fit_growth_logistic <- function(time_h, OD, min_points = 6) {
  # 3-parameter logistic: K / (1 + exp(-r*(t - t_mid)))
  logistic3 <- function(t, K, r, t_mid) {
    K / (1 + exp(-r * (t - t_mid)))
  }
  
  n <- length(time_h)
  if (n < min_points) return(c(mu = NA_real_, r2 = NA_real_, lag_h = NA_real_))
  
  OD_max  <- max(OD)
  OD_min  <- min(OD)
  t_range <- tail(time_h, 1) - time_h[1]
  
  # Numerical gradient for initial guesses
  grad   <- diff(OD) / diff(time_h)
  grad_t <- (head(time_h, -1) + tail(time_h, -1)) / 2
  K0     <- OD_max * 1.05
  t_mid0 <- grad_t[which.max(grad)]
  r0     <- max(4 * max(grad) / K0, 0.05)
  
  result <- tryCatch({
    fit <- nlsLM(
      OD ~ logistic3(time_h, K, r, t_mid),
      start  = list(K = K0, r = r0, t_mid = t_mid0),
      lower  = c(K = OD_min, r = 0.005, t_mid = -t_range),
      upper  = c(K = OD_max * 3, r = 20.0, t_mid = tail(time_h, 1) + t_range),
      control = nls.lm.control(maxiter = 200, maxfev = 10000)
    )
    K_fit   <- coef(fit)[["K"]]
    r_fit   <- coef(fit)[["r"]]
    t_mid_fit <- coef(fit)[["t_mid"]]
    
    if (K_fit < OD_min || r_fit <= 0) stop("Implausible params")
    
    pred   <- predict(fit)
    ss_res <- sum((OD - pred)^2)
    ss_tot <- sum((OD - mean(OD))^2)
    r2     <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
    
    # mu (day⁻¹), R², lag (h)
    c(mu    = r_fit * K_fit / 4 * 24,
      r2    = r2,
      lag_h = max(t_mid_fit - 2.0 / r_fit, 0.0))
  }, error = function(e) NULL)
  
  if (!is.null(result)) return(result)
  
  # ── Fallback: sliding-window max slope on ln(OD) ──────────────────────────
  ln_OD    <- log(pmax(OD, 1e-6))
  best_mu  <- NA_real_
  best_r2  <- NA_real_
  
  for (start in seq_len(n - 4)) {
    for (end in (start + 4):n) {
      x <- time_h[start:end]
      y <- ln_OD[start:end]
      if (diff(range(x)) == 0) next
      fit_lm <- lm(y ~ x)
      slope  <- coef(fit_lm)[["x"]]
      r2_sw  <- summary(fit_lm)$r.squared
      if (is.na(best_mu) || slope > best_mu) {
        best_mu <- slope * 24
        best_r2 <- r2_sw
      }
    }
  }
  
  c(mu = best_mu, r2 = best_r2, lag_h = NA_real_)
}

# ── Sharpe-Schoolfield High Model ──────────────────────────────────────────

sharpeschoolhigh <- function(T_c, r_tref, e, eh, th) {
  T_k         <- T_c + 273.15
  boltz_act   <- (e  / K_B) * (1 / TREF_K - 1 / T_k)
  boltz_deact <- (eh / K_B) * (1 / th     - 1 / T_k)
  r_tref * exp(boltz_act) / (1 + exp(boltz_deact))
}

# ── Calculate Topt and CTmax from SSH parameters ────────────────────────────

calc_topt_ctmax <- function(params, T_range = NULL) {
  if (is.null(T_range)) T_range <- seq(5, 55, length.out = 5000)
  preds <- sharpeschoolhigh(T_range, params[1], params[2], params[3], params[4])
  valid <- is.finite(preds)
  if (!any(valid)) return(c(topt = NA, ctmax = NA, rmax = NA))
  
  topt_idx <- which.max(preds[valid])
  topt     <- T_range[valid][topt_idx]
  rmax     <- preds[valid][topt_idx]
  
  # Try zero-crossing above Topt (never happens for SSH, but included for parity)
  pa          <- preds
  pa[T_range <= topt] <- rmax
  sign_changes <- which(diff(sign(pa)) != 0)
  
  if (length(sign_changes) > 0) {
    i  <- sign_changes[1]
    t1 <- T_range[i];     t2 <- T_range[i + 1]
    p1 <- pa[i];          p2 <- pa[i + 1]
    ctmax <- if (p2 != p1) t1 - p1 * (t2 - t1) / (p2 - p1) else t1
  } else {
    # Operational CT_max: first T above Topt where predicted rate < 5% of rmax
    below <- which(T_range > topt & preds < 0.05 * rmax)
    ctmax <- if (length(below) > 0) T_range[below[1]] else CTMAX_CAP
  }
  
  c(topt = topt, ctmax = ctmax, rmax = rmax)
}

# =============================================================================
# 1. Load and prepare data
# =============================================================================

edata <- read_csv(DATA_PATH) |> 
  filter(evolution_history %in% c("35 evolved", "40 evolved", "fRS585")) |>
  mutate(rep.id = paste(well, block, test_temperature, strain, evolution_history, sep = "_"))

cat(sprintf("Loaded %d rows of data\n", nrow(edata)))

# Convert days to hours for logistic fitting
edata <- edata |>
  mutate(time_h = days * 24)

# =============================================================================
# 2. Estimate growth rates using 3-parameter logistic model
# =============================================================================

gr_list <- edata |>
  group_by(rep.id) |>
  arrange(time_h) |>
  group_map(function(grp, keys) {
    res <- fit_growth_logistic(grp$time_h, grp$od)
    tibble(
      well = grp$well[1],
      block = grp$block[1],
      test_temperature = grp$test_temperature[1],
      strain = grp$strain[1],
      evolution_history = grp$evolution_history[1],
      mu = res["mu"],
      r2 = res["r2"],
      lag_h = res["lag_h"]
    )
  })

growth_rates <- bind_rows(gr_list)

cat(sprintf("Growth rates estimated: %d wells\n", nrow(growth_rates)))
print(
  growth_rates |>
    group_by(test_temperature) |>
    summarise(
      n    = n(),
      mean = round(mean(mu, na.rm = TRUE), 3),
      sd   = round(sd(mu, na.rm = TRUE), 3),
      min  = round(min(mu, na.rm = TRUE), 3),
      max  = round(max(mu, na.rm = TRUE), 3)
    )
)

# Average across replicate wells → one value per strain × block × temperature
mu_mean <- growth_rates |>
  group_by(strain, evolution_history, block, test_temperature) |>
  summarise(
    mu      = mean(mu, na.rm = TRUE),
    n_wells = sum(!is.na(mu)),
    .groups = "drop"
  ) |>
  rename(temp = test_temperature)

cat(sprintf("\nmu_mean rows: %d\n", nrow(mu_mean)))

# =============================================================================
# 3. Fit Sharpe-Schoolfield High model + extract Topt / CTmax
# =============================================================================

# Multistart bounds
BOUNDS_LO <- c(r_tref = 0.01, e = 0.01, eh =  0.5, th = 303.0)
BOUNDS_HI <- c(r_tref = 50.0, e =  3.0, eh = 50.0, th = 335.0)
START_LO  <- c(r_tref =  0.5, e =  0.1, eh =  0.5, th = 305.0)
START_HI  <- c(r_tref = 20.0, e =  2.0, eh = 20.0, th = 325.0)

T_pred <- seq(15, 50, length.out = 300)

fit_params_ssh <- list()
fit_preds_ssh  <- list()

for (sid in unique(mu_mean$strain)) {
  d     <- mu_mean |> filter(strain == sid)
  evo   <- d$evolution_history[1]
  temps <- as.numeric(d$temp)
  rates <- as.numeric(d$mu)
  
  best_popt <- NULL
  best_rss  <- Inf
  
  # 500 random starts
  starts <- mapply(function(...) c(...),
                   runif(N_STARTS, START_LO["r_tref"], START_HI["r_tref"]),
                   runif(N_STARTS, START_LO["e"],      START_HI["e"]),
                   runif(N_STARTS, START_LO["eh"],     START_HI["eh"]),
                   runif(N_STARTS, START_LO["th"],     START_HI["th"]),
                   SIMPLIFY = FALSE)
  
  for (p0 in starts) {
    tryCatch({
      fit <- suppressWarnings(nlsLM(
        rates ~ sharpeschoolhigh(temps, r_tref, e, eh, th),
        start  = list(r_tref = p0[1], e = p0[2], eh = p0[3], th = p0[4]),
        lower  = BOUNDS_LO,
        upper  = BOUNDS_HI,
        control = nls.lm.control(maxiter = 200, maxfev = 5000)
      ))
      rss <- sum(residuals(fit)^2)
      if (rss < best_rss) {
        best_rss  <- rss
        best_popt <- coef(fit)
      }
    }, error = function(e) NULL)
  }
  
  if (is.null(best_popt)) {
    cat(sprintf("  FAILED: %s\n", sid))
    next
  }
  
  ss_tot <- sum((rates - mean(rates))^2)
  r2     <- if (ss_tot > 0) 1 - best_rss / ss_tot else NA_real_
  
  derived <- calc_topt_ctmax(best_popt)
  topt    <- derived["topt"]
  ctmax   <- derived["ctmax"]
  rmax    <- derived["rmax"]
  if (is.na(ctmax)) ctmax <- CTMAX_CAP
  
  fit_params_ssh[[length(fit_params_ssh) + 1]] <- data.frame(
    strain      = sid,
    evo_history = evo,
    r_tref      = best_popt["r_tref"],
    e           = best_popt["e"],
    eh          = best_popt["eh"],
    th          = best_popt["th"],
    topt        = topt,
    ctmax       = ctmax,
    rmax        = rmax,
    r2          = r2,
    row.names   = NULL
  )
  
  pred_rows <- data.frame(
    strain      = sid,
    evo_history = evo,
    temp        = T_pred,
    pred        = sharpeschoolhigh(T_pred,
                                    best_popt["r_tref"], best_popt["e"],
                                    best_popt["eh"],     best_popt["th"])
  )
  fit_preds_ssh[[length(fit_preds_ssh) + 1]] <- pred_rows
}

tpc_params <- bind_rows(fit_params_ssh)
tpc_preds  <- bind_rows(fit_preds_ssh)

cat(sprintf("Fitted: %d strains\n", nrow(tpc_params)))

# =============================================================================
# 4. Summary statistics
# =============================================================================

cat("\nMean Topt and CTmax by group:\n")
print(
  tpc_params |>
    group_by(evo_history) |>
    summarise(
      n_strains  = n(),
      mean_topt  = round(mean(topt,  na.rm = TRUE), 2),
      sd_topt    = round(sd(topt,    na.rm = TRUE), 2),
      mean_ctmax = round(mean(ctmax, na.rm = TRUE), 2),
      sd_ctmax   = round(sd(ctmax,   na.rm = TRUE), 2),
      .groups = "drop"
    )
)

# =============================================================================
# 5. Visualizations
# =============================================================================

# Plot Topt by evolution history
p_topt <- tpc_params |>
  ggplot(aes(x = evo_history, y = topt, color = evo_history)) +
  geom_point(size = 2.5, alpha = 0.7) +
  scale_color_manual(values = EVO_COLORS) +
  labs(
    x = NULL,
    y = "Optimal Temperature (°C)",
    title = "Thermal Optimum (Topt)"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

ggsave(file.path(OUT, "topt_by_evolution.png"), p_topt,
       width = 5, height = 5, dpi = 300)
print(p_topt)

# Plot CTmax by evolution history
p_ctmax <- tpc_params |>
  ggplot(aes(x = evo_history, y = ctmax, color = evo_history)) +
  geom_point(size = 2.5, alpha = 0.7) +
  scale_color_manual(values = EVO_COLORS) +
  labs(
    x = NULL,
    y = "Critical Thermal Maximum (°C)",
    title = "Thermal Tolerance (CTmax)"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

ggsave(file.path(OUT, "ctmax_by_evolution.png"), p_ctmax,
       width = 5, height = 5, dpi = 300)
print(p_ctmax)

# TPC overlay by evolution history
PRIMARY <- c("40 evolved", "35 evolved", "fRS585")

indiv_curves <- tpc_preds |>
  filter(evo_history %in% PRIMARY) |>
  mutate(pred = ifelse(pred < 0, NA_real_, pred))

mean_curves <- tpc_params |>
  filter(evo_history %in% PRIMARY) |>
  group_by(evo_history) |>
  summarise(r_tref = mean(r_tref), e = mean(e),
            eh = mean(eh), th = mean(th), .groups = "drop") |>
  rowwise() |>
  mutate(data = list(data.frame(
    temp = T_pred,
    pred = sharpeschoolhigh(T_pred, r_tref, e, eh, th)
  ))) |>
  unnest(data)

p_tpc <- ggplot() +
  geom_line(data = indiv_curves,
            aes(x = temp, y = pred, group = strain, color = evo_history),
            alpha = 0.22, linewidth = 1.0) +
  geom_line(data = mean_curves,
            aes(x = temp, y = pred, color = evo_history),
            linewidth = 2.5) +
  geom_point(data = mu_mean |> filter(evo_history %in% PRIMARY),
             aes(x = temp, y = mu, color = evo_history),
             size = 2.5, alpha = 0.6) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  coord_cartesian(xlim = c(24, 50), ylim = c(0, 8)) +
  labs(
    x = "Temperature (°C)",
    y = "Growth Rate (day⁻¹)",
    title = "Thermal Performance Curves"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_blank()
  )

ggsave(file.path(OUT, "tpc_overlay.png"), p_tpc,
       width = 9, height = 6, dpi = 300)
print(p_tpc)

# =============================================================================
# 6. Export results
# =============================================================================

# Per-strain thermal traits
tpc_params |>
  select(strain, evo_history, topt, ctmax) |>
  arrange(evo_history, strain) |>
  rename(T_opt_C = topt, CT_max_C = ctmax) |>
  write_csv(file.path(OUT, "thermal-traits-per-strain.csv"))

# SSH parameters
write_csv(tpc_params, file.path(OUT, "ssh-parameters.csv"))

# Growth rates averaged by strain, block, temperature
mu_mean |>
  write_csv(file.path(OUT, "growth-rates-mean.csv"))

# All individual growth rates
growth_rates |>
  write_csv(file.path(OUT, "growth-rates-all-wells.csv"))

# TPC predictions
tpc_preds |>
  write_csv(file.path(OUT, "tpc-predictions.csv"))

cat(sprintf("\nExported to %s/\n", OUT))
for (f in list.files(OUT, full.names = FALSE)) {
  cat(sprintf("  %s\n", f))
}
