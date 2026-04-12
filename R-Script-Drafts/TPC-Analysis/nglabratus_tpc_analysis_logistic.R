# =============================================================================
# N. glabratus Thermal Performance Curve Analysis
# Converted from Python/Jupyter notebook
#
# Fits thermal performance curves (TPCs) for N. glabratus populations
# experimentally evolved at 40°C or 35°C, plus the ancestral strain (fRS585),
# assayed at 25°C, 35°C, 38°C, 41°C, and 42°C.
#
# Identical to nglabratus_tpc_analysis.R except:
#   - OD data imported from data-processed/all-blocks-tpc-experiment.csv
#     instead of Table_S1.xlsx
#   - Growth rate estimation uses 3-parameter logistic only — no sliding-window
#     fallback. Wells where the logistic fails return NA.
#
# Input:  data-processed/all-blocks-tpc-experiment.csv
# Output: Processed_Outputs_logistic/
# =============================================================================

# ── Libraries ─────────────────────────────────────────────────────────────────
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(lme4)
library(car)       # for Wald chi-sq tests
library(minpack.lm)  # nlsLM for robust nonlinear fitting

# ── Configuration ─────────────────────────────────────────────────────────────
# DATA_PATH  <- "R-Script-Drafts/TPC-Analysis/Table_S1.xlsx"
OUT        <- "R-Script-Drafts/TPC-Analysis/Processed_Outputs_logistic"
dir.create(OUT, showWarnings = FALSE)

# Boltzmann constant (eV K⁻¹) and reference temperature (15°C in Kelvin)
K_B      <- 8.617333e-5
TREF_K   <- 288.15
CTMAX_CAP <- 50.0
N_STARTS  <- 500
set.seed(42)

EVO_COLORS <- c(
  "40°C Evolved" = "#FA3208",
  "35°C Evolved" = "#0E63FF",
  "Ancestor"     = "#000000"
)

# ── Helper: convert time values to decimal hours ───────────────────────────────
# Excel stores times as fractions of a day (e.g. 0.5 = 12:00).
# readxl returns them as numeric fractions when col_types = "numeric".
# Multiply by 24 to get hours.
time_to_hours <- function(x) {
  as.numeric(x) * 24
}

# =============================================================================
# 1 & 2. Load OD data from all-blocks-tpc-experiment.csv
#        (replaces Table_S1.xlsx parsing in the original script)
#
# all-blocks-tpc-experiment.csv columns:
#   start_time, days, time, t_600, well, od, strain, test_temperature,
#   block, evolution_history
#
# Mapped to match the variable names used throughout the rest of this script:
#   time_h  = days * 24   (hours, as in the original)
#   OD      = od
#   temp    = test_temperature
#   evo_history assigned via the same assign_evo() logic as the original
# =============================================================================

assign_evo <- function(strain) {
  case_when(
    grepl("^40_", strain)   ~ "40°C Evolved",
    grepl("^35_", strain)   ~ "35°C Evolved",
    grepl("fRS585", strain) ~ "Ancestor",
    TRUE                    ~ "Other"
  )
}

PRIMARY <- c("40°C Evolved", "35°C Evolved", "Ancestor")

df_all <- read_csv("data-processed/all-blocks-tpc-experiment.csv",
                   show_col_types = FALSE) %>%
  filter(!is.na(strain)) %>%
  rename(
    OD   = od,
    temp = test_temperature
  ) %>%
  mutate(
    time_h      = days * 24,
    OD          = pmax(OD, 1e-4),
    evo_history = assign_evo(strain)
  ) %>%
  filter(evo_history %in% PRIMARY)

cat(sprintf("Total rows: %s\n", format(nrow(df_all), big.mark = ",")))

# =============================================================================
# 3. Plot raw OD600 growth curves (QC)
# =============================================================================

plot_df <- df_all %>%
  filter(evo_history %in% PRIMARY) %>%
  group_by(strain, evo_history, time_h, temp) %>%
  summarise(OD = mean(OD), .groups = "drop")

p_raw <- ggplot(plot_df, aes(x = time_h, y = OD,
                              group = strain, color = evo_history)) +
  geom_line(alpha = 0.55, linewidth = 0.8) +
  facet_wrap(~ temp, labeller = label_bquote(.(temp) * "°C")) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(x = "Time (h)", y = "OD600",
       title = "Raw OD600 Growth Curves") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

ggsave(file.path(OUT, "qc_raw_curves.png"), p_raw,
       width = 13, height = 8, dpi = 150)
print(p_raw)

# =============================================================================
# 4. Estimate growth rates (3-parameter logistic only — no fallback)
# =============================================================================

fit_growth_logistic <- function(time_h, OD, min_points = 6) {
  # 3-parameter logistic: K / (1 + exp(-r*(t - t_mid)))
  logistic3 <- function(t, K, r, t_mid) {
    K / (1 + exp(-r * (t - t_mid)))
  }

  n <- length(time_h)
  if (n < min_points) return(c(mu = NA_real_, r2 = NA_real_, lag_h = NA_real_))

  OD_max  <- max(OD);  OD_min <- min(OD)
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
    K_fit     <- coef(fit)[["K"]]
    r_fit     <- coef(fit)[["r"]]
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

  # Return NA if logistic fit failed — no fallback
  if (is.null(result)) return(c(mu = NA_real_, r2 = NA_real_, lag_h = NA_real_))

  result
}

# Run on every well
gr_list <- df_all %>%
  group_by(strain, evo_history, block, well, temp) %>%
  arrange(time_h) %>%
  group_map(function(grp, keys) {
    res <- fit_growth_logistic(grp$time_h, grp$OD)
    bind_cols(keys, as.data.frame(t(res)))
  })

growth_rates <- bind_rows(gr_list)
cat(sprintf("Growth rates estimated: %s wells\n",
            format(nrow(growth_rates), big.mark = ",")))
print(
  growth_rates %>%
    group_by(temp) %>%
    summarise(
      n    = n(),
      mean = round(mean(mu, na.rm = TRUE), 3),
      sd   = round(sd(mu, na.rm = TRUE), 3),
      min  = round(min(mu, na.rm = TRUE), 3),
      max  = round(max(mu, na.rm = TRUE), 3)
    )
)

# =============================================================================
# 4b. Diagnostic plots — logistic fit overlaid on OD time series
# =============================================================================

diag_dir <- file.path(OUT, "diagnostic-logistic")
dir.create(diag_dir, showWarnings = FALSE)

# Helper: re-fit logistic and return a data frame of predicted values,
# or NULL if the fit fails.
logistic3 <- function(t, K, r, t_mid) K / (1 + exp(-r * (t - t_mid)))

get_logistic_pred <- function(time_h, OD, min_points = 6) {
  n <- length(time_h)
  if (n < min_points) return(NULL)

  OD_max  <- max(OD);  OD_min <- min(OD)
  t_range <- tail(time_h, 1) - time_h[1]
  grad    <- diff(OD) / diff(time_h)
  grad_t  <- (head(time_h, -1) + tail(time_h, -1)) / 2
  K0      <- OD_max * 1.05
  t_mid0  <- grad_t[which.max(grad)]
  r0      <- max(4 * max(grad) / K0, 0.05)

  tryCatch({
    fit <- nlsLM(
      OD ~ logistic3(time_h, K, r, t_mid),
      start   = list(K = K0, r = r0, t_mid = t_mid0),
      lower   = c(K = OD_min, r = 0.005, t_mid = -t_range),
      upper   = c(K = OD_max * 3, r = 20.0, t_mid = tail(time_h, 1) + t_range),
      control = nls.lm.control(maxiter = 200, maxfev = 10000)
    )
    K_fit <- coef(fit)[["K"]];  r_fit <- coef(fit)[["r"]]
    if (K_fit < OD_min || r_fit <= 0) stop("Implausible params")

    t_seq <- seq(min(time_h), max(time_h), length.out = 200)
    data.frame(
      time_h = t_seq,
      OD_fit = logistic3(t_seq, K_fit, r_fit, coef(fit)[["t_mid"]])
    )
  }, error = function(e) NULL)
}

# Join r2 from growth_rates onto raw data for annotation
diag_data <- df_all %>%
  left_join(
    growth_rates %>% select(strain, evo_history, block, well, temp, r2, mu),
    by = c("strain", "evo_history", "block", "well", "temp")
  )

series_keys <- diag_data %>%
  distinct(strain, evo_history, block, well, temp)

walk(seq_len(nrow(series_keys)), function(i) {

  s    <- series_keys$strain[i]
  evo  <- series_keys$evo_history[i]
  b    <- series_keys$block[i]
  w    <- series_keys$well[i]
  tmp  <- series_keys$temp[i]

  pd <- diag_data %>%
    filter(strain == s, block == b, well == w, temp == tmp) %>%
    arrange(time_h)

  r2_val <- unique(pd$r2)
  mu_val <- unique(pd$mu)

  fit_curve <- get_logistic_pred(pd$time_h, pd$OD)

  dot_color  <- if (evo %in% names(EVO_COLORS)) EVO_COLORS[[evo]] else "gray40"

  label_text <- if (!is.na(r2_val)) {
    paste0("μ = ", round(mu_val, 3), " day⁻¹\nR² = ", round(r2_val, 3))
  } else {
    "Logistic fit failed"
  }

  p <- ggplot(pd, aes(x = time_h, y = OD)) +
    geom_point(color = "gray60", size = 1.8, alpha = 0.7)

  if (!is.null(fit_curve)) {
    p <- p + geom_line(
      data      = fit_curve,
      aes(x = time_h, y = OD_fit),
      color     = dot_color,
      linewidth = 1.1
    )
  }

  p <- p +
    annotate(
      "text",
      x = -Inf, y = Inf,
      label = label_text,
      hjust = -0.1, vjust = 1.2,
      size  = 3.2
    ) +
    labs(
      title = paste0(s, "  |  ", w, "  |  block ", b, "  |  ", tmp, "°C"),
      x     = "Time (h)",
      y     = "OD600"
    ) +
    theme_bw(base_size = 11)

  fname <- file.path(
    diag_dir,
    paste0(s, "_", w, "_block", b, "_", tmp, "C.png")
  )
  ggsave(fname, p, width = 6, height = 4)
})

cat(sprintf("Diagnostic plots saved to %s/\n", diag_dir))

# Average across replicate wells → one value per strain × block × temperature
mu_mean <- growth_rates %>%
  group_by(strain, evo_history, block, temp) %>%
  summarise(mu      = mean(mu, na.rm = TRUE),
            n_wells = sum(!is.na(mu)),
            .groups = "drop")

cat(sprintf("mu_mean rows: %d\n", nrow(mu_mean)))

# =============================================================================
# 5. Sharpe–Schoolfield High model + Topt / CTmax extraction
# =============================================================================

sharpeschoolhigh <- function(T_c, r_tref, e, eh, th) {
  T_k         <- T_c + 273.15
  boltz_act   <- (e  / K_B) * (1 / TREF_K - 1 / T_k)
  boltz_deact <- (eh / K_B) * (1 / th     - 1 / T_k)
  r_tref * exp(boltz_act) / (1 + exp(boltz_deact))
}

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

# ── Multistart bounds ─────────────────────────────────────────────────────────
BOUNDS_LO <- c(r_tref = 0.01, e = 0.01, eh =  0.5, th = 303.0)
BOUNDS_HI <- c(r_tref = 50.0, e =  3.0, eh = 50.0, th = 335.0)
START_LO  <- c(r_tref =  0.5, e =  0.1, eh =  0.5, th = 305.0)
START_HI  <- c(r_tref = 20.0, e =  2.0, eh = 20.0, th = 325.0)

T_pred <- seq(15, 50, length.out = 300)

fit_params_ssh <- list()
fit_preds_ssh  <- list()

for (sid in unique(mu_mean$strain)) {
  d     <- mu_mean %>% filter(strain == sid)
  evo   <- d$evo_history[1]
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


tpc_params |>
  filter(evo_history != "Other") |>
  ggplot(aes(x = evo_history, y = ctmax)) + geom_point()

library(plotrix)
tpc_params_sum <- tpc_params |>
  group_by(evo_history) |>
  summarise(mean_tmax = mean(ctmax),
            se_tmax = std.error(ctmax))



tpc_params |>
  ggplot(aes(x = evo_history, y = ctmax)) + geom_point() +
  geom_pointrange(aes(x = evo_history, y = mean_tmax, ymin = mean_tmax - se_tmax, ymax = mean_tmax + se_tmax), data = tpc_params_sum, color = "blue")
ggsave(file.path(OUT, "tmax-means.png"), width = 6, height = 4)



# Flag poor fits (R² < 0.9)
bad_strains      <- tpc_params %>% filter(r2 < 0.9) %>% pull(strain)
tpc_params_clean <- tpc_params %>% filter(!strain %in% bad_strains)

cat(sprintf("Fitted:              %d strains\n", nrow(tpc_params)))
cat(sprintf("Poor fits (R²<0.9): %s\n",
            if (length(bad_strains)) paste(bad_strains, collapse = ", ") else "none"))
cat(sprintf("Retained:           %d strains\n", nrow(tpc_params_clean)))
cat("\nMean Topt and CTmax by group:\n")
print(
  tpc_params_clean %>%
    group_by(evo_history) %>%
    summarise(mean_topt  = round(mean(topt,  na.rm = TRUE), 2),
              mean_ctmax = round(mean(ctmax, na.rm = TRUE), 2))
)

# =============================================================================
# 6. SSH QC plot — per-strain fit grid
# =============================================================================
plot_ssh_grid <- function(strain_subset, filename, title_text) {
  n_strains <- nrow(strain_subset)
  ncols     <- 6
  nrows     <- ceiling(n_strains / ncols)

  plot_list <- list()
  for (i in seq_len(n_strains)) {
    row_i  <- strain_subset[i, ]
    sid    <- row_i$strain
    color  <- EVO_COLORS[row_i$evo_history]
    is_bad <- sid %in% bad_strains
    obs    <- mu_mean %>% filter(strain == sid)
    prd    <- tpc_preds %>% filter(strain == sid) %>%
      mutate(pred = pmax(pred, -2))

    lbl <- sprintf("%s\nR²=%.2f%s", sid, row_i$r2, if (is_bad) " ⚠" else "")

    p <- ggplot() +
      geom_line(data = prd, aes(x = temp, y = pred),
                color = color,
                linewidth = if (is_bad) 0.8 else 1.2,
                linetype  = if (is_bad) "dashed" else "solid") +
      geom_point(data = obs, aes(x = temp, y = mu),
                 color = color, size = 1.8, alpha = 0.9) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
      labs(title = lbl) +
      theme_bw(base_size = 7) +
      theme(plot.title = element_text(face = "bold", size = 6),
            axis.title = element_blank())

    plot_list[[i]] <- p
  }

  # Use patchwork if available, otherwise cowplot
  if (requireNamespace("patchwork", quietly = TRUE)) {
    library(patchwork)
    combined <- wrap_plots(plot_list, ncol = ncols) +
      plot_annotation(title = title_text,
                      theme = theme(plot.title = element_text(face = "bold",
                                                               size = 14)))
    ggsave(filename, combined,
           width = ncols * 3.2, height = nrows * 3, dpi = 150,
           limitsize = FALSE)
    cat(sprintf("Saved: %s\n", filename))
  } else {
    message("Install 'patchwork' for the SSH QC grid plot.")
  }
}

plot_ssh_grid(tpc_params,
              file.path(OUT, "fig_ssh_all_strains.png"),
              "Sharpe–Schoolfield High Fits — All Strains")

# =============================================================================
# 7. TPC overlay figure
# =============================================================================
LEGEND_LABELS <- c(
  "40°C Evolved" = "40°C",
  "35°C Evolved" = "35°C",
  "Ancestor"     = "Ancestor"
)

# Individual strain curves (clean only)
indiv_curves <- tpc_preds %>%
  filter(evo_history %in% PRIMARY,
         !strain %in% bad_strains) %>%
  mutate(pred = ifelse(pred < 0, NA_real_, pred))

# Group mean curves
mean_curves <- tpc_params_clean %>%
  filter(evo_history %in% PRIMARY) %>%
  group_by(evo_history) %>%
  summarise(r_tref = mean(r_tref), e = mean(e),
            eh = mean(eh), th = mean(th), .groups = "drop") %>%
  rowwise() %>%
  mutate(data = list(data.frame(
    temp = T_pred,
    pred = sharpeschoolhigh(T_pred, r_tref, e, eh, th)
  ))) %>%
  unnest(data)

p_tpc <- ggplot() +
  geom_line(data = indiv_curves,
            aes(x = temp, y = pred, group = strain, color = evo_history),
            alpha = 0.22, linewidth = 1.2) +
  geom_line(data = mean_curves,
            aes(x = temp, y = pred, color = evo_history),
            linewidth = 3.0) +
  geom_point(data = mu_mean %>% filter(evo_history %in% PRIMARY),
             aes(x = temp, y = mu, color = evo_history),
             size = 3, alpha = 0.55) +
  scale_color_manual(values = EVO_COLORS,
                     breaks = names(LEGEND_LABELS),
                     labels = unname(LEGEND_LABELS), name = NULL) +
  coord_cartesian(xlim = c(24, 50), ylim = c(0, 10)) +
  labs(x = "Temperature (°C)", y = "Growth Rate (day⁻¹)",
       title = "Thermal Performance Curves") +
  theme_bw(base_size = 18) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        axis.text  = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

ggsave(file.path(OUT, "fig1b_overlay_primary.png"), p_tpc,
       width = 10, height = 7, dpi = 300)
print(p_tpc)
cat("Saved: fig1b_overlay_primary.png\n")

# =============================================================================
# 8. Linear mixed-effects model
# =============================================================================
gr_lme <- growth_rates %>%
  filter(evo_history %in% c("35°C Evolved", "40°C Evolved", "Ancestor")) %>%
  mutate(
    temp_fac   = factor(as.character(temp)),
    evo_history = relevel(factor(evo_history), ref = "35°C Evolved"),
    temp_fac   = relevel(temp_fac, ref = "25")
  )

lme_model <- lmer(
  mu ~ evo_history * temp_fac + (1 | strain),
  data = gr_lme,
  REML = TRUE
)

wald_result <- Anova(lme_model, type = 3, test.statistic = "Chisq")
print(wald_result)

p_int <- wald_result["evo_history:temp_fac", "Pr(>Chisq)"]
cat(sprintf("\nInteraction p = %.2e\n", p_int))

# =============================================================================
# 9. Topt vs Thermal Performance Retention (TPR)
# =============================================================================
tpr <- mu_mean %>%
  filter(evo_history %in% PRIMARY,
         temp %in% c(38, 41),
         !strain %in% bad_strains) %>%
  group_by(strain, evo_history, temp) %>%
  summarise(mu = mean(mu, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = temp, values_from = mu,
              names_prefix = "growth_") %>%
  rename(growth_38C = growth_38, growth_41C = growth_41) %>%
  mutate(TPR = growth_41C / growth_38C)

scatter_df <- tpc_params_clean %>%
  filter(evo_history %in% PRIMARY) %>%
  select(strain, evo_history, topt) %>%
  inner_join(tpr %>% select(strain, TPR), by = "strain")

r_p <- cor.test(scatter_df$topt, scatter_df$TPR, method = "pearson")
cat(sprintf("Pearson r = %.3f, p = %.4f\n", r_p$estimate, r_p$p.value))

sig_label <- dplyr::case_when(
  r_p$p.value < 0.0001 ~ "****",
  r_p$p.value < 0.001  ~ "***",
  r_p$p.value < 0.01   ~ "**",
  r_p$p.value < 0.05   ~ "*",
  TRUE                  ~ "ns"
)
annot <- sprintf("r = %.2f, %s", r_p$estimate, sig_label)

p_tpr <- ggplot(scatter_df, aes(x = topt, y = TPR,
                                  color = evo_history)) +
  geom_smooth(method = "lm", se = FALSE,
              color = "grey60", linetype = "dashed", linewidth = 1.0,
              data = scatter_df) +
  geom_point(size = 3, alpha = 0.75) +
  annotate("text",
           x = min(scatter_df$topt) - 0.1,
           y = max(scatter_df$TPR, na.rm = TRUE) * 0.98,
           label = annot, hjust = 0,
           color = "grey40", size = 7, fontface = "bold") +
  scale_color_manual(values = EVO_COLORS,
                     breaks = names(LEGEND_LABELS),
                     labels = unname(LEGEND_LABELS), name = NULL) +
  labs(
    x = "Optimal Temperature (°C)",
    y = "Thermal Performance Retention\n(Growth at 41°C / Growth at 38°C)"
  ) +
  theme_bw(base_size = 16) +
  theme(legend.position = "right",
        legend.background = element_blank(),
        axis.text  = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

ggsave(file.path(OUT, "fig_topt_vs_tpr.png"), p_tpr,
       width = 8, height = 7, dpi = 300)
print(p_tpr)
cat("Saved: fig_topt_vs_tpr.png\n")

# =============================================================================
# 10. Export CSVs
# =============================================================================

# Per-strain Topt and CTmax
tpc_params_clean %>%
  filter(evo_history %in% PRIMARY) %>%
  select(strain, evo_history, topt, ctmax) %>%
  arrange(evo_history, strain) %>%
  rename(T_opt_C = topt, CT_max_C = ctmax) %>%
  write.csv(file.path(OUT, "prism_topt_ctmax_per_strain.csv"), row.names = FALSE)

# Growth rates at 41°C and 42°C per strain
prism_hot <- growth_rates %>%
  filter(evo_history %in% PRIMARY,
         temp %in% c(41, 42),
         !strain %in% bad_strains) %>%
  group_by(strain, evo_history, temp) %>%
  summarise(mu = mean(mu, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = temp, values_from = mu,
              names_prefix = "growth_") %>%
  rename(growth_41C = growth_41, growth_42C = growth_42)

write.csv(prism_hot,
          file.path(OUT, "prism_growth_41_42_per_strain.csv"),
          row.names = FALSE)

# Observed growth rates (temperature rows × strain columns)
mu_mean %>%
  filter(evo_history %in% PRIMARY) %>%
  group_by(strain, temp) %>%
  summarise(mu = mean(mu, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = strain, values_from = mu) %>%
  rename(Temperature_C = temp) %>%
  write.csv(file.path(OUT, "prism_observed_growth_rates.csv"), row.names = FALSE)

# Fitted TPC curves (temperature rows × strain columns)
tpc_preds %>%
  filter(evo_history %in% PRIMARY,
         !strain %in% bad_strains) %>%
  pivot_wider(id_cols = temp, names_from = strain, values_from = pred) %>%
  write.csv(file.path(OUT, "prism_tpc_curves.csv"), row.names = FALSE)

# Thermal Performance Retention per strain
tpr %>%
  select(strain, evo_history, TPR) %>%
  write.csv(file.path(OUT, "prism_thermal_performance_retention.csv"),
            row.names = FALSE)

# Full SSH parameters per strain
write.csv(tpc_params,
          file.path(OUT, "tpc_params_ssh.csv"),
          row.names = FALSE)

cat(sprintf("\nExported to %s/\n", OUT))
for (f in list.files(OUT, full.names = FALSE)) {
  cat(sprintf("  %s\n", f))
}
