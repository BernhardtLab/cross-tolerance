# =============================================================================
# N. glabratus Thermal Performance Curve Analysis — Sliding Window Edition
#
# Growth rates estimated using the same variable-length sliding window approach
# as 02b-growth-rates-sliding-window-variable.R (≥ 5 points, R² ≥ 0.97),
# so results are directly comparable to that script.
#
# Created to diagnose differences between nglabratus_tpc_analysis.R and
# 02b-growth-rates-sliding-window-variable.R growth rate estimates.
#
# Input:  data-processed/all-blocks-tpc-experiment.csv
# Output: R-Script-Drafts/TPC-Analysis/Processed_Outputs_v2/
# =============================================================================

# ── Libraries ─────────────────────────────────────────────────────────────────
library(tidyverse)
library(cowplot)
library(minpack.lm)   # nlsLM for Sharpe-Schoolfield fitting

theme_set(theme_cowplot())

# ── Configuration ─────────────────────────────────────────────────────────────
OUT       <- "R-Script-Drafts/TPC-Analysis/Processed_Outputs_v2"
DIAG_DIR  <- file.path(OUT, "diagnostic-plots")
dir.create(DIAG_DIR, recursive = TRUE, showWarnings = FALSE)

K_B       <- 8.617333e-5   # Boltzmann constant (eV K⁻¹)
TREF_K    <- 288.15        # Reference temperature (15°C in Kelvin)
CTMAX_CAP <- 50.0
N_STARTS  <- 500
set.seed(42)

MIN_WINDOW   <- 5L

EVO_COLORS <- c(
  "40 evolved" = "#FA3208",
  "35 evolved" = "#0E63FF",
  "fRS585"     = "#000000"
)

PRIMARY <- c("40 evolved", "35 evolved", "fRS585")

# =============================================================================
# 1. Read raw OD data
# =============================================================================

all_blocks <- read_csv("data-processed/all-blocks-tpc-experiment.csv",
                       show_col_types = FALSE) %>%
  filter(od > 0) |> 
  filter(evolution_history %in% PRIMARY)

cat(sprintf("Loaded %s rows across %d wells\n",
            format(nrow(all_blocks), big.mark = ","),
            n_distinct(paste(all_blocks$well, all_blocks$block,
                             all_blocks$test_temperature))))

# =============================================================================
# 2. Sliding window growth rate estimation
#    (mirrors 02b-growth-rates-sliding-window-variable.R exactly)
# =============================================================================

max_growth_in_series <- function(df) {

  df    <- arrange(df, days)
  n     <- nrow(df)
  ln_OD <- log(pmax(df$od, 1e-6))

  best_mu    <- NA_real_
  best_r2    <- NA_real_
  best_start <- NA_integer_
  best_end   <- NA_integer_

  # Direct translation of the fallback in nglabratus_tpc_analysis.R.
  # time_h replaced by days — slope is already day⁻¹, no * 24 needed.
  for (start in seq_len(n - 4)) {
    for (end in (start + 4):n) {
      x <- df$days[start:end]
      y <- ln_OD[start:end]
      if (diff(range(x)) == 0) next
      fit_lm <- lm(y ~ x)
      slope  <- coef(fit_lm)[["x"]]
      r2_sw  <- summary(fit_lm)$r.squared
      if (is.na(best_mu) || slope > best_mu) {
        best_mu    <- slope   # already day⁻¹ (days, not hours)
        best_r2    <- r2_sw
        best_start <- start
        best_end   <- end
      }
    }
  }

  tibble(
    mu          = best_mu,
    r_squared   = best_r2,
    days_start  = if (!is.na(best_start)) df$days[best_start] else NA_real_,
    days_end    = if (!is.na(best_end))   df$days[best_end]   else NA_real_,
    window_size = if (!is.na(best_start)) best_end - best_start + 1L else NA_integer_,
    window_idx  = if (!is.na(best_start)) best_start else NA_integer_
  )
}

# =============================================================================
# 3. Apply across all wells
# =============================================================================

growth_summary <- all_blocks %>%
  group_by(well, block, test_temperature, strain, evolution_history) %>%
  group_modify(~ max_growth_in_series(.x)) %>%
  ungroup()

write_csv(growth_summary,
          file.path(OUT, "growth-rates-sliding-window.csv"))

cat(sprintf("Growth rates estimated: %d well×temp combinations\n",
            nrow(growth_summary)))

# =============================================================================
# 4. Diagnostic plots — best window overlaid on ln(OD) time series
# =============================================================================

plot_data <- all_blocks %>%
  left_join(
    growth_summary %>%
      select(well, block, test_temperature, days_start, days_end,
             mu, r_squared, window_size),
    by = c("well", "block", "test_temperature")
  ) %>%
  mutate(in_best_window = days >= days_start & days <= days_end)

series_keys <- plot_data %>%
  distinct(well, block, test_temperature, strain, evolution_history)

walk(seq_len(nrow(series_keys)), function(i) {

  w    <- series_keys$well[i]
  b    <- series_keys$block[i]
  temp <- series_keys$test_temperature[i]
  s    <- series_keys$strain[i]
  evo  <- series_keys$evolution_history[i]

  pd <- filter(plot_data,
               well == w, block == b, test_temperature == temp)

  best_df <- filter(pd, in_best_window)
  if (nrow(best_df) < 2) return(NULL)

  mu_val  <- unique(pd$mu)
  r2_val  <- unique(pd$r_squared)
  n_val   <- unique(pd$window_size)

  label_text <- paste0(
    "μ = ", round(mu_val, 3), " day⁻¹\n",
    "R² = ", round(r2_val, 3), "\n",
    "n = ", n_val, " points"
  )

  dot_color <- if (evo %in% names(EVO_COLORS)) EVO_COLORS[[evo]] else "gray40"

  p <- ggplot(pd, aes(x = days, y = log(od))) +
    geom_point(color = "gray70", size = 1.8, alpha = 0.7) +
    geom_point(data = best_df, color = dot_color, size = 2.5) +
    geom_smooth(
      data    = best_df,
      method  = "lm", formula = y ~ x,
      se      = FALSE,
      color   = dot_color,
      linewidth = 1.1
    ) +
    annotate(
      "text",
      x = -Inf, y = Inf,
      label = label_text,
      hjust = -0.1, vjust = 1.2,
      size  = 3.2
    ) +
    labs(
      title = paste0(s, "  |  ", w, "  |  block ", b, "  |  ", temp, "°C"),
      x     = "Days",
      y     = "ln(OD)"
    )

  fname <- file.path(
    DIAG_DIR,
    paste0(s, "_", w, "_block", b, "_", temp, "C.png")
  )
  ggsave(fname, p, width = 6, height = 4)
})

cat(sprintf("Diagnostic plots saved to %s/\n", DIAG_DIR))

# =============================================================================
# 5. Summary plot: growth rate vs temperature by evolutionary history
# =============================================================================

growth_summary %>%
  filter(evolution_history %in% PRIMARY) %>%
  ggplot(aes(x = test_temperature, y = mu, color = evolution_history)) +
  geom_point(alpha = 0.65, size = 2) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    title = "Growth rates — sliding window (≥ 5 pts, no R² filter)",
    x     = "Temperature (°C)",
    y     = "Max growth rate (day⁻¹)"
  )

ggsave(file.path(OUT, "mu-temp-sliding-window.png"), width = 8, height = 5)

# =============================================================================
# 6. Average across replicate wells → one value per strain × block × temperature
# =============================================================================

mu_mean <- growth_summary %>%
  group_by(strain, evolution_history, block, test_temperature) %>%
  summarise(
    mu      = mean(mu, na.rm = TRUE),
    n_wells = sum(!is.na(mu)),
    .groups = "drop"
  )

cat(sprintf("mu_mean rows: %d\n", nrow(mu_mean)))

# =============================================================================
# 7. Sharpe–Schoolfield High TPC fitting
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

  pa           <- preds
  pa[T_range <= topt] <- rmax
  sign_changes <- which(diff(sign(pa)) != 0)

  if (length(sign_changes) > 0) {
    i  <- sign_changes[1]
    t1 <- T_range[i];     t2 <- T_range[i + 1]
    p1 <- pa[i];          p2 <- pa[i + 1]
    ctmax <- if (p2 != p1) t1 - p1 * (t2 - t1) / (p2 - p1) else t1
  } else {
    below <- which(T_range > topt & preds < 0.05 * rmax)
    ctmax <- if (length(below) > 0) T_range[below[1]] else CTMAX_CAP
  }

  c(topt = topt, ctmax = ctmax, rmax = rmax)
}

BOUNDS_LO <- c(r_tref = 0.01, e = 0.01, eh =  0.5, th = 303.0)
BOUNDS_HI <- c(r_tref = 50.0, e =  3.0, eh = 50.0, th = 335.0)
START_LO  <- c(r_tref =  0.5, e =  0.1, eh =  0.5, th = 305.0)
START_HI  <- c(r_tref = 20.0, e =  2.0, eh = 20.0, th = 325.0)

T_pred <- seq(15, 50, length.out = 300)

fit_params_ssh <- list()
fit_preds_ssh  <- list()

for (sid in unique(mu_mean$strain)) {
  d     <- filter(mu_mean, strain == sid)
  evo   <- d$evolution_history[1]
  temps <- as.numeric(d$test_temperature)
  rates <- as.numeric(d$mu)

  valid_idx <- !is.na(rates)
  if (sum(valid_idx) < 3) {
    cat(sprintf("  SKIPPED (too few points): %s\n", sid))
    next
  }
  temps <- temps[valid_idx]
  rates <- rates[valid_idx]

  best_popt <- NULL
  best_rss  <- Inf

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
        start   = list(r_tref = p0[1], e = p0[2], eh = p0[3], th = p0[4]),
        lower   = BOUNDS_LO,
        upper   = BOUNDS_HI,
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
    strain         = sid,
    evolution_history = evo,
    r_tref         = best_popt["r_tref"],
    e              = best_popt["e"],
    eh             = best_popt["eh"],
    th             = best_popt["th"],
    topt           = topt,
    ctmax          = ctmax,
    rmax           = rmax,
    r2             = r2,
    row.names      = NULL
  )

  fit_preds_ssh[[length(fit_preds_ssh) + 1]] <- data.frame(
    strain         = sid,
    evolution_history = evo,
    temp           = T_pred,
    pred           = sharpeschoolhigh(T_pred,
                                      best_popt["r_tref"], best_popt["e"],
                                      best_popt["eh"],     best_popt["th"])
  )
}

tpc_params <- bind_rows(fit_params_ssh)
tpc_preds  <- bind_rows(fit_preds_ssh)

cat(sprintf("Fitted: %d strains\n", nrow(tpc_params)))

bad_strains      <- tpc_params %>% filter(r2 < 0.9) %>% pull(strain)
tpc_params_clean <- tpc_params %>% filter(!strain %in% bad_strains)

cat(sprintf("Poor fits (R²<0.9): %s\n",
            if (length(bad_strains)) paste(bad_strains, collapse = ", ") else "none"))
cat(sprintf("Retained: %d strains\n", nrow(tpc_params_clean)))

print(
  tpc_params_clean %>%
    filter(evolution_history %in% PRIMARY) %>%
    group_by(evolution_history) %>%
    summarise(mean_topt  = round(mean(topt,  na.rm = TRUE), 2),
              mean_ctmax = round(mean(ctmax, na.rm = TRUE), 2))
)

# =============================================================================
# 8. TPC overlay figure
# =============================================================================

indiv_curves <- tpc_preds %>%
  filter(evolution_history %in% PRIMARY,
         !strain %in% bad_strains) %>%
  mutate(pred = ifelse(pred < 0, NA_real_, pred))

mean_curves <- tpc_params_clean %>%
  filter(evolution_history %in% PRIMARY) %>%
  group_by(evolution_history) %>%
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
            aes(x = temp, y = pred, group = strain, color = evolution_history),
            alpha = 0.22, linewidth = 1.2) +
  geom_line(data = mean_curves,
            aes(x = temp, y = pred, color = evolution_history),
            linewidth = 3.0) +
  geom_point(data = mu_mean %>% filter(evolution_history %in% PRIMARY),
             aes(x = test_temperature, y = mu, color = evolution_history),
             size = 3, alpha = 0.55) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  coord_cartesian(xlim = c(24, 50), ylim = c(0, 10)) +
  labs(x = "Temperature (°C)", y = "Growth Rate (day⁻¹)",
       title = "Thermal Performance Curves — sliding window (no R² filter)") +
  theme_bw(base_size = 18) +
  theme(legend.position    = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.background  = element_blank(),
        axis.text          = element_text(face = "bold"),
        axis.title         = element_text(face = "bold"),
        plot.title         = element_text(face = "bold"))

ggsave(file.path(OUT, "tpc-overlay.png"), p_tpc,
       width = 10, height = 7, dpi = 300)
print(p_tpc)
cat("Saved: tpc-overlay.png\n")

# =============================================================================
# 9. Export CSVs
# =============================================================================

tpc_params_clean %>%
  filter(evolution_history %in% PRIMARY) %>%
  select(strain, evolution_history, topt, ctmax) %>%
  arrange(evolution_history, strain) %>%
  rename(T_opt_C = topt, CT_max_C = ctmax) %>%
  write_csv(file.path(OUT, "topt-ctmax-per-strain.csv"))

write_csv(tpc_params,  file.path(OUT, "tpc-params-ssh.csv"))
write_csv(growth_summary, file.path(OUT, "growth-rates-sliding-window.csv"))

cat(sprintf("\nAll outputs saved to %s/\n", OUT))
for (f in list.files(OUT, full.names = FALSE, recursive = FALSE)) {
  cat(sprintf("  %s\n", f))
}
