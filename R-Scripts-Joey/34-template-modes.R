# =============================================================================
# 34-template-modes.R
# How is variation among fitted TPCs structured? Template mode of variation
# (TMV) following Izem & Kingsolver (2005, Am. Nat. 166:277-289).
#
# Each strain's fitted curve is modelled as a shifted, rescaled template:
#   z_i(t) = (1 / w_i) * z((t - m_i) / w_i) + h_i
#   h_i: vertical shift (overall performance)
#   m_i: horizontal shift (optimum relative to the template optimum, degrees C)
#   w_i: generalist-specialist mode (larger = wider, more generalist)
#
# Two templates, fitted separately:
#   poly: z(s) = 1 + a2 u^2 + a3 u^3 + a4 u^4, u = s / 10 (degree-4 polynomial
#         with zero slope at s = 0, so the template peaks at 0 as in the paper)
#   ss:   Sharpe-Schoolfield shape, normalised so its own optimum is at s = 0
#
# Identifiability: z(0) = 1 is the template's maximum; mean(h) = 0 and
# mean(log w) = 0 fix the offsets and scale. The template optimum anchors m.
#
# Deviations from the paper:
#   - No Frechet mean or arc-length variance decomposition; per-strain
#     parameters are reported instead.
#   - Fit uses every 5th grid point (~0.35 C) for speed.
#
# Fit: all 37 curves (ancestor included), weighted by n_obs. Group comparisons
#   and drug associations use the 36 evolved strains.
#
# Inputs:  data-processed/gcplyr/tpc-predictions-auc-gcplyr-17.csv
#          data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv
#          data-processed/strain-drug-metrics-32.csv  (from Script 32)
# Outputs: data-processed/template-strain-params-34.csv
#          data-processed/template-group-tests-34.csv
#          data-processed/template-drug-associations-34.csv
# =============================================================================

library(tidyverse)

EVO_LEVELS <- c("35 evolved", "40 evolved")
OUT <- "data-processed"

K_EV <- 8.617333e-5  # Boltzmann constant (eV / K)


# =============================================================================
# 1. Data
# =============================================================================

tpc_pred <- read_csv(file.path(OUT, "gcplyr/tpc-predictions-auc-gcplyr-17.csv"), show_col_types = FALSE)
tpc_par  <- read_csv(file.path(OUT, "gcplyr/tpc-params-auc-gcplyr-17.csv"), show_col_types = FALSE)
drug_dat <- read_csv(file.path(OUT, "strain-drug-metrics-32.csv"), show_col_types = FALSE)

fit_dat <- tpc_pred |>
  arrange(strain, temp) |>
  # Every 5th grid point keeps the optimiser fast
  group_by(strain) |>
  filter(row_number() %% 5 == 1) |>
  ungroup() |>
  left_join(tpc_par |> select(strain, n_obs, topt, rmax), by = "strain") |>
  group_by(strain, evolution_history, n_obs, topt, rmax) |>
  summarise(t = list(temp), y = list(pred), .groups = "drop")

k  <- nrow(fit_dat)
wt <- fit_dat$n_obs / mean(fit_dat$n_obs)
stopifnot(k == 37, sum(fit_dat$evolution_history == "fRS585") == 1)


# =============================================================================
# 2. Templates and shape-invariant fit
# =============================================================================

# Polynomial template with its maximum at s = 0 (zero slope, value 1)
template_poly <- function(tp) {
  function(s) {
    u <- s / 10
    1 + tp[1] * u^2 + tp[2] * u^3 + tp[3] * u^4
  }
}

# Sharpe-Schoolfield shape (T in K), normalised to 1 at its own optimum
ss_raw <- function(T, tp) {
  exp(-tp[1] / (K_EV * T)) / (1 + exp(tp[2] / K_EV * (1 / tp[3] - 1 / T)))
}

T_GRID <- seq(275, 335, by = 0.05)

template_ss <- function(tp) {
  vals <- ss_raw(T_GRID, tp)
  T_opt <- T_GRID[which.max(vals)]
  function(s) ss_raw(T_opt + s, tp) / max(vals)
}

fit_template <- function(template_fn, tp_start, tp_lower, tp_upper) {
  n_tp <- length(tp_start)

  ymean <- vapply(fit_dat$y, mean, numeric(1))
  start <- c(tp_start,
             fit_dat$topt - mean(fit_dat$topt),
             rep(0, k),
             ymean - mean(ymean))
  lower <- c(tp_lower, rep(-Inf, 3 * k))
  upper <- c(tp_upper, rep(Inf, 3 * k))

  unpack <- function(par) {
    rest <- par[-seq_len(n_tp)]
    idx_l <- k + seq_len(k)
    idx_h <- 2 * k + seq_len(k)
    list(
      tp = par[seq_len(n_tp)],
      m  = rest[seq_len(k)],
      lw = rest[idx_l] - mean(rest[idx_l]),
      h  = rest[idx_h] - mean(rest[idx_h])
    )
  }

  sse <- function(par) {
    p  <- unpack(par)
    tf <- template_fn(p$tp)
    sum(vapply(seq_len(k), function(i) {
      w    <- exp(p$lw[i])
      pred <- tf((fit_dat$t[[i]] - p$m[i]) / w) / w + p$h[i]
      wt[i] * mean((fit_dat$y[[i]] - pred)^2)
    }, numeric(1)))
  }

  fit <- optim(start, sse, method = "L-BFGS-B", lower = lower, upper = upper,
               control = list(maxit = 300, factr = 1e7))
  if (fit$convergence != 0) warning("optim did not converge: ", fit$message)

  p  <- unpack(fit$par)
  tf <- template_fn(p$tp)

  strain_par <- fit_dat |>
    transmute(strain, evolution_history, n_obs, topt, rmax_obs = rmax) |>
    mutate(
      w = exp(p$lw),
      h = p$h,
      optimum = p$m,
      rmax_fit = tf(0) / w + h,
      r2 = vapply(seq_len(k), function(i) {
        w_i  <- exp(p$lw[i])
        pred <- tf((fit_dat$t[[i]] - p$m[i]) / w_i) / w_i + p$h[i]
        1 - sum((fit_dat$y[[i]] - pred)^2) / sum((fit_dat$y[[i]] - mean(fit_dat$y[[i]]))^2)
      }, numeric(1))
    ) |>
    select(strain, evolution_history, n_obs, topt, rmax_obs, w, h, optimum, rmax_fit, r2)

  list(
    template = tibble(s = seq(-40, 40, by = 0.5), z = tf(seq(-40, 40, by = 0.5))),
    params = strain_par,
    template_par = p$tp,
    sse = fit$value,
    convergence = fit$convergence
  )
}

fit_poly <- fit_template(template_poly, tp_start = c(-0.3, 0, 0),
                         tp_lower = rep(-Inf, 3), tp_upper = rep(Inf, 3))

fit_ss <- fit_template(template_ss, tp_start = c(0.2, 5, 313),
                       tp_lower = c(0.01, 0.5, 290), tp_upper = c(2, 100, 340))

fits <- list(poly = fit_poly, ss = fit_ss)

fit_summary <- tibble(
  template = names(fits),
  sse = map_dbl(fits, \(f) f$sse),
  convergence = map_int(fits, \(f) f$convergence),
  mean_r2 = map_dbl(fits, \(f) mean(f$params$r2)),
  min_r2 = map_dbl(fits, \(f) min(f$params$r2))
)
print(fit_summary)
print(map(fits, \(f) round(f$template_par, 4)))


# =============================================================================
# 3. Mode values per strain
# =============================================================================

strain_params <- map_dfr(fits, \(f) f$params, .id = "template")

write_csv(strain_params, file.path(OUT, "template-strain-params-34.csv"))

strain_params |>
  group_by(template, evolution_history) |>
  summarise(across(c(h, optimum, w, rmax_fit), list(mean = mean, sd = sd)), .groups = "drop") |>
  mutate(across(where(is.double), \(x) round(x, 3))) |>
  as.data.frame() |>
  print()


# =============================================================================
# 4. Evolution group differences in each mode (evolved strains only)
# =============================================================================

MODES <- c("h", "optimum", "w")

group_tests <- function(x35, x40) {
  if (sd(x35) < 1e-8 || sd(x40) < 1e-8) {
    return(tibble(mean_35 = mean(x35), mean_40 = mean(x40), diff_40_minus_35 = NA_real_,
                  ci_lo = NA_real_, ci_hi = NA_real_, p_welch = NA_real_, p_wilcox = NA_real_))
  }
  tt <- t.test(x35, x40)
  tibble(
    mean_35 = mean(x35), mean_40 = mean(x40),
    diff_40_minus_35 = mean(x40) - mean(x35),
    ci_lo = -tt$conf.int[2], ci_hi = -tt$conf.int[1],
    p_welch = tt$p.value,
    p_wilcox = wilcox.test(x35, x40)$p.value
  )
}

group_results <- strain_params |>
  filter(evolution_history %in% EVO_LEVELS) |>
  pivot_longer(all_of(MODES), names_to = "mode", values_to = "value") |>
  group_by(template, mode) |>
  group_modify(\(d, key) {
    group_tests(d$value[d$evolution_history == "35 evolved"],
                d$value[d$evolution_history == "40 evolved"])
  }) |>
  ungroup()

ancestor_values <- strain_params |>
  filter(evolution_history == "fRS585") |>
  select(template, all_of(MODES)) |>
  pivot_longer(all_of(MODES), names_to = "mode", values_to = "ancestor")

group_results <- group_results |>
  left_join(ancestor_values, by = c("template", "mode"))

write_csv(group_results, file.path(OUT, "template-group-tests-34.csv"))

print(group_results |> mutate(across(where(is.double), \(x) round(x, 3))) |> as.data.frame())


# =============================================================================
# 5. Mode values vs drug metrics (evolved strains, within-group partial r)
# =============================================================================

partial_r <- function(m, term) {
  s <- summary(m)$coefficients
  if (!term %in% rownames(s)) return(tibble(r_within = NA_real_, p = NA_real_, n = nobs(m)))
  tv <- s[term, "t value"]
  tibble(
    r_within = tv / sqrt(tv^2 + m$df.residual),
    p = s[term, "Pr(>|t|)"],
    n = nobs(m)
  )
}

assoc_dat <- strain_params |>
  filter(evolution_history %in% EVO_LEVELS) |>
  select(template, strain, evolution_history, all_of(MODES), rmax_fit) |>
  inner_join(drug_dat |> select(strain, drug, log2_ic50, high_drug_lfc),
             by = "strain", relationship = "many-to-many")

drug_assoc <- expand_grid(
  template    = names(fits),
  mode        = c(MODES, "rmax_fit"),
  drug        = c("fluconazole", "caspofungin", "amphotericin"),
  drug_metric = c("log2_ic50", "high_drug_lfc")
) |>
  mutate(res = pmap(list(template, mode, drug, drug_metric), \(tm, md, dg, y) {
    d <- assoc_dat |> filter(template == tm, drug == dg)
    partial_r(lm(reformulate(c(md, "evolution_history"), y), data = d), md)
  })) |>
  unnest(res)

write_csv(drug_assoc, file.path(OUT, "template-drug-associations-34.csv"))

drug_assoc |>
  mutate(cell = sprintf("%.2f (%.3f)", r_within, p)) |>
  select(template, mode, drug, drug_metric, cell) |>
  pivot_wider(names_from = drug_metric, values_from = cell) |>
  as.data.frame() |>
  print()


# =============================================================================
# 6. Correlations among modes and with r_max (all 37 curves, group-adjusted)
# =============================================================================

mode_cor <- map_dfr(names(fits), \(tm) {
  d <- strain_params |>
    filter(template == tm) |>
    mutate(evolution_history = factor(evolution_history))
  expand_grid(x = c("h", "optimum", "w"), y = c("h", "optimum", "w", "rmax_fit")) |>
    filter(x != y) |>
    mutate(template = tm, r_within = map2_dbl(x, y, \(a, b) {
      m <- lm(reformulate(c(b, "evolution_history"), a), data = d)
      cf <- summary(m)$coefficients
      if (!b %in% rownames(cf)) return(NA_real_)
      tv <- cf[b, "t value"]
      tv / sqrt(tv^2 + m$df.residual)
    }))
})

mode_cor |>
  mutate(across(where(is.double), \(x) round(x, 2))) |>
  as.data.frame() |>
  print()


# =============================================================================
# 7. Plot: polynomial template with each mode's observed range
# =============================================================================
# Each panel varies one mode across its observed min and max, others at template values.

FIGS <- "figures"

tf_poly <- template_poly(fit_poly$template_par)
mode_range <- strain_params |>
  filter(template == "poly") |>
  summarise(h_min = min(h), h_max = max(h),
            opt_min = min(optimum), opt_max = max(optimum),
            w_min = min(w), w_max = max(w))

# Restricted to +/-15 C: outside this range the polynomial template is extrapolated
s_plot <- seq(-15, 15, by = 0.1)
curve_df <- function(mode, end, z) tibble(mode = mode, end = end, s = s_plot, z = z)

plot_dat <- bind_rows(
  curve_df("Vertical shift (h)", "min", tf_poly(s_plot) + mode_range$h_min),
  curve_df("Vertical shift (h)", "max", tf_poly(s_plot) + mode_range$h_max),
  curve_df("Horizontal shift (optimum)", "min", tf_poly(s_plot - mode_range$opt_min)),
  curve_df("Horizontal shift (optimum)", "max", tf_poly(s_plot - mode_range$opt_max)),
  curve_df("Width (w)", "min", tf_poly(s_plot / mode_range$w_min) / mode_range$w_min),
  curve_df("Width (w)", "max", tf_poly(s_plot / mode_range$w_max) / mode_range$w_max)
) |>
  mutate(mode = factor(mode, levels = c("Vertical shift (h)", "Horizontal shift (optimum)", "Width (w)")))

template_df <- tibble(s = s_plot, z = tf_poly(s_plot))

ggplot(plot_dat, aes(x = s, y = z, colour = end, group = end)) +
  geom_line(data = template_df, aes(x = s, y = z), inherit.aes = FALSE,
            colour = "black", linetype = "dashed") +
  geom_line(linewidth = 0.8) +
  facet_wrap(~mode, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = c(min = "#2166AC", max = "#B2182B"), name = "Observed value") +
  labs(x = "Temperature relative to template optimum (°C)",
       y = "Relative performance (template units)",
       caption = paste(
         "Dashed line: polynomial template. Coloured lines: observed min and max of each mode,",
         "varying one mode at a time (others at template values).",
         sep = "\n")) +
  theme_classic(12) +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        plot.caption = element_text(size = 9, color = "grey40"))

ggsave(file.path(FIGS, "template-modes-34.png"), width = 11, height = 4.5, dpi = 300, bg = "white")
