library(tidyverse)
library(cowplot)
library(minpack.lm)   # for nlsLM
library(broom)        # for tidy model output
library(ggrepel)
theme_set(theme_cowplot())

edata <- read_csv("data-processed/all-blocks-tpc-experiment.csv")

# ── Quick look ──────────────────────────────────────────────────────────────
edata |>
  ggplot(aes(x = time_days, y = RFU, group = rep.id, color = rep.id)) +
  facet_wrap( ~ treatment) +
  geom_line() + geom_point() +
  labs(title = "Raw RFU trajectories")


# ══════════════════════════════════════════════════════════════════════════════
# APPROACH 1: Logistic growth model (recommended)
#
# Model: RFU(t) = K / (1 + ((K - N0) / N0) * exp(-r * t))
#
# Parameters:
#   r  = intrinsic growth rate (per day) — equivalent to exponential growth
#        rate during the early phase
#   K  = carrying capacity (max RFU)
#   N0 = initial RFU
#
# Why logistic? It uses ALL time points (not just the exponential phase),
# so estimates are more stable and you don't need to manually pick a cutoff.
# The 'r' parameter IS the exponential growth rate.
# ══════════════════════════════════════════════════════════════════════════════

# Helper: starting values guessed from data (critical for nlsLM convergence)
get_start_vals_logistic <- function(df) {
  list(
    K  = max(df$RFU, na.rm = TRUE),
    N0 = min(df$RFU[df$time_days == min(df$time_days)], na.rm = TRUE),
    r  = 0.5   # moderate starting guess; adjust if many wells fail to converge
  )
}

# Fit logistic to one well, return tidy tibble (or NULL on failure)
fit_logistic <- function(df) {
  start <- get_start_vals_logistic(df)
  tryCatch(
    {
      fit <- nlsLM(
        RFU ~ K / (1 + ((K - N0) / N0) * exp(-r * time_days)),
        data  = df,
        start = start,
        lower = c(K = 0, N0 = 0, r = 0),   # enforce biologically sensible bounds
        control = nls.lm.control(maxiter = 200)
      )
      tidy(fit, conf.int = TRUE) |>
        mutate(rep.id = unique(df$rep.id),
               converged = TRUE,
               AIC = AIC(fit))
    },
    error = function(e) {
      tibble(rep.id = unique(df$rep.id), converged = FALSE,
             term = NA, estimate = NA, std.error = NA,
             conf.low = NA, conf.high = NA)
    }
  )
}

# Fit to every well
growth_logistic <- edata |>
  group_by(rep.id) |>
  group_modify(~ fit_logistic(.x)) |>
  ungroup()

# Extract just the growth rate 'r'
growth_rates_logistic <- growth_logistic |>
  filter(term == "r") |>
  select(rep.id, r = estimate, r_se = std.error,
         r_conf.low = conf.low, r_conf.high = conf.high,
         converged, AIC)

print(growth_rates_logistic)


# ══════════════════════════════════════════════════════════════════════════════
# APPROACH 2: Pure exponential, restricted to the exponential phase
#
# Model: RFU(t) = N0 * exp(r * t)
#
# The exponential phase ends at the INFLECTION POINT of the growth curve —
# the moment of maximum absolute growth rate (dRFU/dt). Before this point
# the culture is growing approximately exponentially; after it, per-capita
# growth rate is already declining due to resource limitation.
#
# Two methods to find the inflection point per well:
#
#   A) Numerical: estimate dRFU/dt by finite differences; inflection is at
#      the time step with the largest increase. Self-contained but sensitive
#      to noise in sparse/noisy data.
#
#   B) Model-based: derive inflection time analytically from the logistic fit
#      (t_inf = log((K - N0) / N0) / r). More robust, but requires Approach 1
#      to have converged for that well.
#
# We use B where available, fall back to A otherwise.
# ══════════════════════════════════════════════════════════════════════════════

# --- Method A: numerical inflection point (maximum of finite-difference dRFU/dt) ---
inflection_numerical <- edata |>
  group_by(rep.id) |>
  arrange(time_days) |>
  mutate(
    dRFU_dt = c(NA_real_, diff(RFU) / diff(time_days))
  ) |>
  summarise(
    # time point at which dRFU/dt is highest — this IS the inflection point
    t_inflection_numerical = time_days[which.max(replace_na(dRFU_dt, -Inf))],
    .groups = "drop"
  )

# --- Method B: model-based inflection point from logistic fit ---
# For a logistic N(t) = K / (1 + ((K-N0)/N0)*exp(-r*t)),
# the inflection is at t_inf = log((K - N0) / N0) / r
inflection_model <- growth_logistic |>
  filter(converged, term %in% c("K", "N0", "r")) |>
  select(rep.id, term, estimate) |>
  pivot_wider(names_from = term, values_from = estimate) |>
  mutate(t_inflection_model = log((K - N0) / N0) / r) |>
  select(rep.id, t_inflection_model)

# --- Combine: prefer model-based, fall back to numerical ---
inflection_times <- inflection_numerical |>
  left_join(inflection_model, by = "rep.id") |>
  mutate(
    t_inflection = coalesce(t_inflection_model, t_inflection_numerical)
  )

# Trim each well to its exponential phase (time points up to inflection)
edata_exp_phase <- edata |>
  left_join(inflection_times |> select(rep.id, t_inflection), by = "rep.id") |>
  group_by(rep.id) |>
  filter(time_days <= t_inflection) |>
  ungroup()

# Fit pure exponential to one well
fit_exponential <- function(df) {
  start <- list(
    N0 = min(df$RFU[df$time_days == min(df$time_days)], na.rm = TRUE),
    r  = 0.5
  )
  tryCatch(
    {
      fit <- nlsLM(
        RFU ~ N0 * exp(r * time_days),
        data  = df,
        start = start,
        lower = c(N0 = 0, r = 0),
        control = nls.lm.control(maxiter = 200)
      )
      tidy(fit, conf.int = TRUE) |>
        mutate(rep.id = unique(df$rep.id),
               converged = TRUE,
               n_points  = nrow(df),
               AIC = AIC(fit))
    },
    error = function(e) {
      tibble(rep.id = unique(df$rep.id), converged = FALSE,
             term = NA, estimate = NA, std.error = NA,
             conf.low = NA, conf.high = NA,
             n_points = nrow(df))
    }
  )
}

# Sanity check: plot inflection points on raw data
edata |>
  left_join(inflection_times, by = "rep.id") |>
  ggplot(aes(x = time_days, y = RFU, group = rep.id)) +
  geom_line(alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_vline(aes(xintercept = t_inflection), linetype = "dashed", color = "firebrick", alpha = 0.7) +
  facet_wrap(~ rep.id, scales = "free_y") +
  labs(title = "Detected inflection points (dashed = end of exponential phase)",
       x = "Time (days)", y = "RFU")

growth_exponential <- edata_exp_phase |>
  group_by(rep.id) |>
  group_modify(~ fit_exponential(.x)) |>
  ungroup()

growth_rates_exponential <- growth_exponential |>
  filter(term == "r") |>
  select(rep.id, r = estimate, r_se = std.error,
         r_conf.low = conf.low, r_conf.high = conf.high,
         converged, n_points)

print(growth_rates_exponential)


# ══════════════════════════════════════════════════════════════════════════════
# DIAGNOSTICS: faceted plot of both model fits overlaid on raw data
# ══════════════════════════════════════════════════════════════════════════════

# --- Logistic predictions: full time range per well ---
pred_logistic <- growth_logistic |>
  filter(converged, !is.na(term)) |>
  select(rep.id, term, estimate) |>
  pivot_wider(names_from = term, values_from = estimate) |>
  # generate a dense time sequence within each well's observed range
  left_join(
    edata |> group_by(rep.id) |>
      summarise(t_min = min(time_days), t_max = max(time_days), .groups = "drop"),
    by = "rep.id"
  ) |>
  rowwise() |>
  mutate(time_days = list(seq(t_min, t_max, length.out = 200))) |>
  unnest(time_days) |>
  mutate(
    RFU_pred = K / (1 + ((K - N0) / N0) * exp(-r * time_days)),
    model = "Logistic"
  ) |>
  select(rep.id, time_days, RFU_pred, model)

# --- Exponential predictions: exponential phase only (up to inflection point) ---
pred_exponential <- growth_exponential |>
  filter(converged, !is.na(term)) |>
  select(rep.id, term, estimate) |>
  pivot_wider(names_from = term, values_from = estimate) |>
  left_join(
    # use the inflection time as the upper bound of the prediction range
    inflection_times |> select(rep.id, t_inflection),
    by = "rep.id"
  ) |>
  left_join(
    edata |> group_by(rep.id) |> summarise(t_min = min(time_days), .groups = "drop"),
    by = "rep.id"
  ) |>
  rowwise() |>
  mutate(time_days = list(seq(t_min, t_inflection, length.out = 200))) |>
  unnest(time_days) |>
  mutate(
    RFU_pred = N0 * exp(r * time_days),
    model = "Exponential (phase only)"
  ) |>
  select(rep.id, time_days, RFU_pred, model)

# --- Combine predictions ---
pred_both <- bind_rows(pred_logistic, pred_exponential)

# --- Faceted plot ---
ggplot() +
  # raw data
  geom_point(data = edata,
             aes(x = time_days, y = RFU),
             size = 1, alpha = 0.6, color = "grey40") +
  geom_line(data = edata,
            aes(x = time_days, y = RFU),
            alpha = 0.4, color = "grey40") +
  # model fits
  geom_line(data = pred_both,
            aes(x = time_days, y = RFU_pred, color = model),
            linewidth = 0.9) +
  # inflection point marker
  geom_vline(data = inflection_times,
             aes(xintercept = t_inflection),
             linetype = "dashed", color = "grey60", linewidth = 0.5) +
  facet_wrap(~ rep.id, scales = "free_y") +
  scale_color_manual(values = c("Logistic" = "#2166ac",
                                "Exponential (phase only)" = "#d6604d")) +
  labs(
    title = "Model fits by well",
    subtitle = "Dashed line = inflection point (end of exponential phase)",
    x = "Time (days)", y = "RFU", color = NULL
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 7)
  )

ggsave("figures/exponential-logistic-plate1.png", width = 15, height = 10)
# ══════════════════════════════════════════════════════════════════════════════
# COMPARE the two approaches
# ══════════════════════════════════════════════════════════════════════════════

comparison <- growth_rates_logistic |>
  select(rep.id, r_logistic = r, converged_logistic = converged) |>
  left_join(
    growth_rates_exponential |>
      select(rep.id, r_exponential = r, converged_exp = converged, n_points),
    by = "rep.id"
  )

comparison |>
  ggplot(aes(x = r_logistic, y = r_exponential, label = rep.id)) +
  geom_abline(linetype = "dashed", color = "grey60") +
  geom_point() +
  ggrepel::geom_text_repel(size = 3) +
  labs(title = "Logistic vs exponential-phase estimates",
       x = "r from logistic model", y = "r from exponential phase only")
ggsave("figures/comparison-logistic-exp5.png")

# Save results
write_csv(growth_rates_logistic,    "data-processed/growth-rates-logistic.csv")
write_csv(growth_rates_exponential, "data-processed/growth-rates-exponential.csv")