library(tidyverse)
library(cowplot)
library(minpack.lm)   # for nlsLM
library(broom)        # for tidy model output
library(ggrepel)
theme_set(theme_cowplot())

edata <- read_csv("data-processed/all-blocks-tpc-experiment.csv") |> 
  filter(evolution_history %in% c("35 evolved", "40 evolved", "fRS585")) |>
  mutate(rep.id = paste(well, block, test_temperature, strain, evolution_history, sep = "_")) |> 
  rename(RFU = od,
         time_days = days)

# ── Quick look ──────────────────────────────────────────────────────────────
edata |>
  ggplot(aes(x = time_days, y = RFU, group = rep.id, color = evolution_history)) +
  facet_wrap( ~ test_temperature) +
   geom_line() +
  labs(title = "Raw OD trajectories")


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
      tidy(fit) |>
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

# --- Combine: prefer model-based (if valid), fall back to numerical ---
inflection_times <- inflection_numerical |>
  left_join(inflection_model, by = "rep.id") |>
  mutate(
    # Use model-based inflection only if it's positive; otherwise use numerical
    t_inflection = if_else(is.na(t_inflection_model) | t_inflection_model < 0,
                           t_inflection_numerical,
                           t_inflection_model)
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
      tidy(fit) |>
        mutate(rep.id = unique(df$rep.id),
               converged = TRUE,
               n_points  = nrow(df),
               AIC = AIC(fit))
    },
    error = function(e) {
      tibble(rep.id = unique(df$rep.id), converged = FALSE,
             term = NA, estimate = NA, std.error = NA,
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

length(unique(edata_exp_phase$rep.id))


growth_exponential <- edata_exp_phase |>
  group_by(rep.id) |>
  group_modify(~ fit_exponential(.x)) |>
  ungroup()

growth_rates_exponential <- growth_exponential |>
  filter(term == "r") |>
  select(rep.id, r = estimate, r_se = std.error,
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

# --- Individual plots for each well ---
dir.create("figures/exp-log", showWarnings = FALSE, recursive = TRUE)

unique_wells <- as.vector(unique(edata$rep.id), mode = "character")



for (well in unique_wells) {
  
  p <- ggplot() +
    # raw data
    geom_point(data = edata |> filter(rep.id == {{ well }}) |> ungroup(),
               aes(x = time_days, y = RFU),
               size = 1, alpha = 0.6, color = "grey40") +
    geom_line(data = edata |> filter(rep.id == {{ well }}) |> ungroup(),
              aes(x = time_days, y = RFU),
              alpha = 0.4, color = "grey40") +
    # model fits
    geom_line(data = pred_both |> filter(rep.id == {{ well }}) |> ungroup(),
              aes(x = time_days, y = RFU_pred, color = model),
              linewidth = 0.9) +
    # inflection point marker
    geom_vline(data = inflection_times |> filter(rep.id == {{ well }}) |> ungroup(),
               aes(xintercept = t_inflection),
               linetype = "dashed", color = "grey60", linewidth = 0.5) +
    scale_color_manual(values = c("Logistic" = "#2166ac",
                                  "Exponential (phase only)" = "#d6604d")) +
    labs(
      title = paste("Model fits -", well),
      subtitle = "Dashed line = inflection point (end of exponential phase)",
      x = "Time (days)", y = "RFU", color = NULL
    ) +
    theme(
      legend.position = "bottom"
    )
  
  ggsave(glue::glue("figures/exp-log/well-{well}.png"), 
         plot = p, width = 8, height = 6)
}

rm(well, p, well_data, unique_wells)
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


growth_logistic |> 
  filter(term == "r") |> 
  View()

growth_exponential |> 
  filter(term == "r") |> 
  View()


# ══════════════════════════════════════════════════════════════════════════════
# APPROACH 3: Pure exponential for 41°C and 42°C, time < 0.25 days
# ══════════════════════════════════════════════════════════════════════════════
# Focus on early exponential phase (before saturation effects) for the
# highest temperature experiments

edata_early_exp <- edata |>
  filter(test_temperature %in% c(41, 42)) |>
  filter(time_days < 0.25)

# Fit pure exponential to one well
fit_exponential_early <- function(df) {
  if (nrow(df) < 2) {
    return(tibble(rep.id = unique(df$rep.id), converged = FALSE,
                  term = NA, estimate = NA, std.error = NA,
                  n_points = nrow(df)))
  }
  
  start <- list(
    N0 = min(df$RFU[df$time_days == min(df$time_days)], na.rm = TRUE),
    r  = 1.0  # expect higher growth rates at extreme temperatures
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
      tidy(fit) |>
        mutate(rep.id = unique(df$rep.id),
               converged = TRUE,
               n_points  = nrow(df),
               AIC = AIC(fit))
    },
    error = function(e) {
      tibble(rep.id = unique(df$rep.id), converged = FALSE,
             term = NA, estimate = NA, std.error = NA,
             n_points = nrow(df))
    }
  )
}

# Fit to every well in the early phase
growth_exponential_early <- edata_early_exp |>
  group_by(rep.id) |>
  group_modify(~ fit_exponential_early(.x)) |>
  ungroup()

growth_rates_exponential_early <- growth_exponential_early |>
  filter(term == "r") |>
  select(rep.id, r = estimate, r_se = std.error,
         converged, n_points)

print("Growth rates for 41-42°C early exponential phase (t < 0.25 days):")
print(growth_rates_exponential_early)

# --- Generate predictions for early exponential fits ---
pred_exponential_early <- growth_exponential_early |>
  filter(converged, !is.na(term)) |>
  select(rep.id, term, estimate) |>
  pivot_wider(names_from = term, values_from = estimate) |>
  left_join(
    edata_early_exp |> group_by(rep.id) |> 
      summarise(t_min = min(time_days), t_max = max(time_days), .groups = "drop"),
    by = "rep.id"
  ) |>
  rowwise() |>
  mutate(time_days = list(seq(t_min, t_max, length.out = 100))) |>
  unnest(time_days) |>
  mutate(
    RFU_pred = N0 * exp(r * time_days),
    model = "Exponential (t < 0.25d)"
  ) |>
  select(rep.id, time_days, RFU_pred, model)

# --- Plot early exponential fits on raw data ---
dir.create("figures/exponential-early", showWarnings = FALSE, recursive = TRUE)

unique_early_wells <- as.vector(unique(edata_early_exp$rep.id), mode = "character")

for (well in unique_early_wells) {
  
  well_data <- edata |> filter(rep.id == {{ well }})
  well_preds <- pred_exponential_early |> filter(rep.id == {{ well }})
  
  # only plot if we have successful predictions
  if (nrow(well_preds) > 0) {
    p <- ggplot() +
      # full raw data trajectory for context
      geom_point(data = well_data,
                 aes(x = time_days, y = RFU),
                 size = 1.5, alpha = 0.5, color = "grey50") +
      geom_line(data = well_data,
                aes(x = time_days, y = RFU),
                alpha = 0.3, color = "grey50") +
      # early data points (t < 0.25d) highlighted
      geom_point(data = well_data |> filter(time_days < 0.25),
                 aes(x = time_days, y = RFU),
                 size = 2, alpha = 0.8, color = "#d6604d") +
      geom_line(data = well_data |> filter(time_days < 0.25),
                aes(x = time_days, y = RFU),
                alpha = 0.6, color = "#d6604d", linewidth = 0.8) +
      # exponential fit
      geom_line(data = well_preds,
                aes(x = time_days, y = RFU_pred, color = model),
                linewidth = 1) +
      scale_color_manual(values = c("Exponential (t < 0.25d)" = "#2166ac")) +
      labs(
        title = paste("Early exponential fit -", well),
        subtitle = "Red = time points used for fitting (t < 0.25d), Blue line = model prediction",
        x = "Time (days)", y = "RFU", color = NULL
      ) +
      theme(
        legend.position = "bottom"
      )
    
    ggsave(glue::glue("figures/exponential-early/well-{well}.png"), 
           plot = p, width = 8, height = 6)
  }
}

rm(well, p, well_data, well_preds, unique_early_wells)

# Save results
write_csv(growth_rates_exponential_early, 
          "data-processed/growth-rates-exponential-early-41-42C.csv")

