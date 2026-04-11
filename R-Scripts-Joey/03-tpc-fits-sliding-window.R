### Fit Sharpe-Schoolfield thermal performance curves to sliding window growth rates
#
# Author: Joey Bernhardt
# Input:  data-processed/all-blocks-growth-sliding-window.csv
# Output: data-processed/all-SS-traits-sliding-window.csv
#         figures/tpcs-sliding-window-*.png
#
# Method: For each strain, fit the Sharpe-Schoolfield high temperature
#         inactivation model (sharpeschoolhigh_1981) to mu ~ temperature
#         using nls_multstart. Each well × block observation is treated as
#         an individual data point (i.e. replicates are not averaged before
#         fitting, so the curve is fit to all raw replicate estimates).


# packages ----------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(rTPC)
library(nls.multstart)
library(broom)
library(plotrix)
library(conflicted)

theme_set(theme_cowplot())

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


# read data ---------------------------------------------------------------

# wells excluded due to spurious OD measurements identified via diagnostic plots
excluded_series <- tribble(
  ~well, ~block, ~test_temperature,
  "f5",  1,      42,
  "f10", 1,      41,
  "b2",  2,      35,
  "b2",  3,      41,
  "d8",  3,      41
)

growth_rates <- read_csv("data-processed/all-blocks-growth-sliding-window.csv") |>
  filter(evolution_history %in% c("35 evolved", "40 evolved", "fRS585")) |> 
  filter(window_size == 5) |>       # use the chosen window size
  filter(!is.na(mu)) |>     # drop failed fits
  anti_join(excluded_series, by = c("well", "block", "test_temperature")) |>
  rename(temp = test_temperature,
         rate = mu)

# lookup table for joining evolution history back after fitting
strain_key <- growth_rates |>
  distinct(strain, evolution_history)


# fit Sharpe-Schoolfield model --------------------------------------------

fit_ss <- function(df, iter = 2000) {
  model_name <- "sharpeschoolhigh_1981"

  starts <- get_start_vals(df$temp, df$rate, model_name = model_name)
  lower  <- get_lower_lims(df$temp, df$rate, model_name = model_name)
  upper  <- get_upper_lims(df$temp, df$rate, model_name = model_name)

  tryCatch({
    nls_multstart(
      formula     = rate ~ sharpeschoolhigh_1981(temp, r_tref, e, eh, th, tref = 20),
      data        = df,
      iter        = iter,
      start_lower = starts * 0.5,
      start_upper = starts * 1.5,
      lower       = lower,
      upper       = upper,
      supp_errors = "Y"
    )
  }, error = function(e) NULL)
}

# fit one TPC per strain
fits <- growth_rates |>
  group_by(strain) |>
  nest() |>
  mutate(fit = map(data, fit_ss))

# report any failed fits
n_failed <- sum(map_lgl(fits$fit, is.null))
message(n_failed, " strains failed to fit out of ", nrow(fits))


# generate predictions ----------------------------------------------------

preds <- fits |>
  filter(!map_lgl(fit, is.null)) |>
  mutate(pred = map2(data, fit, ~ {
    tibble(temp = seq(min(.x$temp) - 5, max(.x$temp) + 10, length.out = 200)) |>
      mutate(rate = predict(.y, newdata = tibble(temp = temp)))
  })) |>
  select(strain, pred) |>
  unnest(pred) |>
  left_join(strain_key, by = "strain")


# extract derived traits --------------------------------------------------

traits <- fits |>
  filter(!map_lgl(fit, is.null)) |>
  mutate(traits = map(fit, calc_params)) |>
  select(strain, traits) |>
  unnest(traits) |>
  left_join(strain_key, by = "strain") |>
  rename(tmax = ctmax)

write_csv(traits, "data-processed/all-SS-traits-sliding-window.csv")
message("Traits written to data-processed/all-SS-traits-sliding-window.csv")


# plot: TPC curves faceted by evolution history ---------------------------

ggplot() +
  geom_point(
    data  = growth_rates |> left_join(strain_key, by = c("strain", "evolution_history")),
    aes(x = temp, y = rate, color = evolution_history),
    alpha = 0.5, size = 1.5
  ) +
  geom_line(
    data = preds,
    aes(x = temp, y = rate, color = evolution_history, group = strain),
    linewidth = 0.8
  ) +
  facet_wrap(~ evolution_history, scales = "free_y") +
  labs(
    title = "Sharpe-Schoolfield TPC fits (sliding window growth rates)",
    x     = "Temperature (°C)",
    y     = "Growth rate (per day)"
  ) +
  theme(legend.position = "none")

ggsave("figures/tpcs-sliding-window-by-evolution-history.png", width = 12, height = 8)


# plot: all TPC curves on one panel, coloured by evolution history --------

growth_rates |>
  left_join(strain_key, by = c("strain", "evolution_history")) |> 
  filter(evolution_history %in% c("35 evolved", "40 evolved", "fRS585")) |> 
ggplot(aes(x = temp, y = rate, color = evolution_history)) +
  geom_point()+
  geom_vline(xintercept = c(35, 40), color = "grey80", linetype = "dashed") +
  geom_line(
    data = preds,
    aes(x = temp, y = rate, color = evolution_history, group = strain),
    linewidth = 0.8, alpha = 0.7
  ) +
  labs(
    x     = "Temperature (°C)",
    y     = "Growth rate (per day)",
    color = "Evolution history"
  )

ggsave("figures/tpcs-sliding-window-all.png", width = 10, height = 6)


# plot: one panel per strain ----------------------------------------------

ggplot() +
  geom_point(
    data  = growth_rates,
    aes(x = temp, y = rate, color = evolution_history),
    size = 2, alpha = 0.7
  ) +
  geom_line(
    data = preds,
    aes(x = temp, y = rate, color = evolution_history, group = strain),
    linewidth = 0.8
  ) +
  facet_wrap(~ strain, scales = "free_y") +
  labs(
    title = "Sharpe-Schoolfield TPC fits per strain",
    x     = "Temperature (°C)",
    y     = "Growth rate (per day)"
  ) +
  theme(
    legend.position = "none",
    strip.text      = element_text(size = 7)
  )

ggsave("figures/tpcs-sliding-window-per-strain.png", width = 18, height = 14)


# plot: derived traits (topt, ctmax, breadth) by evolution history --------

trait_summary <- traits |>
  group_by(evolution_history) |>
  summarise(
    across(c(topt, tmax, breadth),
           list(mean = mean, se = std.error),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  )

plot_trait <- function(trait_col, mean_col, se_col, ylabel) {
  ggplot() +
    geom_point(
      data = traits,
      aes(x = evolution_history, y = .data[[trait_col]]),
      alpha = 0.5, size = 2, position = position_jitter(width = 0.1)
    ) +
    geom_pointrange(
      data = trait_summary,
      aes(x    = evolution_history,
          y    = .data[[mean_col]],
          ymin = .data[[mean_col]] - .data[[se_col]],
          ymax = .data[[mean_col]] + .data[[se_col]])
    ) +
    labs(x = NULL, y = ylabel) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}

p_topt    <- plot_trait("topt",   "topt_mean",   "topt_se",   "Topt (°C)")
p_tmax    <- plot_trait("tmax",   "tmax_mean",   "tmax_se",   "Tmax (°C)")
p_breadth <- plot_trait("breadth","breadth_mean","breadth_se","Thermal breadth (°C)")

plot_grid(p_topt, p_tmax, p_breadth, nrow = 1)
ggsave("figures/tpcs-sliding-window-traits.png", width = 14, height = 5)
