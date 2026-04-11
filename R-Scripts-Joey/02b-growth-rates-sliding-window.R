### Find maximum growth rate in each time series using a sliding window approach
#
# Author: Joey Bernhardt
# Input:  data-processed/all-blocks-tpc-experiment.csv
# Output: data-processed/all-blocks-growth-sliding-window.csv
#         figures/growth-rates-sliding-window.png
#
# Method: For each well × block × test_temperature time series, fit a linear
#         model to ln(OD) ~ days for every consecutive window of 4 time points.
#         The maximum slope across all windows is the maximum growth rate (mu),
#         in units of per day.


# packages ----------------------------------------------------------------

library(tidyverse)
library(cowplot)

theme_set(theme_cowplot())


# read data ---------------------------------------------------------------

all_blocks <- read_csv("data-processed/all-blocks-tpc-experiment.csv")


# dev subset — comment this out when ready to run on the full dataset -----

all_blocks <- all_blocks |>
  filter(strain == "fRS585", test_temperature == 35, block == 1)


# sliding window function -------------------------------------------------

# Fits ln(OD) ~ days on a single window of data; returns slope and R2
fit_window <- function(days, od) {
  fit <- lm(log(od) ~ days)
  list(
    growth_rate = coef(fit)[["days"]],
    r_squared   = summary(fit)$r.squared
  )
}

# For one time series (a data frame with days + od columns), slide a window of
# `window_size` consecutive points across every position and return the row
# corresponding to the maximum growth rate.
max_growth_in_series <- function(df, window_size = 4) {
  df <- arrange(df, days)
  n  <- nrow(df)

  if (n < window_size) {
    return(tibble(
      mu         = NA_real_,
      r_squared  = NA_real_,
      days_start = NA_real_,
      days_end   = NA_real_,
      window_idx = NA_integer_
    ))
  }

  windows <- map_dfr(seq_len(n - window_size + 1), function(i) {
    wd  <- df[i:(i + window_size - 1), ]
    fit <- fit_window(wd$days, wd$od)
    tibble(
      window_idx = i,
      days_start = wd$days[1],
      days_end   = wd$days[window_size],
      mu         = fit$growth_rate,
      r_squared  = fit$r_squared
    )
  })

  windows[which.max(windows$mu), ]
}


# apply to every time series ----------------------------------------------
# Each unique well × block × test_temperature is one time series.

growth_summary <- all_blocks |>
  filter(od > 0) |>
  group_by(well, block, test_temperature, strain, evolution_history) |>
  group_modify(~ max_growth_in_series(.x)) |>
  ungroup()


# save results ------------------------------------------------------------

write_csv(growth_summary, "data-processed/all-blocks-growth-sliding-window.csv")

message("Done. Results written to data-processed/all-blocks-growth-sliding-window.csv")
message(nrow(growth_summary), " time series fit")


# plot: max growth rates by evolution history and temperature --------------

growth_summary |>
  ggplot(aes(x = evolution_history, y = mu, color = evolution_history)) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "black", linewidth = 0.5) +
  facet_wrap(~test_temperature, scales = "free_y",
             labeller = labeller(test_temperature = function(x) paste0(x, "°C"))) +
  labs(
    title = "Maximum growth rates (sliding window, 4-point windows)",
    x     = NULL,
    y     = "Max growth rate (per day)"
  ) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 45, hjust = 1, vjust = 1)
  )

ggsave("figures/growth-rates-sliding-window.png", width = 14, height = 6)


# plot: diagnostic — log OD curves with max window highlighted ------------
# Shows every time series; the 4 points used for the max growth rate are
# highlighted in red with the fitted line drawn through them.

plot_data <- all_blocks |>
  filter(od > 0) |>
  left_join(
    growth_summary |> select(well, block, test_temperature, days_start, days_end, mu),
    by = c("well", "block", "test_temperature")
  ) |>
  mutate(in_max_window = days >= days_start & days <= days_end)

length(unique(plot_data$mu))




plot_data |>
  mutate(series_id = paste(well, block, test_temperature, sep = "_")) |>
  ggplot(aes(x = days, y = log(od))) +
  geom_point(aes(color = in_max_window), alpha = 0.6, size = 1.5) +
  geom_smooth(
    data    = filter(plot_data, in_max_window),
    aes(group = interaction(well, block, test_temperature)),
    method  = "lm", formula = y ~ x,
    se      = FALSE, color = "red", linewidth = 0.8
  ) +
  facet_wrap(~ well, scales = "free") +
  scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "tomato")) +
  labs(
    title = "ln(OD) over time — red points and line = max growth window",
    x     = "Days",
    y     = "ln(OD)",
    color = "In max window"
  ) +
  theme(
    legend.position = "none",
    strip.text      = element_text(size = 6)
  )

ggsave("figures/growth-rates-sliding-window-diagnostic.png", width = 24, height = 20)
