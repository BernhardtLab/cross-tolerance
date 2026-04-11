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

# all_blocks <- all_blocks |>
#   filter(strain == "fRS585", block == 1)


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


# apply across window sizes 4, 5, 6 --------------------------------------
# Runs the full sliding window analysis for each window size and stacks
# results so we can compare how sensitive mu estimates are to window choice.

window_sizes <- c(4, 5, 6)

growth_summary <- map_dfr(window_sizes, function(ws) {
  all_blocks |>
    filter(od > 0) |>
    group_by(well, block, test_temperature, strain, evolution_history) |>
    group_modify(~ max_growth_in_series(.x, window_size = ws)) |>
    ungroup() |>
    mutate(window_size = ws)
})


# save results ------------------------------------------------------------

write_csv(growth_summary, "data-processed/all-blocks-growth-sliding-window.csv")

message("Done. Results written to data-processed/all-blocks-growth-sliding-window.csv")


# plot: compare mu estimates across window sizes --------------------------
# Each panel is one temperature; points are coloured by window size so you
# can see whether the estimates shift meaningfully as the window grows.

growth_summary |>
  ggplot(aes(x = factor(window_size), y = mu, color = factor(window_size))) +
  geom_jitter(width = 0.15, alpha = 0.7, size = 2) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "black", linewidth = 0.5) +
  facet_wrap(~ test_temperature, scales = "free_y",
             labeller = labeller(test_temperature = function(x) paste0(x, "°C"))) +
  labs(
    title = "Max growth rate by window size — fRS585 block 1, all temperatures",
    x     = "Window size (time points)",
    y     = "Max growth rate (per day)",
    color = "Window size"
  )

ggsave("figures/growth-rates-window-size-comparison.png", width = 14, height = 6)


# plot: diagnostic — one plot per well × temperature ----------------------
# For each well × block × temperature combination, saves an individual PNG
# showing the raw ln(OD) points in grey and the fitted line for each window
# size (4, 5, 6) in a different colour, with R² values annotated.

dir.create("figures/diagnostic", showWarnings = FALSE)

# Build one joined data frame per window size, then stack them
plot_data <- map_dfr(window_sizes, function(ws) {
  all_blocks |>
    filter(od > 0) |>
    left_join(
      growth_summary |>
        filter(window_size == ws) |>
        select(well, block, test_temperature, days_start, days_end, mu, r_squared),
      by = c("well", "block", "test_temperature")
    ) |>
    mutate(
      window_size   = ws,
      in_max_window = days >= days_start & days <= days_end
    )
})

# Unique well × block × temperature combinations to iterate over
series_keys <- plot_data |>
  distinct(well, block, test_temperature)

walk(seq_len(nrow(series_keys)), function(i) {
  w    <- series_keys$well[i]
  b    <- series_keys$block[i]
  temp <- series_keys$test_temperature[i]

  pd <- plot_data |>
    filter(well == w, block == b, test_temperature == temp)

  raw <- pd |> filter(window_size == window_sizes[1])  # one copy of the raw points

  r2_labels <- pd |>
    distinct(window_size, r_squared) |>
    arrange(window_size) |>
    mutate(
      label        = paste0("W", window_size, ": R²=", round(r_squared, 3)),
      label_y_rank = row_number()
    )

  p <- ggplot(pd, aes(x = days, y = log(od))) +
    geom_point(data = raw, color = "gray60", alpha = 0.6, size = 2) +
    geom_smooth(
      data    = ~ filter(.x, in_max_window),
      aes(color = factor(window_size), group = factor(window_size)),
      method  = "lm", formula = y ~ x,
      se      = FALSE, linewidth = 0.9
    ) +
    geom_text(
      data  = r2_labels,
      aes(label = label, color = factor(window_size),
          vjust = label_y_rank * 1.8),
      x     = -Inf, y = Inf,
      hjust = -0.05,
      size  = 3.5,
      show.legend = FALSE
    ) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = paste0(w, " | block ", b, " | ", temp, "°C"),
      x     = "Days",
      y     = "ln(OD)",
      color = "Window size"
    )

  fname <- paste0("figures/diagnostic/", w, "_block", b, "_", temp, "C.png")
  ggsave(fname, p, width = 6, height = 4)
})

message("Diagnostic plots saved to figures/diagnostic/")




# now plots all the growth rates over the temperature gradient ------------

growth_summary |>
  filter(window_size == 4) |>
  ggplot(aes(x = test_temperature, y = mu, color = evolution_history)) +
  geom_point()
ggsave("figures/mu-temp.png")

growth_summary |>
  filter(window_size == 4, test_temperature == 42) |>
  arrange((mu)) |> View()  # or arrange(mu) to see the lowest

