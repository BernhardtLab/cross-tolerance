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
library(zoo)

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
max_growth_in_series <- function(df, window_size = 4, min_r_squared = 0.97) {
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

  # Only consider windows with a good enough fit; if none pass the threshold,
  # fall back to the best available so the series still returns a result
  eligible <- filter(windows, r_squared >= min_r_squared)
  if (nrow(eligible) == 0) {
    eligible <- windows  # fall back to all windows
  }

  eligible[which.max(eligible$mu), ]
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
  filter(strain != "WT_FLZ_2") |> 
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


growth_summary |> 
  filter(strain != "WT_FLZ_2") |> 
  filter(test_temperature == 25) |> View()



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
### ok now we only have 5 wells that seems to have somewhat sketchy growth rates, because they include what look like some outlier OD readings. They are 
## f5 block 1 42C,
##f10, block 1, 41C
## b2 block 2 35C,
## b2 block3, 41C
## d8 block 3, 41C
### consider taking these out


growth_summary |>
  filter(window_size == 6) |>
  ggplot(aes(x = test_temperature, y = mu, color = evolution_history)) +
  geom_point()
ggsave("figures/mu-temp.png")

growth_summary |>
  filter(window_size == 6, test_temperature == 35) |>
  arrange((mu)) |> View()  # or arrange(mu) to see the lowest


### note that the code below here, I don't end up using, because I didn't find the rolling median approach to be very good -- but I'm leaving the code here for now.
# outlier detection using rolling median ----------------------------------
# For a given time series, compute a rolling median of OD values and flag
# points that deviate more than `threshold` * MAD from the rolling median.
# k is the rolling window size (must be odd); a k of 3 works well for sparse
# time series like these since it only looks one point either side.

# Spike detection based on direction reversal.
# A measurement error typically appears as a point that breaks the local trend:
# the OD jumps up (or down) then immediately returns, creating a sharp V or Λ
# shape. We detect this by looking at the difference before and after each point:
# if those differences are opposite in sign AND both large relative to the
# typical step size in the series, the point is flagged as a spike.

detect_outliers <- function(df, threshold = 3) {
  df <- arrange(df, days)
  n  <- nrow(df)

  od <- df$od

  # first differences between consecutive OD readings
  diffs <- diff(od)  # length n-1

  # for each interior point i, diff_before = od[i] - od[i-1]
  #                              diff_after = od[i+1] - od[i]
  diff_before <- c(NA, diffs)        # shift right: index i = diff between i and i-1
  diff_after  <- c(diffs, NA)        # index i = diff between i+1 and i

  # typical step size across the whole series
  mad_diff <- mad(diffs, na.rm = TRUE)

  # a point is a spike if:
  # 1. the differences either side are opposite in sign (direction reversal), AND
  # 2. at least one of the differences is large (at least threshold * MAD)
  # Using OR rather than AND for condition 2 catches spikes during the exponential
  # phase, where the global MAD is inflated by large legitimate diffs — meaning
  # both diffs around a spike may not both clear the threshold even though the
  # spike itself is obvious
  is_outlier <- !is.na(diff_before) & !is.na(diff_after) &
    sign(diff_before) != sign(diff_after) &
    (abs(diff_before) > threshold * mad_diff |
     abs(diff_after)  > threshold * mad_diff)

  df |> mutate(is_outlier = is_outlier)
}

# apply to the two wells of interest
outlier_wells <- list(
  list(well = "b2",  block = 2, test_temperature = 35),
  list(well = "f10", block = 1, test_temperature = 38)
)

outlier_data <- map_dfr(outlier_wells, function(w) {
  all_blocks |>
    filter(well == w$well, block == w$block, test_temperature == w$test_temperature) |>
    detect_outliers()
})

# plot: OD time series with outliers highlighted in red
outlier_data |>
  mutate(series_label = paste0(well, " | block ", block, " | ", test_temperature, "°C")) |>
  ggplot(aes(x = days, y = od, color = is_outlier)) +
  geom_line(color = "gray70") +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("FALSE" = "steelblue", "TRUE" = "red"),
    labels = c("FALSE" = "Normal", "TRUE" = "Outlier")
  ) +
  facet_wrap(~ series_label, scales = "free") +
  labs(
    title = "OD time series — potential outliers flagged by spike detection",
    x     = "Days",
    y     = "OD",
    color = NULL
  )

ggsave("figures/outlier-detection.png", width = 10, height = 5)

