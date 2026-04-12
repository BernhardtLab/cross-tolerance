### Find maximum growth rate using variable-length sliding windows (≥ 5 points)
#
# Modified from original sliding window script
#
# Key features:
#   - considers ALL windows of length ≥ 5 points
#   - applies R² filter (≥ 0.97)
#   - returns window metadata
#   - produces diagnostic plots showing chosen window + fitted slope
#
# Input:
#   data-processed/all-blocks-tpc-experiment.csv
#
# Output:
#   data-processed/all-blocks-growth-sliding-window-variable.csv
#   figures/mu-temp-variable-window.png
#   figures/diagnostic-variable/*.png

# packages ----------------------------------------------------------------

library(tidyverse)
library(cowplot)

theme_set(theme_cowplot())

# read data ---------------------------------------------------------------

all_blocks <- read_csv("data-processed/all-blocks-tpc-experiment.csv")

# sliding window helpers --------------------------------------------------

fit_window <- function(days, od) {
  fit <- lm(log(od) ~ days)
  list(
    growth_rate = coef(fit)[["days"]],
    r_squared   = summary(fit)$r.squared
  )
}

# main function: variable window search
max_growth_in_series <- function(df, min_window_size = 5, min_r_squared = 0.97) {
  
  df <- arrange(df, days)
  n  <- nrow(df)
  
  if (n < min_window_size) {
    return(tibble(
      mu          = NA_real_,
      r_squared   = NA_real_,
      days_start  = NA_real_,
      days_end    = NA_real_,
      window_size = NA_integer_,
      window_idx  = NA_integer_
    ))
  }
  
  # generate ALL windows ≥ min_window_size
  windows <- map_dfr(seq_len(n), function(start) {
    
    map_dfr((start + min_window_size - 1):n, function(end) {
      
      if (end > n) return(NULL)
      
      wd <- df[start:end, ]
      
      fit <- fit_window(wd$days, wd$od)
      
      tibble(
        window_idx  = start,
        window_size = end - start + 1,
        days_start  = wd$days[1],
        days_end    = wd$days[nrow(wd)],
        mu          = fit$growth_rate,
        r_squared   = fit$r_squared
      )
    })
  })
  
  # apply R² threshold
  eligible <- windows %>%
    filter(r_squared >= min_r_squared)
  
  if (nrow(eligible) == 0) {
    eligible <- windows
  }
  
  # choose best slope
  best <- eligible %>%
    slice_max(mu, n = 1, with_ties = FALSE)
  
  return(best)
}

# apply across dataset ----------------------------------------------------

growth_summary <- all_blocks %>%
  filter(od > 0) %>%
  group_by(well, block, test_temperature, strain, evolution_history) %>%
  group_modify(~ max_growth_in_series(.x, min_window_size = 5, min_r_squared = 0.97)) %>%
  ungroup()

# save results ------------------------------------------------------------

write_csv(
  growth_summary,
  "data-processed/all-blocks-growth-sliding-window-variable.csv"
)

message("Growth rate estimation complete.")

# summary plot ------------------------------------------------------------

growth_summary %>%
  filter(strain != "WT_FLZ_2") %>%
  ggplot(aes(x = test_temperature, y = mu, color = evolution_history)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Growth rates (variable window ≥ 5 points)",
    x = "Temperature (°C)",
    y = "Max growth rate (per day)"
  )

ggsave("figures/mu-temp-variable-window.png", width = 8, height = 5)

# diagnostic plots --------------------------------------------------------

dir.create("figures/diagnostic-variable", showWarnings = FALSE)

# join best window info back to raw data
plot_data <- all_blocks %>%
  filter(od > 0) %>%
  left_join(
    growth_summary %>%
      select(
        well, block, test_temperature,
        days_start, days_end,
        mu, r_squared, window_size
      ),
    by = c("well", "block", "test_temperature")
  ) %>%
  mutate(
    in_best_window = days >= days_start & days <= days_end
  )

write_csv(plot_data, "data-processed/plot-data-variable-fits.csv")

# unique time series
series_keys <- plot_data %>%
  distinct(well, block, test_temperature)

# loop over each series
walk(seq_len(nrow(series_keys)), function(i) {
  
  w    <- series_keys$well[i]
  b    <- series_keys$block[i]
  temp <- series_keys$test_temperature[i]
  
  pd <- plot_data %>%
    filter(well == w, block == b, test_temperature == temp)
  
  best_window_df <- pd %>%
    filter(in_best_window)
  
  # skip if insufficient points
  if (nrow(best_window_df) < 2) return(NULL)
  
  fit <- lm(log(od) ~ days, data = best_window_df)
  
  label_text <- paste0(
    "μ = ", round(unique(pd$mu), 3), "\n",
    "R² = ", round(unique(pd$r_squared), 3), "\n",
    "n = ", unique(pd$window_size)
  )
  
  p <- ggplot(pd, aes(x = days, y = log(od))) +
    
    # all data
    geom_point(color = "gray60", alpha = 0.7, size = 2) +
    
    # highlight best window
    geom_point(
      data = best_window_df,
      color = "red",
      size = 2.5
    ) +
    
    # fitted slope
    geom_smooth(
      data = best_window_df,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      color = "red",
      linewidth = 1
    ) +
    
    # annotation
    annotate(
      "text",
      x = -Inf, y = Inf,
      label = label_text,
      hjust = -0.1, vjust = 1.2,
      size = 3.5
    ) +
    
    labs(
      title = paste0(w, " | block ", b, " | ", temp, "°C"),
      x = "Days",
      y = "ln(OD)"
    )
  
  fname <- paste0(
    "figures/diagnostic-variable/",
    w, "_block", b, "_", temp, "C.png"
  )
  
  ggsave(fname, p, width = 6, height = 4)
})

message("Diagnostic plots saved to figures/diagnostic-variable/")
