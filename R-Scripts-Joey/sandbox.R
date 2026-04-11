### Log-linear model fitting to find maximum growth rate

library(tidyverse)
all_blocks <- read_csv("data-processed/all-blocks-tpc-experiment.csv")

test_bit <- all_blocks |> 
  filter(strain == "40_F2", test_temperature == 35)

# ============================================================================
# Fit log-linear model to 5-point windows and find max growth rate
# ============================================================================

# Function to fit log-linear model and extract growth rate
fit_loglinear <- function(days, od) {
  # Fit model: ln(OD) = b0 + b1 * days
  # where b1 is the growth rate (mu)
  model <- lm(log(od) ~ days)
  
  # Extract growth rate (slope) and R-squared
  growth_rate <- coef(model)[2]
  r_squared <- summary(model)$r.squared
  
  return(list(
    growth_rate = growth_rate,
    r_squared = r_squared
  ))
}

# For each well, find maximum growth rate across all 5-point windows
max_growth_rates <- test_bit |>
  nest(data = -well) |>
  mutate(
    results = map(data, function(well_data) {
      # Sort by days to ensure time order
      well_data <- well_data |> arrange(days)
      
      n <- nrow(well_data)
      window_size <- 4
      
      # If not enough points, return NA
      if (n < window_size) {
        return(tibble(
          window_start = NA,
          growth_rate = NA,
          r_squared = NA,
          max_growth_rate = NA
        ))
      }
      
      # Fit model to all 4-point windows
      window_results <- map_df(1:(n - window_size + 1), function(i) {
        window_data <- well_data[i:(i + window_size - 1), ]
        
        fit <- fit_loglinear(window_data$days, window_data$od)
        
        tibble(
          window_start = i,
          days_start = window_data$days[1],
          days_end = window_data$days[5],
          growth_rate = fit$growth_rate,
          r_squared = fit$r_squared
        )
      })
      
      # Find the window with maximum growth rate
      max_idx <- which.max(window_results$growth_rate)
      
      window_results |>
        mutate(is_max = row_number() == max_idx)
    })
  ) |>
  select(well, results) |>
  unnest(results)

# Summary: maximum growth rate for each well
max_growth_summary <- max_growth_rates |>
  filter(is_max) |>
  select(well, growth_rate, r_squared, days_start, days_end)

print(max_growth_summary)

# ============================================================================
# Plot: log OD with fitted line for max growth rate window
# ============================================================================

well_list <- unique(test_bit$well)

# Prepare data for all plots
plot_data <- map_df(well_list, function(well_name) {
  well_data <- test_bit |>
    filter(well == well_name) |>
    arrange(days)
  
  n <- nrow(well_data)
  window_size <- 4
  
  # Fit model to all windows and find max
  window_results <- map_df(1:(n - window_size + 1), function(i) {
    window_data <- well_data[i:(i + window_size - 1), ]
    fit <- fit_loglinear(window_data$days, window_data$od)
    
    tibble(
      window_start = i,
      days_start = window_data$days[1],
      days_end = window_data$days[5],
      growth_rate = fit$growth_rate,
      r_squared = fit$r_squared
    )
  })
  
  max_idx <- which.max(window_results$growth_rate)
  max_window <- window_results[max_idx, ]
  
  # Mark which points are in the max window
  well_data |>
    mutate(
      in_max_window = days >= max_window$days_start & days <= max_window$days_end,
      max_growth = round(max_window$growth_rate, 2),
      max_r_squared = round(max_window$r_squared, 3),
      days_start = round(max_window$days_start, 3),
      days_end = round(max_window$days_end, 3)
    )
})

ggplot(plot_data, aes(x = days, y = log(od))) +
  geom_point(aes(color = in_max_window), alpha = 0.6, size = 2) +
  geom_smooth(data = plot_data |> filter(in_max_window), 
              method = "lm", se = FALSE, color = "red", linewidth = 1, formula = y ~ x) +
  facet_wrap(~well, scales = "free") +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "red")) +
  labs(
    x = "Days",
    y = "ln(OD)",
    color = "In max window"
  ) +
  theme_minimal()
