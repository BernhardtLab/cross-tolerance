

### Estimate growth rates and fit thermal performance curves
# Author: Joey Bernhardt
# Input: "data-processed/all-temps-od.csv" (OD data from Tecan plate reader; TPC data)
# Output: 
# Written for R version 4.2.3
# Last updated: July 11 2025

# load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(lubridate)
library(growthTools)
library(rTPC)
library(minpack.lm)
library(car) ### for bootstrapping
library(plotrix) ### for SE calculations
library(broom)
library(tidyverse)
library(conflicted)
conflict_prefer("select", "dplyr")
conflicts_prefer(dplyr::filter)
library(nls.multstart)
library(rTPC)


# load data ---------------------------------------------------------------

  
all_blocks_no_lag <- read_csv("data-processed/all-blocks-growth-no-lag.csv")
  
d <- all_blocks_no_lag %>% 
    mutate(curve_id = strain) %>% 
    mutate(temp = test_temperature, 
           rate = mu)

well_key <- all_blocks_no_lag %>% 
  dplyr::select(strain, evolution_history) %>% 
  distinct()  
  
  
df <- d  
fit_briere <- function(df, model_name = "briere2_1999", iter = 2000) {
  
  # get starting values and parameter bounds
  starts <- get_start_vals(df$temp, df$rate, model_name = model_name)
  lower  <- get_lower_lims(df$temp, df$rate, model_name = model_name)
  upper  <- get_upper_lims(df$temp, df$rate, model_name = model_name)
  
  # construct formula for the Brière model
  param_names <- names(starts)
  rhs <- paste0(model_name, "(temp, ",
                paste(param_names, collapse = ", "),
                ")")
  formula <- as.formula(paste("rate ~", rhs))
  
  # fit using nls_multstart with error handling
  tryCatch({
    nls_multstart(
      formula = formula,
      data = df,
      iter = iter,
      start_lower = starts * 0.5,
      start_upper = starts * 1.5,
      lower = lower,
      upper = upper,
      supp_errors = "N"
    )
  }, error = function(e) {
    structure(list(error = TRUE, message = e$message),
              class = "tpc_fit_error")
  })
}


fits_briere <- df %>%
  group_by(curve_id) %>%
  nest() %>%
  mutate(fit = map(data, ~ fit_briere(.x, iter = 2000)))

fit_status_briere <- fits_briere %>%
  mutate(ok = map_lgl(fit, ~ !inherits(.x, "tpc_fit_error") && !is.null(.x))) %>%
  select(curve_id, ok)

  
  preds_briere <- fits_briere %>%
    filter(map_lgl(fit, ~ !inherits(.x, "tpc_fit_error") && !is.null(.x))) %>%
    mutate(pred = map2(data, fit, ~ {
      tibble(temp = seq(min(.x$temp)-10, max(.x$temp) + 15, length.out = 200)) %>%
        mutate(fit = predict(.y, newdata = tibble(temp = temp)))
    })) %>%
    dplyr::select(curve_id, pred) %>%
    unnest(pred)
  
preds2_briere <- preds_briere %>% 
  left_join(well_key, by = c("curve_id" = "strain"))
  
  ggplot() +
    geom_point(data = df, aes(x = temp, y = rate, color = evolution_history)) +
    geom_line(data = preds2_briere, aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1) +
    facet_wrap(~ evolution_history, scales = "free_y") +
    theme_minimal() +
    labs(
      x = "Temperature (°C)",
      y = "Growth rate",
      title = "Briere fits per population"
    )
  ggsave("figures/tpcs-all-no-lag.png", width = 10, height = 6)
  
  
  
  
  ggplot() +
    geom_vline(xintercept = 35, color = "darkgrey") +
    geom_vline(xintercept = 40, color = "darkgrey") +
    geom_hline(yintercept = 1, color = "darkgrey") +
    # geom_line(data = filter(preds2, evolution_history != "Fluconazole evolved"), aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1, alpha = 0.5) +
    geom_line(data = filter(preds2, evolution_history == "fRS585"), aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1, alpha = 0.5) +
    geom_point(data = filter(df, evolution_history == "fRS585"), aes(x = temp, y = rate, color = evolution_history), size = 1.5) +
    
    geom_line(data = filter(preds2, evolution_history == "35 evolved"), aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1, alpha = 0.5) +
    geom_point(data = filter(df, evolution_history == "35 evolved"), aes(x = temp, y = rate, color = evolution_history), size = 1.5) +
    
    geom_line(data = filter(preds2, evolution_history == "40 evolved"), aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1, alpha = 0.5) +
    geom_point(data = filter(df, evolution_history == "40 evolved"), aes(x = temp, y = rate, color = evolution_history), size = 1.5) +
    
    geom_line(data = filter(preds2, evolution_history == "Caspofungin evolved"), aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1, alpha = 0.5) +
    geom_point(data = filter(df, evolution_history == "Caspofungin evolved"), aes(x = temp, y = rate, color = evolution_history), size = 1.5) +
    
    # geom_point(data = filter(df, evolution_history != "Fluconazole evolved"), aes(x = temp, y = rate, color = evolution_history), size = 1.5) +
    labs(
      x = "Temperature (°C)",
      y = "Growth rate",
    ) + scale_color_manual(values = c( "#019875FF", "#FF847CFF", "#C0392BFF", "#96281BFF")) 
    
  ggsave("figures/tpcs-all-no-lag-single.png", width = 8, height = 5)  
  
  
  
  ggplot() +
    geom_point(data = df, aes(x = temp, y = rate, color = evolution_history)) +
    geom_line(data = preds2, aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1) +
    facet_wrap(~ curve_id, scales = "free_y") +
    theme_minimal() +
    labs(
      x = "Temperature (°C)",
      y = "Growth rate",
      title = "Sharpe–Schoolfield high fits per population"
    )
  ggsave("figures/tpcs-all-no-lag-separate.png", width = 15, height = 12)
  
  
  
  
  
  ggplot() +
    geom_point(data = df, aes(x = temp, y = rate, color = evolution_history)) +
    geom_line(data = preds2, aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1) +
    # facet_wrap(~ evolution_history, scales = "free_y") +
    theme_minimal() +
    labs(
      x = "Temperature (°C)",
      y = "Growth rate",
      title = "Sharpe–Schoolfield high fits per population"
    )
  ggsave("figures/tpcs-all-no-facet-no-lag.png", width = 10, height = 6)
  
  
  
  ggplot() +
    geom_point(data = df, aes(x = temp, y = rate, color = evolution_history)) +
    geom_line(data = preds2, aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1) +
    # facet_wrap(~ evolution_history, scales = "free_y") +
    theme_minimal() +
    labs(
      x = "Temperature (°C)",
      y = "Growth rate",
      title = "Sharpe–Schoolfield high fits per population"
    )
  ggsave("figures/tpcs-all-no-facet-no-lag.png", width = 10, height = 6)
  
  preds2 %>% 
    filter(evolution_history %in% c("35 evolved", "40 evolved")) %>%
    ggplot(aes(x = temp, y = fit, group = curve_id, color = evolution_history)) + geom_line() 
  ggsave("figures/tpcs-all-temp-evolved-no-lag.png", width = 10, height = 6)

  
  
  
  preds2 %>% 
    # filter(evolution_history %in% c("35 evolved", "40 evolved", "Fluconazole evolved")) %>%
    filter(evolution_history %in% c("35 evolved", "40 evolved", "Caspofungin evolved")) %>%
    ggplot(aes(x = temp, y = fit, group = curve_id, color = evolution_history)) + geom_line() 
  ggsave("figures/tpcs-all-temp-evolved-no-lag.png", width = 10, height = 6)
  
  
  
  length(unique(df$strain))
  
  
  param_table <- fits %>%
    filter(map_lgl(fit, ~ !inherits(.x, "tpc_fit_error") && !is.null(.x))) %>%
    mutate(
      params = map(fit, ~ broom::tidy(.x)),
      traits = map(fit, ~ calc_params(.x))
    ) %>%
    dplyr::select(curve_id, params, traits)
  
  # Parameter estimates
  param_estimates <- param_table %>% dplyr::select(curve_id, params) %>% unnest(params)

  
  # Derived traits (topt, rmax, ctmax, etc.)
  trait_estimates <- param_table %>% select(curve_id, traits) %>% unnest(traits)
  View(trait_estimates)

  
  traits <- trait_estimates %>% 
    left_join(well_key, by = c("curve_id" = "strain")) %>% 
    filter(!grepl("Fluc", evolution_history))
  
  traits %>%
    ggplot(aes(x = evolution_history, y = topt)) + geom_point()

  
  traits %>% 
    group_by(evolution_history) %>% 
    summarise(mean_topt = mean(topt),
              se_topt = std.error(topt)) %>% 
    ggplot(aes(x = evolution_history, y = mean_topt)) + geom_pointrange(aes(x = evolution_history, ymin = mean_topt - se_topt, ymax = mean_topt + se_topt)) +
    ylab("Topt") + xlab("Evolution history")
  ggsave("figures/topts-evolution-history-no-lag.png", width = 8, height = 6)
  
  
  traits %>% 
    group_by(evolution_history) %>% 
    summarise(mean_tmax = mean(ctmax),
              se_tmax = std.error(ctmax)) %>% 
    ggplot(aes(x = evolution_history, y = mean_tmax)) + geom_pointrange(aes(x = evolution_history, ymin = mean_tmax - se_tmax, ymax = mean_tmax + se_tmax)) +
    ylab("Tmax") + xlab("Evolution history")
  ggsave("figures/tmax-evolution-history-no-lag.png", width = 8, height = 6)
  

  
  t4 <- traits %>%
    group_by(evolution_history) %>% 
    summarise(mean_tmax = mean(ctmax),
              se_tmax = std.error(ctmax),
              mean_topt = mean(topt),
              se_topt = std.error(topt),
              mean_breadth = mean(breadth),
              se_breadth = std.error(breadth))
  
  
ggplot() +
    geom_pointrange(aes(x = evolution_history, y = mean_tmax, ymin = mean_tmax - se_tmax, ymax = mean_tmax + se_tmax), data = t4) +
  geom_point(aes(x = evolution_history, y = ctmax), data = traits, alpha = 0.5) +
    ylab("Tmax") + xlab("Evolution history")
ggsave("figures/tmax-evolution-history-all-pops-no-lag.png", width = 9, height = 6)

ggplot() +
  geom_pointrange(aes(x = evolution_history, y = mean_topt, ymin = mean_topt - se_topt, ymax = mean_topt + se_topt), data = t4) +
  geom_point(aes(x = evolution_history, y = topt), data = traits, alpha = 0.5) +
  ylab("Topt") + xlab("Evolution history")
ggsave("figures/topt-evolution-history-all-pops-no-lag.png", width = 9, height = 6)




ggplot() +
  geom_pointrange(aes(x = evolution_history, y = mean_breadth, ymin = mean_breadth - se_breadth, ymax = mean_breadth + se_breadth), data = t4) +
  geom_point(aes(x = evolution_history, y = breadth), data = traits, alpha = 0.5) +
  ylab("Breadth") + xlab("Evolution history")
ggsave("figures/breadth-evolution-history-all-pops-no-lag.png", width = 9, height = 6)



# predict growth at 40C ---------------------------------------------------

predict_at_temp <- function(model, temp = 40) {
  if (is.null(model)) return(NA_real_)
  
  tryCatch(
    {
      predict(model, newdata = data.frame(temp = temp))
    },
    error = function(e) NA_real_
  )
}





param_table_40 <- fits %>%
  mutate(
    params  = map(fit, ~ broom::tidy(.x$fit)),
    traits  = map(fit, ~ calc_params(.x$fit)),
    rate_40 = map_dbl(fit, predict_at_temp, temp = 40),
    rate_35 = map_dbl(fit, predict_at_temp, temp = 35),
    rate_41 = map_dbl(fit, predict_at_temp, temp = 41),
    rate_42 = map_dbl(fit, predict_at_temp, temp = 42)
  ) %>%
  select(curve_id, params, traits, rate_40, rate_42, rate_41, rate_35)

growth_40 <- left_join(param_table_40, well_key, by = c("curve_id" = "strain")) %>% 
  filter(!grepl("Fluc", evolution_history))

growth_40 %>% 
  ggplot(aes(x = evolution_history, y = rate_41)) + geom_point()

g2 <- growth_40 %>% 
  filter(!grepl("Fluc", evolution_history)) %>% 
  group_by(evolution_history) %>% 
  summarise(mean_40 = mean(rate_40),
            se_40 = std.error(rate_40),
            mean_42 = mean(rate_42),
            se_42 = std.error(rate_42),
            mean_41 = mean(rate_41),
            se_41 = std.error(rate_41),
            mean_35 = mean(rate_35),
            se_35 = std.error(rate_35))

ggplot() +
  geom_pointrange(aes(x = evolution_history, y = mean_40, ymin = mean_40 - se_40, ymax = mean_40 + se_40), data = g2) +
  geom_point(aes(x = evolution_history, y = rate_40), data = growth_40, alpha = 0.5) +
  ylab("Growth at 40C") + xlab("Evolution history")
ggsave("figures/growth-40-no-lag.png", width = 9, height = 6)


ggplot() +
  geom_pointrange(aes(x = evolution_history, y = mean_35, ymin = mean_35 - se_35, ymax = mean_35 + se_35), data = g2) +
  geom_point(aes(x = evolution_history, y = rate_35), data = growth_40, alpha = 0.5) +
  ylab("Growth at 35C") + xlab("Evolution history")
ggsave("figures/growth-35-no-lag.png", width = 9, height = 6)




ggplot() +
  geom_pointrange(aes(x = evolution_history, y = mean_41, ymin = mean_41 - se_41, ymax = mean_41 + se_41), data = g2) +
  geom_point(aes(x = evolution_history, y = rate_41), data = growth_40, alpha = 0.5) +
  ylab("Growth at 41C") + xlab("Evolution history")
ggsave("figures/growth-41-no-lag.png", width = 9, height = 6)


ggplot() +
  geom_pointrange(aes(x = evolution_history, y = mean_42, ymin = mean_42 - se_42, ymax = mean_42 + se_42), data = g2) +
  geom_point(aes(x = evolution_history, y = rate_42), data = growth_40, alpha = 0.5) +
  ylab("Growth at 42C") + xlab("Evolution history")
ggsave("figures/growth-42-no-lag.png", width = 9, height = 6)


# find temperature where growth = 0.1 -------------------------------------

temp_at_rate <- function(model, target_rate = 1,
                         from = 40, to = 50, by = 0.001) {
  
 
  # evaluate on grid
  temps <- seq(from, to, by)
  preds <- tryCatch(
    predict(model, newdata = data.frame(temp = temps)),
    error = function(e) rep(NA_real_, length(temps))
  )
  
  # if model never reaches target_rate → return NA
  if (all(is.na(preds)) || max(preds, na.rm = TRUE) < target_rate)
    return(NA_real_)
  
  # temperature with prediction closest to target
  idx <- which.min(abs(preds - target_rate))
  temps[idx]
}


param_table_full <- fits %>%
  mutate(
    params    = map(fit, ~ broom::tidy(.x$fit)),
    traits    = map(fit, ~ calc_params(.x$fit)),
    rate_40   = map_dbl(fit, predict_at_temp, temp = 40),
    rate_42   = map_dbl(fit, predict_at_temp, temp = 42),
    rate_41   = map_dbl(fit, predict_at_temp, temp = 41),
    temp_rate = map_dbl(fit, temp_at_rate, target_rate = 1)
  ) %>%
  select(curve_id, params, traits, rate_40, rate_42, temp_rate, rate_41)


growth_full <- left_join(param_table_full, well_key, by = c("curve_id" = "strain")) %>% 
  filter(!grepl("Fluc", evolution_history))

g3 <- growth_full %>% 
  group_by(evolution_history) %>% 
  summarise(mean_40 = mean(rate_40),
            se_40 = std.error(rate_40),
            mean_42 = mean(rate_42),
            se_42 = std.error(rate_42),
            mean_tmax = mean(temp_rate),
            se_tmax = std.error(temp_rate))

  
ggplot() +
  geom_pointrange(aes(x = evolution_history, y = mean_tmax, ymin = mean_tmax - se_tmax, ymax = mean_tmax + se_tmax), data = g3) +
  geom_point(aes(x = evolution_history, y = temp_rate), data = growth_full, alpha = 0.5) +
  ylab("Temperature at growth rate of 1") + xlab("Evolution history")
ggsave("figures/tmax_est-no-lag.png", width = 9, height = 6)


# merge all the traits ----------------------------------------------------

p2 <- param_table_full %>% 
  select(curve_id, rate_40, rate_42, rate_41, temp_rate)

p3 <- left_join(traits, p2)

write_csv(p3, "data-processed/all-SS-traits.csv")

library(visreg)

mod1 <- lm(rate_41 ~ evolution_history, data = p3)
summary(mod1)

visreg(mod1, "evolution_history")

mod1b <- lm(rate_42 ~ evolution_history, data = p3)
summary(mod1b)

visreg(mod1b, "evolution_history")


mod1d <- lm(eh ~ evolution_history, data = p3)
summary(mod1d)

visreg(mod1d, "evolution_history")

mod1e <- lm(ctmax ~ evolution_history, data = p3)
summary(mod1e)

visreg(mod1e, "evolution_history")


mod1c <- lm(temp_rate ~ evolution_history, data = p3)
summary(mod1c)

visreg(mod1c, "evolution_history")


growth_42_raw <- all_blocks_no_lag %>%
  filter(!grepl("Fluc", evolution_history)) %>% 
  filter(test_temperature == 41)

mod2 <- lm(mu ~ evolution_history, data =growth_42_raw)
summary(mod2)



visreg(mod2, "evolution_history")



# test for hot-cold trade-off ---------------------------------------------
library(vegan)
growth_wide <- preds %>% 
  ungroup() %>% 
  spread(key = temp, value = fit) %>% 
  select(-curve_id)


cov_mat <- cov(growth_wide)
pca_res <- prcomp(cov_mat, center = TRUE,scale. = TRUE)


pca16 <- rda(growth_wide)


loadings16 <- scores(pca_res,choices=c(1,2))
names(growth_wide)

loadings16 <- scores(pca_res,choices=c(1,2))
summary(eigenvals(pca_res))

pcs16 <- as_data_frame((loadings16)) %>% 
  select(PC1)
pc1_16 <- pcs16 %>% 
  mutate(temperature = names(growth_wide)) %>% 
  mutate(temperature = as.numeric(temperature))


pc1_16 %>% 
  ggplot(aes(x = temperature, y = PC1)) + geom_point() +
  # xlim(0, 40) + 
  geom_hline(yintercept = 0) +
  xlab("Temperature (°C)") + geom_line() +
  ylab("PC1 loadings")

ggsave("figures/generalist-specialist-trade-off.png", width = 8, height = 6)


pc2 <- as_data_frame((loadings16)) %>% 
  select(PC2)
pc2b <- pc2 %>% 
  mutate(temperature = names(growth_wide)) %>% 
  mutate(temperature = as.numeric(temperature))


pc2b %>% 
  ggplot(aes(x = temperature, y = PC2)) + geom_point() +
  # xlim(0, 40) + 
  geom_hline(yintercept = 0) +
  xlab("Temperature (°C)") + geom_line() +
  ylab("PC2 loadings")

