library(tidyverse)
library(broom)
library(lubridate)
library(minpack.lm)
library(rootSolve)
library(forcats)
library(dplyr)
library(rTPC)
library(purrr)
library(nls.multstart)
library(ggplot2)
library(boot)
library(car)
library(cowplot)
theme_set(theme_cowplot())



#### Fitting TPCs with the Thomas model

all_blocks <- read_csv("data-processed/all-blocks-growth-no-lag.csv")
d <- all_blocks %>% 
  mutate(curve_id = strain) %>% 
  mutate(temp = test_temperature, 
         rate = mu)

all_blocks2 <- all_blocks %>% 
  mutate(curve_id = strain) %>% 
  mutate(temp = test_temperature, 
         rate = mu)

d %>% 
  ggplot(aes(x = temp, y = rate)) + geom_point()
unique(d$strain)


# Split into list by curve_id
d_split <- split(d, d$curve_id)

# Initialize product lists --------------------------------------------------------------
boot_results <- list()

fit_error_log <- data.frame(
  curve_id = character(),
  message = character(),
  stringsAsFactors = FALSE
)

fit_results <- list()
start_vals_list <- list()
residuals_list <- list()
opt_fits_list <- list()


get_topt_rmax <- function(a, b, c, topt_param, temps = seq(0, 70, length.out = 250)) {
  preds <- thomas_2012(temp = temps, a = a, b = b, c = c, topt = topt_param)
  max_idx <- which.max(preds)
  tibble(topt = temps[max_idx],
         rmax = preds[max_idx])
}

find_topt_optim <- function(a, b, c, topt) {
  neg_fun <- function(temp) -thomas_2012(temp = temp, a = a, b = b, c = c, topt = topt)
  opt <- optim(par = topt, fn = neg_fun)  # unbounded; starts at the fitted 'topt'
  list(topt_calc = opt$par, rmax = -opt$value)
}

# Loop fits - include calc derived params  -----------------------------------------------------

for (id in names(d_split)) {
  d_curve <- d_split[[id]]
  message("Fitting curve_id = ", unique(d_curve$curve_id))
  
  # Attempt start values (no logging)
  start_vals <- tryCatch({
    get_start_vals(d_curve$temp, d_curve$rate, model_name = "thomas_2012")
  }, error = function(e) {
    message("Start value error for curve_id = ", id, ": ", e$message)
    return(NULL)
  })
  if (is.null(start_vals)) next
  
  # Parameter bounds
  lower_lims <- c(a = 0, b = -10, c = 0, topt = -100)
  upper_lims <- c(a = 500, b = 10, c = 700, topt = 100)
  
  # model fit (log errors here only)
  fit <- tryCatch({
    nlsLM(rate ~ thomas_2012(temp = temp, a, b, c, topt),
          data = d_curve,
          start = start_vals,
          lower = lower_lims,
          upper = upper_lims,
          control = nls.lm.control(maxiter = 1000))
  }, error = function(e) {
    message("Fit error for curve_id = ", id, ": ", e$message)
    fit_error_log <<- rbind(fit_error_log, data.frame(curve_id = id, message = e$message))
    return(NULL)
  })
  if (is.null(fit)) next
  
  # Save successful fit and values
  fit_results[[id]] <- list(fit = fit, data = d_curve)
  residuals_list[[id]] <- residuals(fit)
  
  # R2
  obs <- d_curve$rate
  pred <- predict(fit)
  tss <- sum((obs - mean(obs))^2)
  rss <- sum((obs - pred)^2)
  r_squared <- 1 - (rss / tss)
  
  
  coefs <- coef(fit)
  
  
  # calc_params for optimal fit ----
  opt_vals <- get_topt_rmax(coefs["a"], coefs["b"], coefs["c"], coefs["topt"],
                            temps = seq(min(d_curve$temp), max(d_curve$temp), length.out = 200))
  
  opt_fits_list[[id]] <- data.frame(
    curve_id = id,
    a = coefs["a"],
    b = coefs["b"],
    c = coefs["c"],
    topt_param = coefs["topt"],      # fitted parameter from Thomas model
    topt_calc = opt_vals$topt,       # recalculated optimum
    rmax = opt_vals$rmax,
    tmax = coefs["topt"] + 0.5 * coefs["c"],
    r_squared = r_squared
  )
  
  
  # Bootstrapping (no logging)
  boot3 <- tryCatch({
    Boot(fit, method = 'residual', R = 10000)
  }, error = function(e) {
    message("Bootstrap error for curve_id = ", id, ": ", e$message)
    return(NULL)
  })
  if (is.null(boot3)) next
  
  # Extract bootstrap parameter sets
  boot_params <- boot3$t %>%
    as.data.frame() %>%
    drop_na() %>%
    mutate(iter = 1:n(),
           tmax = topt + (0.5 * c),
           curve_id = d_curve$curve_id[1])
  
  boot_results[[id]] <- boot_params
  
  boot_params <- boot_params %>%
    rowwise() %>%
    mutate(vals = list(
      get_topt_rmax(a, b, c, topt,
                    temps = seq(min(d_curve$temp), max(d_curve$temp), length.out = 200))
    )) %>%
    mutate(
      topt_calc = vals$topt,   # recalculated optimum
      rmax = vals$rmax
    ) %>%
    select(-vals) %>%
    ungroup()
  # -------------------------------------------------------------------
  
  boot_results[[id]] <- boot_params
}



boots <- bind_rows(boot_results, .id = "curveid")

find_topt_optim <- function(a, b, c, topt) {
  neg_fun <- function(temp) -thomas_2012(temp = temp, a = a, b = b, c = c, topt = topt)
  opt <- optim(par = topt, fn = neg_fun)  # unbounded; starts at the fitted 'topt'
  list(topt_calc = opt$par, rmax = -opt$value)
}


boot_results2 <- purrr::pmap_dfr(
  boots[, c("a", "b", "c", "topt")],
  function(a, b, c, topt) {
    out <- find_topt_optim(a, b, c, topt)
    data.frame(
      tref = topt,           # the fitted parameter value
      topt_calc_func  = out$topt_calc,  # the true optimum (argmax)
      rmax_calc_func       = out$rmax        # the max predicted rate at topt_calc
    )
  }
)


boot_results_first <- bind_cols(boots, boot_results2)

write_csv(boot_results_first, "data-processed/tpc-boots-thomas-no-lag.csv") 
boot_results_first_fit <- read_csv("data-processed/tpc-boots-thomas-no-lag.csv")

# Error log - save ----------------------------------------------

write_csv(fit_error_log, "data-processed/fit_error_log-tpc-thomas-no-lag.csv")


# now fit the ones that failed the first time -----------------------------

d <- all_blocks2 %>% 
  filter(curve_id %in% c(fit_error_log$curve_id)) ### this needs to be the ones that didn't fit
d %>% distinct(curve_id) %>% tally()

# Split into list by curve_id
d_split <- split(d, d$curve_id)

# initialize --------------------------------------------------------------
boot_results <- list()

fit_error_log <- data.frame(
  curve_id = character(),
  message = character(),
  stringsAsFactors = FALSE
)

fit_results <- list()
start_vals_list <- list()
residuals_list <- list()
opt_fits_list <- list()



# Loop over each curve_id
for (id in names(d_split)) {
  d_curve <- d_split[[id]]
  message("2nd Processing curve_id = ", id)
  
  lower_lims <- c(a = 0, b = -10, c = 0, topt = -100)
  upper_lims <- c(a = 500, b = 10, c = 700, topt = 100)
  
  # Multistart for initial parameters
  multstart_fit <- tryCatch({
    nls_multstart(
      rate ~ thomas_2012(temp = temp, a, b, c, topt),
      data = d_curve,
      start_lower = c(a = 0.01, b = -1, c = 1, topt = 0.1),
      start_upper = c(a = 5, b = 1, c = 200, topt = 45),
      lower = lower_lims,
      upper = upper_lims,
      iter = 500,
      supp_errors = "Y",
      control = nls.control(maxiter = 500)
    )
  }, error = function(e) {
    message("nls_multstart error for curve_id = ", id, ": ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(multstart_fit)) next
  
  start_vals <- coef(multstart_fit)
  
  # Final fit using nlsLM
  fit <- tryCatch({
    nlsLM(
      rate ~ thomas_2012(temp = temp, a, b, c, topt),
      data = d_curve,
      start = start_vals,
      lower = lower_lims,
      upper = upper_lims,
      control = nls.lm.control(maxiter = 10000)
    )
  }, error = function(e) {
    message("nlsLM fit error for curve_id = ", id, ": ", conditionMessage(e))
    fit_error_log <- rbind(fit_error_log, data.frame(curve_id = id, message = conditionMessage(e)))
    return(NULL)
  })
  if (is.null(fit)) next
  
  # Save successful fit and values
  fit_results[[id]] <- list(fit = fit, data = d_curve)
  # Calculate residuals
  residuals_list[[id]] <- residuals(fit)
  
  # R2 
  obs <- d_curve$rate
  pred <- predict(fit)
  tss <- sum((obs - mean(obs))^2)
  rss <- sum((obs - pred)^2)
  r_squared <- 1 - (rss / tss)
  
  # Store optimal parameters + R2
  coefs <- coef(fit)
  
  # calc_params for optimal fit ----
  opt_vals <- get_topt_rmax(coefs["a"], coefs["b"], coefs["c"], coefs["topt"],
                            temps = seq(min(d_curve$temp), max(d_curve$temp), length.out = 200))
  
  opt_fits_list[[id]] <- data.frame(
    curve_id = id,
    a = coefs["a"],
    b = coefs["b"],
    c = coefs["c"],
    topt_param = coefs["topt"],      # fitted parameter from Thomas model
    topt_calc = opt_vals$topt,       # recalculated optimum
    rmax = opt_vals$rmax,
    tmax = coefs["topt"] + 0.5 * coefs["c"],
    r_squared = r_squared
  )  
  # bootstrapping (no logging)
  boot3 <- tryCatch({
    Boot(fit, method = 'residual', R = 10000)
  }, error = function(e) {
    message("Bootstrap error for curve_id = ", id, ": ", e$message)
    return(NULL)
  })
  if (is.null(boot3)) next
  
  # Extract and store bootstrap results
  boot_params <- boot3$t %>%
    as.data.frame() %>%
    drop_na() %>%
    mutate(iter = 1:n(),
           tmax = topt + (0.5 * c),
           curve_id = d_curve$curve_id[1])
  
  boot_params <- boot_params %>%
    rowwise() %>%
    mutate(vals = list(
      get_topt_rmax(a, b, c, topt,
                    temps = seq(min(d_curve$temp), max(d_curve$temp), length.out = 200))
    )) %>%
    mutate(
      topt_calc = vals$topt,   # recalculated optimum
      rmax = vals$rmax
    ) %>%
    select(-vals) %>%
    ungroup()
  # -------------------------------------------------------------------
  
  boot_results[[id]] <- boot_params
  
} 

boots <- bind_rows(boot_results, .id = "curveid")
boot_results2 <- purrr::pmap_dfr(
  boots[, c("a", "b", "c", "topt")],
  function(a, b, c, topt) {
    out <- find_topt_optim(a, b, c, topt)
    data.frame(
      tref = topt,           # the fitted parameter value
      topt_calc_func  = out$topt_calc,  # the true optimum (argmax)
      rmax_calc_func       = out$rmax        # the max predicted rate at topt_calc
    )
  }
)

boot_results_second_fit <- bind_cols(boots, boot_results2)
write_csv(boot_results_second_fit, "data-processed/tpc-boots-second-fit-thomas-no-lag.csv") ### now need to merge these with the other boots from the first fit 


boot_results_all <- bind_rows(boot_results_first_fit, boot_results_second_fit)
write_csv(boot_results_all, "data-processed/tpc-boots-all-thomas-no-lag.csv")


bs2 <- boot_results_all %>% 
  filter(topt > -99) %>% ### removing the bound hitting ones
  group_by(curveid) %>%
  sample_n(size = 474, replace = FALSE)### sampling 400 so all curve ids have the same number of boots (come back here!)

well_key <- all_blocks %>% 
  dplyr::select(strain, evolution_history) %>% 
  distinct()

bs3 <- left_join(bs2, well_key, by = c("curveid" = "strain")) %>% 
  group_by(curve_id, evolution_history) %>% 
  summarise(mean_topt = mean(topt_calc_func),
                 mean_tmax = mean(tmax))


bs4 <- left_join(bs2, well_key, by = c("curveid" = "strain")) %>% 
  group_by(curve_id, evolution_history) %>% 
  summarise(mean_topt = mean(topt_calc_func),
            mean_tmax = mean(tmax)) %>% 
  group_by(evolution_history) %>% 
  summarise(mean_topt2 = mean(mean_topt),
            mean_tmax2 = mean(mean_tmax),
            se_topt2 = std.error(mean_topt),
            se_tmax2 = std.error(mean_tmax))

ggplot() +
  geom_point(aes( x= evolution_history, y = mean_tmax), data = bs3, alpha = 0.5) +
  geom_pointrange(aes(x = evolution_history, y = mean_tmax2, ymin = mean_tmax2-se_tmax2, ymax = mean_tmax2 + se_tmax2), data = bs4) +ylab("Tmax") +
  xlab("Evolution history")
ggsave("figures/tmax-thomas-no-lag.png", width = 9, height = 6)

ggplot() +
  geom_point(aes( x= evolution_history, y = mean_topt), data = bs3, alpha = 0.5) +
  geom_pointrange(aes(x = evolution_history, y = mean_topt2, ymin = mean_topt2-se_topt2, ymax = mean_topt2 + se_topt2), data = bs4) +ylab("Topt") +
  xlab("Evolution history")
ggsave("figures/topt-thomas-no-lag.png", width = 9, height = 6)

ggplot() +
  # geom_point(aes( x= evolution_history, y = mean_topt), data = bs3, alpha = 0.5) +
  geom_pointrange(aes(x = evolution_history, y = mean_topt2, ymin = mean_topt2-se_topt2, ymax = mean_topt2 + se_topt2), data = bs4) +ylab("Topt") +
  xlab("Evolution history")
ggsave("figures/topt-thomas-averages-no-lag.png", width = 9, height = 6)



# draw the ribbons on the curves ------------------------------------------

preds_thomas <- bs2 %>% 
  group_by(curveid) %>% 
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(0, max(d$temp) + 10, length.out = 200))) %>%
  ungroup() %>%
  mutate(pred = thomas_2012(temp = temp, a,b,c, tref))


b2_gr <- preds_thomas %>% 
  group_by(temp, curveid) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975),
            mean_growth = mean(pred)) %>%
  ungroup() %>% 
  mutate(curve_id = curveid)

b2b <- b2_gr %>% 
  left_join(well_key, by = c("curve_id" = "strain"))

ggplot() +
  geom_ribbon(aes(x = temp, ymin = conf_lower, ymax = conf_upper, group = curveid), data = b2b, alpha = 0.5) +
  geom_line(aes(x = temp, y = mean_growth, group = curveid), data = b2b) +
  geom_point(aes(x = temp, y = rate), data = all_blocks2) +
  ylim(-2, 13) +
  facet_wrap( ~ curve_id) +ylab("Growth rate") + xlab("Temperature") + coord_cartesian()
ggsave("figures/all-tpcs-boots-no-lag.png", width = 16, height = 14)  



ggplot() +
  # geom_ribbon(aes(x = temp, ymin = conf_lower, ymax = conf_upper, group = curveid), data = b2_gr, alpha = 0.5) +
  geom_line(aes(x = temp, y =  mean_growth, group = curveid, color = evolution_history), data = b2b) +
  geom_point(aes(x = temp, y = rate, color = evolution_history), data = all_blocks2) +
  ylim(0, 12) +
  ylab("Growth rate") + xlab("Temperature") +
  facet_wrap( ~ evolution_history, scales = "free") + xlim(10, 45)
ggsave("figures/all-tpcs-boots-mean-no-lag.png", width = 10, height = 6)  


ggplot() +
  # geom_ribbon(aes(x = temp, ymin = conf_lower, ymax = conf_upper, group = curveid), data = b2_gr, alpha = 0.5) +
  geom_line(aes(x = temp, y =  mean_growth, group = curveid, color = curve_id), data = filter(b2b, evolution_history == "35 evolved")) +
  geom_point(aes(x = temp, y = rate, color = strain), data = filter(all_blocks2, evolution_history == "35 evolved")) +
  ylim(0, 12) +
  ylab("Growth rate") + xlab("Temperature") +
  facet_wrap( ~ evolution_history, scales = "free") + xlim(10, 45)
ggsave("figures/all-tpcs-boots-mean-35-no-lag.png", width = 10, height = 6)  
