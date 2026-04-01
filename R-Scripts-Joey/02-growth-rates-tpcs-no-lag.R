

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

### Notes that Nick thinks the Tecan (used for these wasn't reliable at 42... maybe we take out those data from the TPC?)


ods <- read_csv("data-processed/all-temps-od.csv")

ods %>% 
  filter(well == "B3", test_temperature == "42") %>% View

ods %>% 
  ggplot(aes(x = time, y = OD, color = factor(test_temperature))) + geom_point()

od2 <- ods %>% 
  group_by(well, test_temperature) %>% ## it looks like this grouping will work because we don't have temperatures tested on multiple days, so this is enough to uniquely ID the wells
  mutate(days = as.duration(time - min(time)) / ddays(1))


od2 %>% 
  filter(test_temperature %in% c("41", "42")) %>% 
  ggplot(aes(x = days, y = OD, color = factor(test_temperature))) + geom_point() 

od2 %>% 
  ggplot(aes(x = days, y = OD, color = factor(test_temperature))) + geom_point() +
  facet_grid(test_temperature ~ treatment, scales = "free")


# estimate growth rates ---------------------------------------------------

gdat_all <- od2 %>%
  mutate(ln_abundance = log(OD)) %>% 
  mutate(unique_id = paste(well, test_temperature,treatment, sep = "_")) %>% 
  group_by(unique_id) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,
    id = unique(.$unique_id),
    plot.best.Q = TRUE,
    fpath = "figures/tpc-growth/" 
  ))


summary_df <- gdat_all %>%
  summarise(
    unique_id,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) 





s2 <- summary_df %>% 
  separate(unique_id, into = c("well", "test_temperature", "treatment"), sep = "_", remove = FALSE)
write_csv(s2, "data-processed/growth-tools-rates-tpcs.csv")
s2 <- read_csv("data-processed/growth-tools-rates-tpcs.csv")

s2 %>% 
  filter(treatment != "water") %>% 
  filter(treatment == "NA") %>%
  ggplot(aes(x = test_temperature, y = mu, color = treatment)) + geom_point() ### something looks wrong here... the mus are way too high for water... seems like it might be well plate key issue, where wells have been mis-assigned

s2 %>% 
  filter(treatment == "fRS585") %>% 
  mutate(test_temperature = as.numeric(test_temperature)) %>% 
  ggplot(aes(x = test_temperature, y = mu, color = treatment)) + geom_point() +
  ylab("Growth rate (per day)") + xlab("Temperature")
ggsave("figures/mu-temperature.png", width = 8, height = 6)


### next step is to fit the tpc

s3 <- s2 %>% 
  filter(treatment == "fRS585") %>% 
  mutate(temp = as.numeric(test_temperature)) 

start_vals <- get_start_vals(s3$test_temperature, s3$mu, model_name = "thomas_2012")
lower_lims <- c(a = 0, b = -10, c = 0, topt = -100)
upper_lims <- c(a = 100, b = 10, c = 700, topt = 100)

fit <- nlsLM(mu ~ thomas_2012(temp = temp, a, b, c, topt), data = s3,
        start = start_vals,
        lower = lower_lims,
        upper = upper_lims,
        control = nls.lm.control(maxiter = 1000))

summary(fit)

params <- coef(fit)

temp_seq <- seq(min(s3$temp-2), max(s3$temp+2), length.out = 200)


predicted_mu <- thomas_2012(temp = temp_seq, 
                            a = params["a"], 
                            b = params["b"], 
                            c = params["c"], 
                            topt = params["topt"])

fit_df <- data.frame(temp = temp_seq, mu = predicted_mu)
write_csv(fit_df, "data-processed/frs585-TPC.csv")

# Step 5: Plot original data and fitted curve
ggplot(s3, aes(x = temp, y = mu)) +
  geom_point(color = "grey", size = 2) +  # observed data
  geom_line(data = fit_df, aes(x = temp, y = mu), color = "black", size = 1) +  # model fit
  labs(title = "N. glabrata (fRS585)", x = "Temperature", y = "Growth rate (per day)") +
  theme_minimal() +ylim(0, 11.5)
ggsave("figures/tpc-frs585.png", width = 6, height = 4)


### bootstrap the curve
boot_fit <- Boot(fit, method = 'residual', R = 1000)  

boot_params <- boot_fit$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n(),
         tmax = topt + (0.5 * c))

write_csv(boot_params, "data-processed/frs585-boot-params.csv")

boot_params <- read_csv("data-processed/frs585-boot-params.csv")

b2 <- boot_params %>% 
  summarise(mean_tmax = mean(tmax),
            se_tmax = std.error(tmax))


ggplot() + 
  geom_point(color = "grey", size = 2, data = s3, aes(x = temp, y = mu)) +  # observed data
  geom_line(data = fit_df, aes(x = temp, y = mu), color = "black", size = 1) +  # model fit
  labs(title = "N. glabrata (fRS585)", x = "Temperature", y = "Growth rate (per day)") +
  theme_minimal() +ylim(0, 11.5) +
  geom_pointrange(aes(xmin = mean_tmax - se_tmax, xmax = mean_tmax + se_tmax, y = 0, x = mean_tmax), data = b2, color = "red")
ggsave("figures/tpc-frs585.png", width = 6, height = 4)


### now for fun, let's bring in the growth rates from the evolution experiment to plot along side

evolution_expt <- read_csv("data-processed/all-blocks-growth.csv") %>% 
  filter(evolution_history != "Fluconazole evolved") %>% 
  filter(evolution_history != "Caspofungin evolved") 

e2 <- evolution_expt %>% 
  group_by(test_temperature, evolution_history) %>% 
  summarise(mean_growth = mean(mu),
            se_growth = std.error(mu))


ggplot() + 
  geom_point(color = "grey", size = 2, data = s3, aes(x = temp, y = mu)) +  # observed data
  geom_line(data = fit_df, aes(x = temp, y = mu), color = "black", size = 1) +  # model fit
  labs(title = "N. glabrata (fRS585)", x = "Temperature", y = "Growth rate (per day)") +
  theme_minimal() +ylim(0, 11.5) +
  # geom_pointrange(aes(xmin = mean_tmax - se_tmax, xmax = mean_tmax + se_tmax, y = 0, x = mean_tmax), data = b2, color = "red") +
  geom_pointrange(aes(ymin = mean_growth - se_growth, ymax = mean_growth + se_growth, y = mean_growth, x = test_temperature, color = evolution_history), data = e2) 
ggsave("figures/tpc-evolution-results.png", width = 8, height = 6)

ggplot() + 
  geom_point(color = "grey", size = 2, data = s3, aes(x = temp, y = mu-.8)) +  # observed data
  geom_line(data = fit_df, aes(x = temp, y = mu-.8), color = "black", size = 1) +  # model fit
  labs(title = "N. glabrata (fRS585)", x = "Temperature", y = "Growth rate (per day)") +
  theme_minimal() +ylim(0, 10) +
  # geom_pointrange(aes(xmin = mean_tmax - se_tmax, xmax = mean_tmax + se_tmax, y = 0, x = mean_tmax), data = b2, color = "red") +
  geom_pointrange(aes(ymin = mean_growth - se_growth, ymax = mean_growth + se_growth, y = mean_growth, x = test_temperature, color = evolution_history), data = e2) 
ggsave("figures/tpc-evolution-results-tpc-shifted.png", width = 8, height = 6)



### Fit tpcs to the growth curves from the biotek experiment

all_blocks <- read_csv("data-processed/all-blocks-growth.csv")


growth_35evolved <- all_blocks %>% 
  filter(evolution_history == "35 evolved") %>% 
  mutate(temp = test_temperature)


growth_35evolved %>% 
  ggplot(aes(x = test_temperature, y = mu, color = factor(block))) + geom_point()



start_vals <- get_start_vals(growth_35evolved$temp, growth_35evolved$mu, model_name = "thomas_2012")
lower_lims <- c(a = 0, b = 0, c = 0, topt = 0)
upper_lims <- c(a = 100, b = 10, c = 700, topt = 100)





# choose model
mod = 'sharpschoolhigh_1981'

d <- growth_35evolved %>% 
  mutate(temp = test_temperature, rate = mu)

# get start vals
start_vals <- get_start_vals(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')

# get limits
low_lims <- get_lower_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')


fit <- nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                     data = d,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

summary(fit)


calc_params(fit) %>%
  # round for easy viewing
  mutate_all(round, 2)


# predict new data
new_data <- data.frame(temp = seq(min(d$temp), max(d$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
ggplot(d, aes(temp, rate)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Growth rate',
       title = 'Growth rates across temperatures for 35 evolved')


d40 <- all_blocks %>% 
  filter(evolution_history == "40 evolved") %>% 
  mutate(temp = test_temperature, rate = mu)




# get start vals
start_vals <- get_start_vals(d40$temp, d40$rate, model_name = 'sharpeschoolhigh_1981')

# get limits
low_lims <- get_lower_lims(d40$temp, d40$rate, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(d40$temp, d40$rate, model_name = 'sharpeschoolhigh_1981')


fit40 <- nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                     data = d40,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

summary(fit40)


calc_params(fit40) %>%
  # round for easy viewing
  mutate_all(round, 2)

calc_params(fit) %>%
  # round for easy viewing
  mutate_all(round, 2)

# predict new data
new_data40 <- data.frame(temp = seq(min(d40$temp), max(d40$temp), 0.5))
preds40 <- augment(fit40, newdata = new_data40)

# plot data and model fit
ggplot(d40, aes(temp, rate)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds40, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Growth rate',
       title = 'Growth rates across temperatures for 40 evolved')



dcasp <- all_blocks %>% 
  filter(evolution_history == "Caspofungin evolved") %>% 
  mutate(temp = test_temperature, rate = mu)

# get start vals
start_vals <- get_start_vals(dcasp$temp, dcasp$rate, model_name = 'sharpeschoolhigh_1981')

# get limits
low_lims <- get_lower_lims(dcasp$temp, dcasp$rate, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(dcasp$temp, dcasp$rate, model_name = 'sharpeschoolhigh_1981')


fitcasp <- nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                       data = dcasp,
                       iter = 500,
                       start_lower = start_vals - 10,
                       start_upper = start_vals + 10,
                       lower = low_lims,
                       upper = upper_lims,
                       supp_errors = 'Y')

summary(fitcasp)
confint(fitcasp)

calc_params(fitcasp) %>%
  # round for easy viewing
  mutate_all(round, 2)

# predict new data
new_datacasp <- data.frame(temp = seq(min(dcasp$temp), max(dcasp$temp), 0.5))
predscasp <- augment(fitcasp, newdata = new_datacasp)


dfluc <- all_blocks %>% 
  filter(evolution_history == "Fluconazole evolved") %>% 
  mutate(temp = test_temperature, rate = mu)

# get start vals
start_vals <- get_start_vals(dfluc$temp, dfluc$rate, model_name = 'sharpeschoolhigh_1981')

# get limits
low_lims <- get_lower_lims(dfluc$temp, dfluc$rate, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(dfluc$temp, dfluc$rate, model_name = 'sharpeschoolhigh_1981')


fitfluc <- nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                         data = dfluc,
                         iter = 500,
                         start_lower = start_vals - 10,
                         start_upper = start_vals + 10,
                         lower = low_lims,
                         upper = upper_lims,
                         supp_errors = 'Y')

summary(fitfluc)
confint(fitfluc)

calc_params(fitfluc) %>%
  # round for easy viewing
  mutate_all(round, 2)

# predict new data
new_datafluc <- data.frame(temp = seq(min(dfluc$temp), max(dfluc$temp), 0.5))
predsfluc <- augment(fitfluc, newdata = new_datafluc)




dwild <- all_blocks %>% 
  filter(evolution_history == "fRS585") %>% 
  mutate(temp = test_temperature, rate = mu)

# get start vals
start_vals <- get_start_vals(dwild $temp, dwild $rate, model_name = 'sharpeschoolhigh_1981')

# get limits
low_lims <- get_lower_lims(dwild $temp, dwild $rate, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(dwild $temp, dwild $rate, model_name = 'sharpeschoolhigh_1981')


fitwild <- nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                         data = dwild,
                         iter = 500,
                         start_lower = start_vals - 10,
                         start_upper = start_vals + 10,
                         lower = low_lims,
                         upper = upper_lims,
                         supp_errors = 'Y')

summary(fitwild)
confint(fitwild)

calc_params(fitwild) %>%
  # round for easy viewing
  mutate_all(round, 2)

calc_params(fit) %>%
  # round for easy viewing
  mutate_all(round, 2)

# predict new data
new_datawild <- data.frame(temp = seq(min(dwild$temp), max(dwild$temp), 0.5))
predswild <- augment(fitwild, newdata = new_datawild)










ggplot(d40, aes(temp, rate)) +
  geom_point() +
  geom_point(data = d, aes(temp, rate), color = "blue") +
  geom_point(data = dwild, aes(temp, rate), color = "green") +
  geom_point(data = dcasp, aes(temp, rate), color = "orange") +
  geom_point(data = dfluc, aes(temp, rate), color = "purple") +
  geom_point(data = d40, aes(temp, rate), color = "red") +
  geom_line(aes(temp, .fitted), predswild, col = 'green', size = 1.5) +
  geom_line(aes(temp, .fitted), predscasp, col = 'orange', size = 1.5) +
  geom_line(aes(temp, .fitted), predsfluc, col = 'purple', size = 1.5) +
  geom_line(aes(temp, .fitted), preds, col = 'blue', size = 1.5) +
  geom_line(aes(temp, .fitted), preds40, col = 'red', size = 1.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Growth rate',
       title = '40 evolv (red); 35 evolv (blue), wild type (green); caspofungin evol (orange), fluc evol (purple)')
ggsave("figures/tpcs-descendants.png", width = 8, height = 6)
ggsave("figures/tpcs-descendants-drug.png", width = 8, height = 6)



# start again for 40C with fitting all pops -------------------------------

View(all_blocks)

# load in data
d <- all_blocks %>% 
  mutate(curve_id = strain) %>% 
  mutate(temp = test_temperature,
         rate = mu)

# Split into list by curve_id
d_split <- split(d, d$curve_id)

# Initialize result list and fit-error log
boot_results <- list()
fit_error_log <- data.frame(
  curve_id = character(),
  message = character(),
  stringsAsFactors = FALSE
)

fit_results <- list()
residuals_list <- list()


# Loop over each curve_id
for (id in names(d_split)) {
  d_curve <- d_split[[id]]
  message("Processing curve_id = ", id)
  
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
  upper_lims <- c(a = 100, b = 10, c = 700, topt = 100)
  
  # Attempt model fit (log errors here only)
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
  # Calculate residuals
  residuals_list[[id]] <- residuals(fit)
  
  # bootstrapping (no logging)
  boot3 <- tryCatch({
    Boot(fit, method = 'residual', R = 5000)
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
  
  boot_results[[id]] <- boot_params
}

# Combine results into one data frame
boots <- bind_rows(boot_results, .id = "curveid")
# write_csv(boots, "data-processed/all-tpcs-boots-filtered.csv") 
write_csv(boots, "data-processed/all-tpcs-boots.csv") 
boots <- read_csv("data-processed/all-tpcs-boots.csv")

well_key <- all_blocks %>% 
  dplyr::select(strain, evolution_history) %>% 
distinct()


boots2 <- boots %>% 
  left_join(well_key, by = c("curve_id" = "strain")) %>% 
  filter(topt > -99, a < 100)


b2b <- boots2 %>% 
  group_by(curveid, evolution_history) %>% 
  summarise(mean_tmax = mean(tmax),
            se_tmax = std.error(tmax)) 

b3 <- b2b %>% 
  group_by(evolution_history) %>% 
  summarise(mean_tmax2 = mean(mean_tmax),
            se_tmax2 = std.error(mean_tmax))
  
  ggplot() +
    geom_point(aes(x = evolution_history, y = mean_tmax), data = b2b) +
    geom_pointrange(aes(x = evolution_history, ymin = mean_tmax2 - se_tmax2, ymax = mean_tmax2 + se_tmax2, y = mean_tmax2), data = b3)
  

# try again ---------------------------------------------------------------
#### now try with no lag
  
all_blocks_no_lag <- read_csv("data-processed/all-blocks-growth-no-lag.csv")
  
d <- all_blocks_no_lag %>% 
    mutate(curve_id = strain) %>% 
    mutate(temp = test_temperature, 
           rate = mu)

well_key <- all_blocks_no_lag %>% 
  dplyr::select(strain, evolution_history) %>% 
  distinct()  
  
  
df <- d  
  fit_sharp_high <- function(df, model_name = "sharpeschoolhigh_1981", tref = 25, iter = 2000) {
    # get start values and bounds
    starts <- get_start_vals(df$temp, df$rate, model_name = model_name)
    lower <- get_lower_lims(df$temp, df$rate, model_name = model_name)
    upper <- get_upper_lims(df$temp, df$rate, model_name = model_name)
    
    # construct formula with tref fixed
    param_names <- names(starts)
    rhs <- paste0(model_name, "(temp, ", paste(param_names, collapse = ", "), ", tref = ", tref, ")")
    formula <- as.formula(paste("rate ~", rhs))
    
    # fit with error handling
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
      structure(list(error = TRUE, message = e$message), class = "tpc_fit_error")
    })
  }
  fits <- df %>%
    group_by(curve_id) %>%
    nest() %>%
    mutate(fit = map(data, ~ fit_sharp_high(.x, tref = 25, iter = 2000)))

  fit_status <- fits %>%
    mutate(ok = map_lgl(fit, ~ !inherits(.x, "tpc_fit_error") && !is.null(.x))) %>%
    select(curve_id, ok)
  
  print(fit_status)

  
  preds <- fits %>%
    filter(map_lgl(fit, ~ !inherits(.x, "tpc_fit_error") && !is.null(.x))) %>%
    mutate(pred = map2(data, fit, ~ {
      tibble(temp = seq(min(.x$temp)-10, max(.x$temp) + 5, length.out = 200)) %>%
        mutate(fit = predict(.y, newdata = tibble(temp = temp)))
    })) %>%
    dplyr::select(curve_id, pred) %>%
    unnest(pred)
  
preds2 <- preds %>% 
  left_join(well_key, by = c("curve_id" = "strain"))
  
  ggplot() +
    geom_point(data = df, aes(x = temp, y = rate, color = evolution_history)) +
    geom_line(data = preds2, aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1) +
    facet_wrap(~ evolution_history, scales = "free_y") +
    theme_minimal() +
    labs(
      x = "Temperature (°C)",
      y = "Growth rate",
      title = "Sharpe–Schoolfield high fits per population"
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


traits2 <- traits %>% 
  separate(curve_id, into = c("temp", "well"), sep = "_", remove = FALSE)

ggplot() +
  geom_point(aes(x = evolution_history, y = topt, color = well), data = filter(traits2, evolution_history %in% c("35 evolved", "40 evolved"))) +
  geom_line(aes(x = evolution_history, y = topt, color = well, group = well), data = filter(traits2, evolution_history %in% c("35 evolved", "40 evolved"))) +
  geom_pointrange(aes(x = evolution_history, y = mean_topt, ymin = mean_topt - se_topt, ymax = mean_topt + se_topt), data = filter(t4, evolution_history %in% c("35 evolved", "40 evolved"))) +
  ylab("Topt") + xlab("Evolution history") 
ggsave("figures/topts-temperature-treatments.png", width = 6, height = 4)


traits2b <- traits2 %>% 
  mutate(evolution_history = as.factor(evolution_history)) %>% 
  mutate(evolution_history = fct_relevel(evolution_history, "fRS585", "35 evolved", "40 evolved", "Caspofungin evolved"))

levels(traits2b$evolution_history)


t4b <- t4 %>% 
  mutate(evolution_history = as.factor(evolution_history)) %>% 
  mutate(evolution_history = fct_relevel(evolution_history, "fRS585", "35 evolved", "40 evolved", "Caspofungin evolved"))
  

anc_topt <- t4b$mean_topt[t4b$evolution_history == "fRS585"]

ggplot() +
  geom_hline(yintercept = anc_topt, color = "grey") +
  geom_point(aes(x = evolution_history, y = topt, color = well), data = traits2b) +
  geom_line(aes(x = evolution_history, y = topt, color = well, group = well), data = traits2b) +
  geom_pointrange(aes(x = evolution_history, y = mean_topt, ymin = mean_topt - se_topt, ymax = mean_topt + se_topt), data = t4b) +
  ylab("Topt (°C)") + xlab("Evolution history") +
  guides(color = guide_legend(title = "Population"))

ggsave("figures/topts-temperature-treatments-all.png", width = 8, height = 5)



ggplot() +
  geom_hline(yintercept = anc_topt, color = "grey") +
  geom_point(aes(x = evolution_history, y = topt), data = traits2b, alpha = 0.5) +
  # geom_line(aes(x = evolution_history, y = topt, group = well), data = traits2b) +
  geom_pointrange(aes(x = evolution_history, y = mean_topt, ymin = mean_topt - se_topt, ymax = mean_topt + se_topt), data = t4b) +
  ylab("Topt (°C)") + xlab("Evolution history") +
  guides(color = guide_legend(title = "Population"))

ggsave("figures/topts-temperature-treatments-all-grey.png", width = 8, height = 5)





# sidebar to see the correlation between thermal traits and drug r --------

topts <- traits2b %>% 
  select(evolution_history, well, topt) %>% 
  filter(evolution_history %in% c("35 evolved", "40 evolved"))
 
  ### has the topts
casp <- traits2b %>% 
  mutate(evolution_history = case_when(grepl("35", evolution_history) ~ "35 evolved",
                                       grepl("40", evolution_history) ~ "40 evolved",
                                      TRUE ~ evolution_history)) %>% 
  filter(evolution_history %in% c("35 evolved", "40 evolved")) %>% 
  filter(drug == "caspofungin") ### has drug tolerances

all_traits <- left_join(casp, traits2b)


all_traits %>% 
  filter(mean_ic503 < 0.04) %>% 
  ggplot(aes(x = topt, y = mean_ic503)) + geom_point() +
  geom_smooth(method = "lm")



# calculate the average shift between the frs and the 40C -----------------

traits2 %>% 
  mutate(topt_shift = topt - 37.78100) %>% ### this is the frs topt
  ungroup() %>% 
  summarise(mean_topt_shift = mean(topt_shift)) ### avg shift is 	0.32045 for the 40C relative to the frs


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
    rate_41 = map_dbl(fit, predict_at_temp, temp = 41),
    rate_42 = map_dbl(fit, predict_at_temp, temp = 42)
  ) %>%
  select(curve_id, params, traits, rate_40, rate_42, rate_41)

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
            se_41 = std.error(rate_41))

ggplot() +
  geom_pointrange(aes(x = evolution_history, y = mean_40, ymin = mean_40 - se_40, ymax = mean_40 + se_40), data = g2) +
  geom_point(aes(x = evolution_history, y = rate_40), data = growth_40, alpha = 0.5) +
  ylab("Growth at 40C") + xlab("Evolution history")
ggsave("figures/growth-40-no-lag.png", width = 9, height = 6)

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

temp_at_rate <- function(model, target_rate = 2,
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



temp_at_fract <- function(model, fraction = 0.2,
                         from = 40, to = 50, by = 0.001) {
  # evaluate on grid
  temps <- seq(from, to, by)
  preds <- tryCatch(
    predict(model, newdata = data.frame(temp = temps)),
    error = function(e) rep(NA_real_, length(temps))
  )
  
  # # if model never reaches target_rate → return NA
  # if (all(is.na(preds)) || max(preds, na.rm = TRUE) < target_rate)
  #   return(NA_real_)
  
  # target rate = fraction of max
  max_rate <- max(preds, na.rm = TRUE)
  target_rate <- fraction * max_rate
  
  
  
  # find temperature closest to target
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
    temp_rate = map_dbl(fit, temp_at_rate, target_rate = 2),
    temp_20pct = map_dbl(fit, temp_at_fract, fraction = 0.2)
  ) %>%
  select(curve_id, params, traits, rate_40, rate_42, temp_20pct, rate_41, temp_rate)


growth_full <- left_join(param_table_full, well_key, by = c("curve_id" = "strain")) %>% 
  filter(!grepl("Fluc", evolution_history))

g3 <- growth_full %>% 
  group_by(evolution_history) %>%
  summarise(mean_40 = mean(rate_40),
            se_40 = std.error(rate_40),
            mean_42 = mean(rate_42),
            se_42 = std.error(rate_42),
            mean_tmax = mean(temp_rate),
            se_tmax = std.error(temp_rate),
            mean_tmax_20 = mean(temp_20pct),
            se_tmax_20 = std.error(temp_20pct))

  
ggplot() +
  geom_pointrange(aes(x = evolution_history, y = mean_tmax, ymin = mean_tmax - se_tmax, ymax = mean_tmax + se_tmax), data = g3) +
  geom_point(aes(x = evolution_history, y = temp_rate), data = growth_full, alpha = 0.5) +
  ylab("Temperature at growth rate of 2") + xlab("Evolution history")
ggsave("figures/tmax_est-no-lag.png", width = 9, height = 6)


growth_full_2 <- growth_full %>% 
separate(curve_id, into = c("temp", "well"), sep = "_", remove = FALSE)

growth_full_2b <- growth_full_2 %>% 
  mutate(evolution_history = as.factor(evolution_history)) %>% 
  mutate(evolution_history = fct_relevel(evolution_history, "fRS585", "35 evolved", "40 evolved", "Caspofungin evolved"))


g3b <- g3 %>% 
  mutate(evolution_history = as.factor(evolution_history)) %>% 
  mutate(evolution_history = fct_relevel(evolution_history, "fRS585", "35 evolved", "40 evolved", "Caspofungin evolved"))

anc_tmax <- g3b$mean_tmax_20[g3b$evolution_history == "fRS585"]

ggplot() +
  geom_hline(yintercept =  anc_tmax, color = "grey") +
  geom_pointrange(aes(x = evolution_history, y = mean_tmax_20, ymin = mean_tmax_20 - se_tmax_20, ymax = mean_tmax_20 + se_tmax_20), data = g3b) +
  geom_point(aes(x = evolution_history, y = temp_20pct, color = well), data = growth_full_2b) +
  geom_line(aes(x = evolution_history, y = temp_20pct, color = well, group = well), data = growth_full_2b) +
  geom_pointrange(aes(x = evolution_history, y = mean_tmax_20, ymin = mean_tmax_20 - se_tmax_20, ymax = mean_tmax_20 + se_tmax_20), data = g3b) +
  ylab("Temperature at 20 percent max growth (°C)") + xlab("Evolution history") +
  guides(color = guide_legend(title = "Population"))
# ggsave("figures/tmax_est-50pct-no-lag.png", width = 9, height = 6)
ggsave("figures/tmax_est-20pct-no-lag.png", width = 8, height = 5)


ggplot() +
  geom_hline(yintercept =  anc_tmax, color = "grey") +
  geom_pointrange(aes(x = evolution_history, y = mean_tmax_20, ymin = mean_tmax_20 - se_tmax_20, ymax = mean_tmax_20 + se_tmax_20), data = g3b) +
  geom_point(aes(x = evolution_history, y = temp_20pct), data = growth_full_2b, alpha = 0.5) +
  # geom_line(aes(x = evolution_history, y = temp_20pct, color = well, group = well), data = growth_full_2b) +
  geom_pointrange(aes(x = evolution_history, y = mean_tmax_20, ymin = mean_tmax_20 - se_tmax_20, ymax = mean_tmax_20 + se_tmax_20), data = g3b) +
  ylab("Temperature at 20 percent max growth (°C)") + xlab("Evolution history") +
  guides(color = guide_legend(title = "Population"))
# ggsave("figures/tmax_est-50pct-no-lag.png", width = 9, height = 6)
ggsave("figures/tmax_est-20pct-no-lag-grey.png", width = 8, height = 5)




ggplot() +
  geom_point(aes(x = evolution_history, y = temp_20pct), data = growth_full, alpha = 0.5) +
  geom_pointrange(aes(x = evolution_history, y = mean_tmax_20, ymin = mean_tmax_20 - se_tmax_20, ymax = mean_tmax_20 + se_tmax_20), data = g3) +
  ylab("Temperature at 20 percent max growth") + xlab("Evolution history")
# ggsave("figures/tmax_est-50pct-no-lag.png", width = 9, height = 6)
ggsave("figures/tmax_est-20pct-no-lag.png", width = 9, height = 6)


# merge all the traits ----------------------------------------------------

p2 <- param_table_full %>% 
  select(curve_id, rate_40, rate_42, rate_41, temp_rate, temp_20pct)

p3 <- left_join(traits, p2)

write_csv(p3, "data-processed/all-SS-traits.csv")


p4 <- p3 %>% 
  group_by(evolution_history) %>% 
  summarise_all(.funs = c("mean", "std.error"))


# plots -------------------------------------------------------------------

ggplot() +
  geom_vline(xintercept = 35, color = "darkgrey", linetype = "dashed") +
  geom_vline(xintercept = 40, color = "darkgrey", linetype = "dashed") +
  geom_hline(yintercept = 1, color = "darkgrey") +
  geom_vline(aes(xintercept = topt, color = evolution_history), data = filter(p3, evolution_history == "fRS585")) +
  geom_point(aes(x = topt, y = 0, color = evolution_history), data = p3, alpha = 0.2, size = 4) +
  geom_pointrange(aes(x = topt_mean, xmin =topt_mean - topt_std.error, xmax = topt_mean + topt_std.error,  y = 0, color = evolution_history), data = p4, position = position_dodge(width = 0.7)) +
  
  geom_line(data = filter(preds2, evolution_history != "Fluconazole evolved"), aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1, alpha = 0.5) +
  geom_point(data = filter(df, evolution_history != "Fluconazole evolved"), aes(x = temp, y = rate, color = evolution_history), size = 1.5) +
  labs(
    x = "Temperature (°C)",
    y = "Growth rate",
  ) + scale_color_manual(values = c( "#019875FF", "#FF847CFF", "#C0392BFF", "darkgrey")) 

ggsave("figures/tpcs-all-no-lag-single.png", width = 10, height = 5)  



# big plot of TPCs --------------------------------------------------------

ggplot() +
  geom_vline(xintercept = 35, color = "darkgrey") +
  geom_vline(xintercept = 40, color = "darkgrey") +
  geom_hline(yintercept = 1, color = "darkgrey") +
  # geom_pointrange(aes(x = topt_mean, xmin = topt_mean - topt_std.error, xmax = topt_mean + topt_std.error,  y = 0, color = evolution_history), data = p4, position = position_dodge(width = 0.7)) +
  geom_line(data = filter(preds2, evolution_history == "fRS585"), aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1, alpha = 0.5, color = "#019875FF") +
  geom_point(data = filter(df, evolution_history == "fRS585"), aes(x = temp, y = rate, color = evolution_history), size = 1.5, color = "#019875FF") +
  
  geom_line(data = filter(preds2, evolution_history == "35 evolved"), aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1, alpha = 0.5, color = "#FF847CFF") +
  geom_point(data = filter(df, evolution_history == "35 evolved"), aes(x = temp, y = rate, color = evolution_history), size = 1.5, color = "#FF847CFF") +

  geom_line(data = filter(preds2, evolution_history == "40 evolved"), aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1, alpha = 0.5, color = "#C0392BFF") +
  geom_point(data = filter(df, evolution_history == "40 evolved"), aes(x = temp, y = rate, color = evolution_history), size = 1.5, color = "#C0392BFF") +

  # geom_line(data = filter(preds2, evolution_history == "Caspofungin evolved"), aes(x = temp, y = fit, color = evolution_history, group = curve_id), size = 1, alpha = 0.5, color = "#2A363BFF") +
  # geom_point(data = filter(df, evolution_history == "Caspofungin evolved"), aes(x = temp, y = rate, color = evolution_history), size = 1.5, color = "#2A363BFF") +
labs(
  x = "Temperature (°C)",
  y = expression("Growth rate (" * day^{-1} * ")")
) + scale_color_manual(values = c("#FF847CFF","#C0392BFF", "#2A363BFF","#019875FF")) +
  theme(legend.position = "none")
ggsave("figures/tpcs-all-no-lag-single-frs-35-40-casp.png", width = 6, height = 4) 
ggsave("figures/tpcs-all-no-lag-single-frs-35-40.png", width = 6, height = 4)  



ggsave("figures/tpcs-all-no-lag-single.png", width = 6, height = 4)  

unique(p4$evolution_history)


## "Fluconazole evolved" "35 evolved"          "fRS585"              "Caspofungin evolved" "40 evolved"   

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

mod1f <- lm(temp_20pct ~ evolution_history, data = p3)
summary(mod1f)

visreg(mod1f, "evolution_history")


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



# new graphs for seminar --------------------------------------------------

traits <- "data-processed/all-SS-traits.csv"
