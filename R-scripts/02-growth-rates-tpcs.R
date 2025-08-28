

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
