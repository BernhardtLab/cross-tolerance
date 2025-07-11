

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



# load data ---------------------------------------------------------------

ods <- read_csv("data-processed/all-temps-od.csv")


ods %>% 
  ggplot(aes(x = time, y = OD, color = factor(test_temperature))) + geom_point()

od2 <- ods %>% 
  group_by(well, test_temperature) %>% ## it looks like this grouping will work because we don't have temperatures tested on multiple days, so this is enough to uniquely ID the wells
  mutate(days = as.duration(time - min(time)) / ddays(1))


od2 %>% 
  ggplot(aes(x = days, y = OD, color = factor(test_temperature))) + geom_point()


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

s2 %>% 
  ggplot(aes(x = test_temperature, y = mu, color = treatment)) + geom_point() ### something looks wrong here... the mus are way too high for water... seems like it might be well plate key issue, where wells have been mis-assigned


           