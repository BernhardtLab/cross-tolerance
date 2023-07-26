###SVETA
#TPC USING PACKAGE rTPC

#packages
library(usethis)
library(devtools)
library(nls.multstart)
library(broom)
library(tidyverse)
library(remotes)
library(rTPC)
library(readxl)

#all models
get_model_names()

#copied briere from personal lab meetings r script
mod3 = 'briere2_1999'

start_vals_briere <- get_start_vals(d$temp, d$rate, model_name = 'briere2_1999')
low_lims_briere <- get_lower_lims(d$temp, d$rate, model_name = 'briere2_1999')
upper_lims_briere <- get_upper_lims(d$temp, d$rate, model_name = 'briere2_1999')

fit_briere <- nls_multstart(rate~briere2_1999(temp = temp, tmin, tmax, a, b), 
                            data = d,
                            iter = 500,
                            start_lower = get_start_vals(d$temp, d$rate, model_name = 'briere2_1999') - 10,
                            start_upper = get_start_vals(d$temp, d$rate, model_name = 'briere2_1999') + 10,
                            lower = get_lower_lims(d$temp, d$rate, model_name = 'briere2_1999'),
                            upper = get_upper_lims(d$temp, d$rate, model_name = 'briere2_1999'),
                            supp_errors = 'Y')
fit_briere 
#Nonlinear regression model
#model: rate ~ briere2_1999(temp = temp, tmin, tmax, a, b)
#data: data
#tmin     tmax        a        b 
#17.61509 42.15999  0.00372  1.31615 
#residual sum-of-squares: 326.7
#Number of iterations to convergence: 42 
#Achieved convergence tolerance: 1.49e-08

calc_params(fit_briere) %>%
  mutate_all(round, 2)
#WITHOUT 20 AND 30 DEG
#rmax  topt ctmin ctmax    e   eh    q10 thermal_safety_margin thermal_tolerance breadth skewness
# 10.22 33.83 17.62 42.09 4.31 1.07 314.61                  8.26             24.48   10.25     3.24

#WITH 30 DEG, WITHOUT 20 DEG
# rmax  topt ctmin ctmax    e   eh  q10 thermal_safety_margin thermal_tolerance breadth skewness
# 9.98 33.98  17.6 42.09 0.96 1.07 3.45                  8.11             24.49   10.23    -0.11

new_data_briere <- data.frame(temp = seq(min(d$temp), max(d$temp), 0.5))
preds_briere <- augment(fit_briere, newdata = new_data)

ggplot(d, aes(temp, rate)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_briere, col = 'pink', size = 2) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Metabolic rate',
       title = 'briere')
AIC(fit, fit_briere)

###SUMMARY WITHOUT 20 AND 30 DEG:
#fit of sharpeschoolhigh_1981: df = 5, AIC = 330.8485
#fit of thomas_2017: df = 6, AIC = 441.0477
#fit of lactin2_1995: df = 5, AIC =  349.7212
#fit of briere2_1999: df = 5, AIC = 349.5893

###SUMMARY WITH 30 DEG, WITHOUT 20 DEG:
#fit of sharpeschoolhigh_1981: df = 5, AIC = 364.2945
#fit of thomas_2017: df = 6, AIC = 391.8803
#fit of lactin2_1995: df = 5, AIC = 401.4693
#fit of briere2_1999: DF = 5, AIC = 378.3588
