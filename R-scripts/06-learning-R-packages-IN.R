# working with thermal performance curves
#ijeoma nwafor
#july 19th, 2023

#installing packages needed to load in thermal performance packages
install.packages("usethis") #needed for devtools
install.packages("devtools") # needed for install_github/version
install.packages("nls.multstart") # for nonlinear squares regression
# takes a bunch of different starting values to fit a non linear regression model
install.packages("broom") #cleans up ^ and turns into tidy tibbles
install.packages("tidyverse")
install.packages("remotes")
install.packages("readxl")

library(usethis)
library(devtools)
library(nls.multstart)
library(broom)
library(tidyverse)
library(remotes)
library(readxl)

##installing rTPC packages straight from github 
remotes::install_github("padpadpadpad/rTPC", build_vignettes = TRUE)
library(rTPC)

#VIGNETTE TESTING DATA
{
  get_model_names()
# load in data
data("chlorella_tpc")

# keep just a single curve
d <- filter(chlorella_tpc, curve_id == 1)

# show the data
ggplot(d, aes(temp, rate)) +
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Metabolic rate',
       title = 'Respiration across temperatures')

# choose model
mod = 'sharpschoolhigh_1981'

# get start vals
start_vals <- get_start_vals(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')

# get limits
low_lims <- get_lower_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')

start_vals
low_lims
upper_lims


fit <- nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                     data = d,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')
fit

# calculate additional traits
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
       y = 'Metabolic rate',
       title = 'Respiration across temperatures')
}

#Using our data 
data_gr <- read_excel("data-raw/growth_rates_summary_wip.xlsx", sheet = 2) %>%
  rename(rate = `growth rate`)
view(data_gr)

data_gr %>% ggplot(aes(x = `temp`, y = `rate`)) +
  geom_point() + geom_smooth() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'growth rate',
       title = 'base TPC')

mod = 'sharpschoolhigh_1981'

start_vals_gr <- get_start_vals(data_gr$temp, data_gr$rate, model_name = 'sharpeschoolhigh_1981')

# get limits
low_lims_gr <- get_lower_lims(data_gr$temp, data_gr$rate, model_name = 'sharpeschoolhigh_1981')
upper_lims_gr <- get_upper_lims(data_gr$temp, data_gr$rate, model_name = 'sharpeschoolhigh_1981')

start_vals_gr
low_lims_gr
upper_lims_gr

fit <- nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                     data = data_gr,
                     iter = 500,
                     start_lower = start_vals_gr - 10,
                     start_upper = start_vals_gr + 10,
                     lower = low_lims_gr,
                     upper = upper_lims_gr,
                     supp_errors = 'Y')
fit

# calculate additional traits
calc_params(fit) %>%
  # round for easy viewing
  mutate_all(round, 2)

##estimates a ctmax of 41.75 and a thermal tolerance of 42.11
#can fix with different functional form and may have a different ctmax/min
#compare fits to different models (AIC - fit 1 and fit 2 to compare or visually with facet_wrap)

# predict new data
new_data <- data.frame(temp = seq(min(data_gr$temp), max(data_gr$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit - line of best fit 
ggplot(d, aes(temp, rate)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'pink') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'growth rate',
       title = 'TPC')

##can compare to a different model type with other functional curve; missing lower temperature data points 

#Trying other models 
mod = 'briere2_1999'

start_vals_gr2 <- get_start_vals(data_gr$temp, data_gr$rate, model_name = 'briere2_1999')

# get limits
low_lims_gr2 <- get_lower_lims(data_gr$temp, data_gr$rate, model_name = 'briere2_1999')
upper_lims_gr2 <- get_upper_lims(data_gr$temp, data_gr$rate, model_name = 'briere2_1999')

start_vals_gr2
low_lims_gr2
upper_lims_gr2

fit_2 <- nls_multstart(rate~thomas_2017(temp = temp, r_tref,e,eh,th, tref = 15),
                     data = data_gr,
                     iter = 500,
                     start_lower = start_vals_gr2 - 10,
                     start_upper = start_vals_gr2 + 10,
                     lower = low_lims_gr2,
                     upper = upper_lims_gr2,
                     supp_errors = 'Y')
fit_2

# calculate additional traits
calc_params(fit) %>%
  # round for easy viewing
  mutate_all(round, 2)

##estimates a ctmax of 41.75 and a thermal tolerance of 42.11
#can fix with different functional form and may have a different ctmax/min
#compare fits to different models (AIC - fit 1 and fit 2 to compare or visually with facet_wrap)

# predict new data
new_data <- data.frame(temp = seq(min(data_gr$temp), max(data_gr$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit - line of best fit 
ggplot(d, aes(temp, rate)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'pink') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'growth rate',
       title = 'TPC')


##Now lets load in ThermPerf
remotes::install_github("mdjbru-R-packages/thermPerf")
library(thermPerf)


##Playing with SPOCK Package 
install.packages("remotes")
remotes::install_github("labmccormick/SPOCK")

install.packages("Sleuth3")
install.packages("ggplot2")

##following along with the pdf by David Gerard
qplot(temp, rate, data = data_gr)

class(data_gr$temp)
data_gr$temp <- as.numeric(data_gr$temp)

data_gr$rate <- as.numeric(data$`growth rate`)

aout_alldiff <- aov(rate ~ temp, data = data_gr)
aout_alldiff

summary(aout_alldiff)

ptout <- pairwise.t.test(x = data_gr$rate,
                         g = data_gr$temp,
                         p.adjust.method = "none")
ptout

data_gr$isSpock <- data_gr$temp == "Spock's"
data_gr$isSpock
## all false

aout_otherssame <- aov(rate ~ isSpock, data = data_gr)

anova(aout_otherssame, aout_alldiff)

aout_allequal <- aov(rate ~ 1, data = data_gr)
anova(aout_allequal, aout_otherssame, aout_alldiff)
