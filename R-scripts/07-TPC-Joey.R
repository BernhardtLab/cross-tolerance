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
#[1] "beta_2012"             "boatman_2017"          "briere2_1999"         
#[4] "delong_2017"           "flinn_1991"            "gaussian_1987"        
#[7] "hinshelwood_1947"      "joehnk_2008"           "johnsonlewin_1946"    
#[10] "kamykowski_1985"       "lactin2_1995"          "lrf_1991"             
#[13] "modifiedgaussian_2006" "oneill_1972"           "pawar_2018"    

#[16] "quadratic_2008"        "ratkowsky_1983"        "rezende_2019"         
#[19] "sharpeschoolfull_1981" "sharpeschoolhigh_1981" "sharpeschoollow_1981" 
#[22] "spain_1982"            "thomas_2012"           "thomas_2017"          
#[25] "weibull_1995"         

#copied briere from personal lab meetings r script
d <- read_excel("data-raw/growth_rates_summary_wip.xlsx", sheet = 2) %>% 
  rename(rate = `growth rate`)
view(d)


mod = 'sharpeschoolhigh_1981'

start_vals <- get_start_vals(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')
start_vals
low_lims <- get_lower_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')

fit_mod <- nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref, e, eh, th, tref = 15), 
                         data = d,
                         iter = 500,
                         start_lower = get_start_vals(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981') - 10,
                         start_upper = get_start_vals(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981') + 10,
                         lower = get_lower_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981'),
                         upper = get_upper_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981'),
                         supp_errors = 'Y')
fit_mod



# briere ------------------------------------------------------------------

start_vals <- get_start_vals(d$temp, d$rate, model_name = 'briere2_1999')
start_vals
low_lims <- get_lower_lims(d$temp, d$rate, model_name = 'briere2_1999')
upper_lims <- get_upper_lims(d$temp, d$rate, model_name = 'briere2_1999')

fit_mod <- nls_multstart(rate~briere2_1999(temp = temp), 
                         data = d,
                         iter = 500,
                         start_lower = get_start_vals(d$temp, d$rate, model_name = 'briere2_1999') - 10,
                         start_upper = get_start_vals(d$temp, d$rate, model_name = 'briere2_1999') + 10,
                         lower = get_lower_lims(d$temp, d$rate, model_name = 'briere2_1999'),
                         upper = get_upper_lims(d$temp, d$rate, model_name = 'briere2_1999'),
                         supp_errors = 'Y')
fit_mod

########

calc_params(fit_mod) %>%
  mutate_all(round, 2)

new_data <- data.frame(temp = seq(min(d$temp), max(d$temp), 0.0001)) #originally 0.5
preds <- augment(fit_mod, newdata = new_data)

#getting the superscript
poster_graph <- ggplot(d, aes(temp, rate)) +
  geom_line(aes(temp, .fitted), preds, col = 'darkorchid4', size = 2) +
  geom_point(size = 3.5, alpha = 0.4) +
  # theme_bw(base_size = 20) +
  xlab('Temperature (ºC)')+
  ylab(bquote('Population growth rate  '(day^-1))) +
  theme(text=element_text(size=20, family="sans")) +
  theme(axis.text = element_text(size = 20)) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )
poster_graph                         

ggsave(plot = poster_graph, filename = "figures/poster_graph.pdf", width = 7, height = 5)
#need to change size now
AIC(fit, fit_mod)

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
