#SVETA
#PLOTTING THERMAL PERFORMANCE CURVE

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot)
theme_set(theme_cowplot())

#uploaded the done avg actual temps and calculated growth rates to git in raw data
#graphing the temps on x, growth rates on y

###GRAPHING AVG GROWTH RATES
#reading in table
growth_rates_summary_wip <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/growth_rates_summary_wip.xlsx") %>% 
  view()

#code used to graph avg temp and avg growth rate
grsw_graph <- growth_rates_summary_wip %>% 
  ggplot(aes(x = `avg temp`, y = `avg growth rate`)) + geom_line()
grsw_graph

###GRAPHING WITH VARIATION
#reading in table
growth_rates_summary_wip <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/growth_rates_summary_wip.xlsx", sheet = 2, range = "a1:b73") 
 view(growth_rates_summary_wip)

 grsw_graph2 <- growth_rates_summary_wip %>% 
   ggplot(aes(x = `temp`, y = `growth rate`)) + geom_point()
 grsw_graph2
#multiple issues 
#how to separate temps by colour
#how to get rid of so many growth rates - change scales
#how to add line of curve
 
 growth_rates_summary_wip <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/growth_rates_summary_wip.xlsx", sheet = 3) 
 view(growth_rates_summary_wip)
 
#code used to graph avg temp and all growth rate data points
grsw_graph2 <- growth_rates_summary_wip %>% 
  ggplot(aes(x = "avg temp", y = "growth rate 1")) + geom_point()
() + geom_errorbar()
grsw_graph2
