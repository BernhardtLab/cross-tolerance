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

#code used to graph avg temp and all growth rate data points
 grsw_graph2 <- growth_rates_summary_wip %>% 
   ggplot(aes(x = `temp`, y = `growth rate`)) + geom_point()
 
 grsw_graph2
#multiple issues: 
#how to get rid of so many growth rates - change scales
#how to add line of curve
#how to separate temps by colour
#for 18 deg, 0.099 becomes 9.99 (r transforms into 9.9 e-2, then forgets about e-2 when graphing?)
 
grsw_graph3 <- growth_rates_summary_wip %>% 
   ggplot(aes(x = `temp`, y = `growth rate`)) + geom_point() +
   scale_y_discrete(breaks=seq(0, 10, 0.25))

#adapted from bp + coord_cartesian(ylim=c(5, 7.5)) + 
#scale_y_continuous(breaks=seq(0, 10, 0.25))  # Ticks from 0-10, every .25
#but just gets rid of any y values
 
grsw_graph3
##???
 
 
 
