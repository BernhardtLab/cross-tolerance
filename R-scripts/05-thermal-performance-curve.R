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

#READING IN TABLE
growth_rates_summary_wip <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/growth_rates_summary_wip.xlsx") %>% 
 view()

grsw_graph <- growth_rates_summary_wip %>% 
  ggplot(aes(x = `avg actual temp`, y = `avg growth rate`)) + geom_line()
grsw_graph
