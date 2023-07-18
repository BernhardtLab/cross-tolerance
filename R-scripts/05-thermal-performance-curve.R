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
growth_rates_summary_wip_1 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/growth_rates_summary_wip.xlsx") %>% 
  view()

#code used to graph avg temp and avg growth rate
grsw_graph <- growth_rates_summary_wip %>% 
  ggplot(aes(x = `avg actual temp`, y = `avg growth rate`)) + geom_line()
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
 #r thinks that y is discrete???
#how to add line of curve
#how to separate temps by colour
#for 18 deg, 0.099 becomes 9.99 (r transforms into 9.9 e-2, then forgets about e-2 when graphing?)
 
#fixing standard form and hopefully the discrete axis)
 growth_rates_summary_wip_csv <- read_csv("data-raw/growth_rates_summary_wip.csv") %>% 
   mutate(growth_rate = as.numeric(`growth rate`))
 view(growth_rates_summary_wip_csv)
 str(growth_rates_summary_wip_csv)
 grsw_graph4 <-  growth_rates_summary_wip_csv %>% 
   ggplot(aes(x = temp, y =growth_rate)) + geom_point() + geom_smooth()
 
 growth_rates_summary_wip_2 <- read_excel("data-raw/growth_rates_summary_wip.xlsx",sheet = 2, col_types = c("numeric", "numeric"))
View(growth_rates_summary_wip_2) 
  str(growth_rates_summary_wip_2)

 
 growth_rates_summary_wip <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/growth_rates_summary_wip.xlsx",sheet = 2, range = "a1:b73") %>% 
   format(scientific = FALSE) %>% 
   as.numeric() %>% 
   view()
 
format(growth_rates_summary_wip, scientific = FALSE) %>% View(growth_rates_summary_wip)
#??

#trying scipen 
install.packages("Tplyr")
library(Tplyr)
install.packages("settings")
library(settings)

require(settings)
10/1000000
#gives scientific notation
options(scipen = 100, digits = 6)
10/1000000
#gives decimal
settings::reset(options)
10/1000000
#gives decimal

growth_rates_summary_wip <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/growth_rates_summary_wip.xlsx", sheet = 2, range = "a1:b73") %>% 
  options(scipen = 100, digits = 6) #invalid argument
growth_rates_summary_wip <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/growth_rates_summary_wip.xlsx", sheet = 2, range = "a1:b73") %>% 
  options(scipen = 4) #invalid argument
View(growth_rates_summary_wip)

options(scipen = 6)
format(growth_rates_summary_wip)
View(growth_rates_summary_wip)

#fixing y axis:
grsw_graph3 <-  growth_rates_summary_wip_2 %>% 
   ggplot(aes(x = `temp`, y = `growth rate`)) + geom_point() + geom_smooth()
+
  scale_y_discrete(breaks = seq(0, 10, 1))
   scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9))

#adapted from bp + coord_cartesian(ylim=c(5, 7.5)) + 
#scale_y_continuous(breaks=seq(0, 10, 0.25))  # Ticks from 0-10, every .25
#but just gets rid of any y values
 
grsw_graph3
##???