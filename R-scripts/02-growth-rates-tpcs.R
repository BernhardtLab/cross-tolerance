

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



# load data ---------------------------------------------------------------

ods <- read_csv("data-processed/all-temps-od.csv")


ods %>% 
  ggplot(aes(x = time, y = OD, color = factor(test_temperature))) + geom_point()

od2 <- ods %>% 
  group_by(well, test_temperature) %>% ## it looks like this grouping will work because we don't have temperatures tested on multiple days, so this is enough to uniquely ID the wells
  mutate(days = as.duration(time - min(time)) / ddays(1))


od2 %>% 
  ggplot(aes(x = days, y = OD, color = factor(test_temperature))) + geom_point()

