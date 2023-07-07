#SVETA
#GROWTH CURVE AVG ACTUAL TEMPS
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(janitor)
install.packages("cowplot")
library(cowplot) #this package is an add-on to ggplot which automatically gives a theme

###reading in June 30, 40 deg
#couldn't find a way to exclude the temp row when doing mean
#so ended up just selecting from B onwards when pulling in data
june30_40 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/June3023_40C.xlsx", range = "B40:CL137") %>%
  clean_names() %>% 
  view()

###avg temp
june30_40 %>% 
  top_n(n = 1) %>% 
  t() %>% 
  mean()

