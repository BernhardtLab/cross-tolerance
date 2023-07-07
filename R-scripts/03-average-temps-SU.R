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
#answer: 40.04607
#checking in excel: 40.04606742

###June 30, 41 deg
june30_41 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/June2923_41C.xlsx", range = "B40:CL137") %>% 
  clean_names() %>% 
  view()

june30_41 %>% 
  top_n(n = 1) %>% 
  t() %>% 
  mean()
#answer: 41.1809

###June 29, 37 deg
june29_37 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/June2923_37C.xlsx", range = "B40:CL137") %>%
  clean_names() %>% 
  view()

june29_37 %>% 
  top_n(n = 1) %>% 
  t() %>% 
  mean()
#answer: 37.1191

###June 28, 30 deg
june28_30 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/June2823_30C.xlsx", range = "B40:CL137") %>% 
  clean_names() %>% 
  view()

june28_30 %>% 
  top_n(n = 1) %>% 
  t() %>% 
  mean()
#answer: 30.44719