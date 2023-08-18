#SVETA

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot)

#GRAPHING OD READS FROM HYS_1 DAY 1
hys1_37 <- read_excel ("data-raw/HYS_1/HYS_1.xlsx", sheet = "37") %>% 
  mutate(temperature = 37)
view(hys1_37)

#hys1_37 %>% 
#  ggplot(aes(x = well, y = day1)) + geom_point()

hys1_42  <- read_excel ("data-raw/HYS_1/HYS_1.xlsx", sheet = "42") %>% 
  mutate(temperature = 42)
view(hys1_42)

#hys1_42 %>% 
#  ggplot(aes(x = well, y = day1)) + geom_point()

hys1_both <- bind_rows(hys1_42, hys1_37) %>% 
  mutate(unique_well = paste(well,temperature, sep = "_")) 

hys1_both %>% 
  ggplot (aes (x = well, y = day1, colour = factor(temperature), group = unique_well)) + 
  geom_point() +
  xlab('well') +
  ylab('OD600')