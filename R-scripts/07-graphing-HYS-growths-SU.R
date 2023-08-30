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
#on its own
hys1_37 %>% 
 ggplot(aes(x = well, y = day1)) + geom_point()

hys1_42  <- read_excel ("data-raw/HYS_1/HYS_1.xlsx", sheet = "42") %>% 
  mutate(temperature = 42)
view(hys1_42)
#on its own
hys1_42 %>% 
  ggplot(aes(x = well, y = day1)) + geom_point()

#together
hys1_both <- bind_rows(hys1_42, hys1_37) %>% 
  mutate(unique_well = paste(well,temperature, sep = "_")) %>% view()

hys1_both %>% 
  ggplot (aes (x = well, y = day1, colour = factor(temperature), group = unique_well)) + 
  geom_point() +
  xlab('well') +
  ylab('OD600')
#blank = well h12
#blank values close together --> overlap --> looks as if only one dot, but with zoom in it's two
#blank will not be h12 from now on (prob should change h12 --> blank in future?)


#GRAPHING OD READS FROM HYS_2 DAY 1
hys2_37 <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "37") %>% 
  mutate(temperature = 37)
View(hys2_37)

hys2_37 %>% 
  ggplot(aes(x = well, y = day1, colour = factor(treatment))) + 
  geom_point() +
  ggtitle('37C')
  
hys2_42 <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "42") %>% 
  mutate(temperature = 42)
View(hys2_42)

hys2_42 %>% 
  ggplot(aes(x = well, y = day1, colour = factor(treatment))) + 
  geom_point() +
  ggtitle('42C')

#both together
hys2_both <- bind_rows(hys2_42, hys2_37) %>% 
  mutate(unique_well = paste(well,temperature, sep = "_"))
View(hys2_both)

hys2_both %>% 
  ggplot (aes (x = well, y = day1, colour = factor(temperature), group = unique_well)) + 
  geom_point() +
  ggtitle('both')
#didn't colour code blanks vs culture, but evident that lowest od simlar in both temps is blanks

#GRAPHING OD READS FROM HYS_2 DAY 2
#graphing 42 - looking at difference between day 1 and day 2
hys2_42alt <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "42alt") %>% 
  mutate(temperature = 42) %>% 
  mutate(unique_well = paste(well,day, sep = "_"))
View(hys2_42alt)

hys2_42alt %>% 
  ggplot(aes( x = well, y = OD, colour = factor(day), group = unique_well))+
  geom_point() +
  ggtitle('42 days 1 and 2')
