## FILE PATHS WERE UPDATED ON MARCH 3RD - DATA IMPORTS WONT FUNCTION

#SVETA
#GRAPHING HYS4

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot)

#ctrl
hys4_ctrl <- read_excel ("data-raw/HYS4.xlsx", sheet = "ctrl") %>% 
  mutate (temperature = "35") #%>% 
  View()

#ctrl facet wrap graph
hys4_ctrl %>% 
  ggplot(aes( x = hours, y = OD, colour = treatment)) +
  geom_point(aes(shape = treatment), size = 2) +
  theme_minimal() +
  facet_wrap(~well, scales = "free_x") +
  geom_line(aes(x = hours, y = OD, group = well)) +
  ggtitle("ctrl")
  
#high_24
hys4_high_24 <- read_excel ("data-raw/HYS4.xlsx", sheet = "high_24hr") %>% 
  mutate (temperature = "40") #%>% 
  View()

#graph high 24 facet wrap
hys4_high_24%>% 
    ggplot(aes( x = hours, y = OD, colour = treatment)) +
    geom_point(aes(shape = treatment), size = 2) +
    theme_minimal() +
    facet_wrap(~well, scales = "free_x") +
    geom_line(aes(x = hours, y = OD, group = well)) +
    ggtitle("high 24")
  
#high_48
hys4_high_48 <- read_excel ("data-raw/HYS4.xlsx", sheet = "high_48hr") %>% 
  mutate (temperature = "40") #%>% 
  View()

#graph high 48 facet wrap
  hys4_high_48 %>% 
    ggplot(aes( x = hours, y = OD, colour = treatment)) +
    geom_point(aes(shape = treatment), size = 2) +
    theme_minimal() +
    facet_wrap(~well, scales = "free_x") +
    geom_line(aes(x = hours, y = OD, group = well)) +
    ggtitle("high 48")
  
  
#together
hys4_combined <- bind_rows(hys4_ctrl, hys4_high_24, hys4_high_48) %>% 
  mutate(unique_well = paste(well, treatment, temperature, sep = "_"))
View(hys4_combined)

#grpahing together - wip
hys4_combined %>% 
  ggplot (aes (x = well, y = OD, colour = factor(temperature), group = unique_well)) +
  geom_point() +
  xlab('well')+
  ylab('OD600')

hys4_combined %>% 
  ggplot(aes( x = hours, y = OD, colour = temperature, group = unique_well)) +
  geom_point(aes(shape = treatment), size = 2) +
  theme_minimal() +
  facet_wrap(~well, scales = "free_x") +
  geom_line(aes(x = hours, y = OD, group = well))

hys4_combined %>% 
  ggplot(aes(x = well, y = OD, colour = hours, group = unique_well)) +
  scale_shape_manual(values = c(21, 23)) +
  geom_point (aes(shape = treatment, size = 4, fill = factor(temperature),  stroke = 2)) +
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest2")) +
  scale_colour_manual(values=wes_palette(n=2, name="Cavalcanti1")) +
  theme_minimal()

library(wesanderson)
hys2_bothalt %>% 
  ggplot(aes(x = well, y = OD, colour = day, group = unique_well)) +
  scale_shape_manual(values = c(21, 23)) +
  geom_point (aes(shape = treatment, size = 4, fill = factor(temperature),  stroke = 2))+
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest2")) +
  scale_colour_manual(values=wes_palette(n=2, name="Cavalcanti1")) +
  theme_minimal() +
  ggtitle("HYS2 days 1 and 2, 37 and 42 deg")
#where is the time??