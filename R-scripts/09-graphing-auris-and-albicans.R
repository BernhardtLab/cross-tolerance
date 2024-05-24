#SVETA
#graphing HYS5 growths using HYS4 code
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot)

#frs 1 ctrl
hys5_frs1_ctrl <- read_excel ("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/auris_and_albicans/HYS5.xlsx", sheet = "frs1_ctrl") %>% 
  mutate (temperature = "ctrl") %>%
  mutate (strain = "frs1")
View(hys5_frs1_ctrl)

hys5_frs1_ctrl %>% 
  ggplot(aes( x = day, y = OD, colour = treatment)) +
  geom_point(aes(shape = treatment), size = 2) +
  theme_minimal() +
  facet_wrap(~well, scales = "free_x") +
  geom_line(aes(x = day, y = OD, group = well)) +
  ggtitle("frs 1 ctrl")

#frs 1 trt
hys5_frs1_trt <- read_excel ("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/auris_and_albicans/HYS5.xlsx", sheet = "frs1_trt") %>% 
  mutate (temperature = "trt") %>%
  mutate (strain = "frs1")
View(hys5_frs1_trt)

hys5_frs1_trt %>% 
  ggplot(aes( x = day, y = OD, colour = treatment)) +
  geom_point(aes(shape = treatment), size = 2) +
  theme_minimal() +
  facet_wrap(~well, scales = "free_x") +
  geom_line(aes(x = day, y = OD, group = well)) +
  ggtitle("frs 1 trt")
