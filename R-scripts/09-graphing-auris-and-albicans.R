#SVETA
#graphing auris and albicans growths using HYS4 code
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot)

#HYS7
#frs1 ctrl
hys7_frs1_ctrl <- read_excel ("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/auris_and_albicans/HYS7.xlsx", sheet = "frs1_ctrl") %>% 
  mutate (temperature = "ctrl") %>%
  mutate (strain = "frs1")
View(hys7_frs1_ctrl)
hys7_frs1_ctrl %>% 
  ggplot(aes( x = day, y = OD, colour = treatment)) +
  geom_point(aes(shape = treatment), size = 2) +
  theme_minimal() +
  facet_wrap(~well, scales = "free_x") +
  geom_line(aes(x = day, y = OD, group = well)) +
  ggtitle("frs 1 ctrl")

#frs1trt
hys7_frs1_trt <- read_excel ("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/auris_and_albicans/HYS7.xlsx", sheet = "frs1_trt") %>% 
  mutate (temperature = "trt") %>%
  mutate (strain = "frs1")
View(hys7_frs1_trt)
hys7_frs1_trt %>% 
  ggplot(aes( x = day, y = OD, colour = treatment)) +
  geom_point(aes(shape = treatment), size = 2) +
  theme_minimal() +
  facet_wrap(~well, scales = "free_x") +
  geom_line(aes(x = day, y = OD, group = well)) +
  ggtitle("frs 1 trt")
#frs152 ctrl
hys7_frs152_ctrl <- read_excel ("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/auris_and_albicans/HYS7.xlsx", sheet = "frs152_ctrl") %>% 
  mutate (temperature = "ctrl") %>%
  mutate (strain = "frs152")
View(hys7_frs152_ctrl)
hys7_frs152_ctrl %>% 
  ggplot(aes( x = day, y = OD, colour = treatment)) +
  geom_point(aes(shape = treatment), size = 2) +
  theme_minimal() +
  facet_wrap(~well, scales = "free_x") +
  geom_line(aes(x = day, y = OD, group = well)) +
  ggtitle("frs 152 ctrl")

#frs152 trt
hys7_frs152_trt <- read_excel ("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/auris_and_albicans/HYS7.xlsx", sheet = "frs152_trt") %>% 
  mutate (temperature = "trt") %>%
  mutate (strain = "frs152")
View(hys7_frs152_trt)
hys7_frs152_trt %>% 
  ggplot(aes( x = day, y = OD, colour = treatment)) +
  geom_point(aes(shape = treatment), size = 2) +
  theme_minimal() +
  facet_wrap(~well, scales = "free_x") +
  geom_line(aes(x = day, y = OD, group = well)) +
  ggtitle("frs 152 trt")

#HYS5
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
