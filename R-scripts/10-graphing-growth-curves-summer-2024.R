#growth curves summer 2024
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot)
library(janitor)
library(tidyr)

#June11.2024.42C.Shaking
trials42 <- read_excel ("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/Growth curves/summer2024/June11.2024.42C.Shaking.xlsx", sheet = "raw") %>% 
  clean_names()
View(trials42)

trials42_frs152 <- trials42 %>% 
  select(c(time, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11))
View(trials42_frs152)

trials42_frs152 %>% 
  ggplot(aes(time, b1))+
  geom_point()

#copying old code
copy_of23 <- read_excel ("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/Growth curves/summer2024/June11.2024.42C.Shaking.xlsx", sheet = "transposed") %>% 
  clean_names()
View(copy_of23)

copy_of23_a <- copy_of23 %>% 
  filter(`time` != "T° 600") %>% 
  gather(3:98, key = time, value = OD600) %>% 
  rename(well = `time`) %>% 
  mutate(time = as.numeric(time))

#c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11