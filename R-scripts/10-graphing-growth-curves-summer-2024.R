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
  gather(??, key = time, value = OD600) #%>%  
  rename(well = `time`)

#filter(`number` != "Time") %>%
#gather(3:98, key = time, value = OD600) %>% 
View(copy_of23_a)

copy_of23_b <- copy_of23_a %>% 
  mutate(treatment = case_when(str_detect(well, "A1") ~ "YPD",
                               str_detect(well, "B") ~ "FRS152",
                               str_detect(well, "C") ~ "FRS1",
                               str_detect(well, "D") ~ "FRS585_30",
                               str_detect(well, "E") ~ "FRS585_37",
                               str_detect(well, "F") ~ "YPD",
                               str_detect(well, "G") ~ "YPD",
                               str_detect(well, "h") ~ "YPD",))
View(copy_of23_b)

#running old code
june16_42 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/Growth curves/June1623_42C.xlsx", range = "A40:CL137")
View(june16_42)

june16_42_a <- june16_42 %>% 
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) #%>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))
View(june16_42_a)

#trying pivot longer
#june16_42_p <- june16_42 %>% 
  filter(`Time [s]` != "Temp. [°C]") %>%
  pivot_longer(c(`Time [s]`, A1))
#View(june16_42_p)
