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


june16_42_a <- june16_42 %>% 
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))
View(june16_42_a)

#copying old code
copy_of23 <- read_excel ("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/Growth curves/summer2024/June11.2024.42C.Shaking.xlsx", sheet = "transposed") %>% 
  clean_names()
View(copy_of23)

str(copy_of23)

copy_of23_a <- copy_of23 %>% 
  filter(`time` != "T° 600") %>% 
  mutate(time = as.numeric(time)) #%>% 
  gather(2:98, key = time, value = OD600) %>% 
  rename(well = `time`) 
View(copy_of23_a)

copy_of23_b <- copy_of23 %>% 
  filter(`time` != "T° 600") %>% 
  gather(key = time, value = OD600, 2:98)
View(copy_of23_b)

#copy_of23_b <- copy_of23_a %>% 
  #mutate(treatment = case_when(str_detect(well, "A1") ~ "YPD",
                               str_detect(well, "B") ~ "FRS152",
                               str_detect(well, "C") ~ "FRS1",
                               str_detect(well, "D") ~ "FRS585_30",
                               str_detect(well, "E") ~ "FRS585_37",
                               str_detect(well, "F") ~ "YPD",
                               str_detect(well, "G") ~ "YPD",
                               str_detect(well, "h") ~ "YPD",))
#View(copy_of23_b)
####
with_number <- read_excel ("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/Growth curves/summer2024/June11.2024.42C.Shaking.xlsx", sheet = "transposed_withtoprow") %>% 
  clean_names()
View(with_number)

with_number_a <- with_number %>% 
  filter(`time` != "T° 600") %>% 
  filter(`time` != "Time_old") %>% 
  gather(key = time, value = OD600, 2:98) %>% 
  rename(well = `time`) #%>% 
  rename(time_sec = `time`) #%>% 
  mutate(time_sec = as.numeric(time_sec))

View(with_number_a)

with_number_a <- with_number %>% 
  filter(`number` != "T° 600") %>% 
  filter(`number` != "Time") %>% 
  gather(key = number, value = OD600, 2:98) 
#%>% 
  rename(well = `number`) %>% 
  rename(time_sec = `time`) %>% 
  mutate(time_sec = as.numeric(time_sec))

View(with_number_a)
  
with_number_b <- with_number_a %>% 
  mutate(treatment = case_when(str_detect(well, "A") ~ "YPD",
                               str_detect(well, "B") ~ "FRS152",
                               str_detect(well, "C") ~ "FRS1",
                               str_detect(well, "D") ~ "FRS585_30",
                               str_detect(well, "E") ~ "FRS585_37",
                               str_detect(well, "F") ~ "YPD",
                               str_detect(well, "G") ~ "YPD",
                               str_detect(well, "H") ~ "YPD",
                               ))
View(with_number_b)
#did not do all 1 and 12 are blanks!

graph <- with_number_b %>% 
  ggplot(aes(x = time_sec, y = OD600, group = well, color = treatment)) + 
  geom_line() +
  facet_wrap(~well)
graph

#scraps
 # gather(2:98, key = time, value = OD600) #%>%  
#  rename(well = `time`)

#filter(`number` != "Time") %>%
#gather(3:98, key = time, value = OD600) %>% 


###
#trying raw
trials42 <- read_excel ("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/Growth curves/summer2024/June11.2024.42C.Shaking.xlsx", sheet = "raw") %>% 
  clean_names()
View(trials42)

str(trials42)
#time in POSIXCT format

trials42_a <- trials42 %>% 
  select(-t_600) 
View(trials42_a)
trials42_a %>% ggplot(aes(time, a1)) + geom_line() #just one well no treatment

trials42_b <- trials42 %>% 
  select(-t_600) %>% 
  gather(key = time, value = OD600, 2:97)

View(trials42_b)

  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))


  ###
#running old code
june16_42 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/Growth curves/June1623_42C.xlsx", range = "A40:CL137")
View(june16_42)

june16_42_a <- june16_42 %>% 
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))
View(june16_42_a)

#trying pivot longer
#june16_42_p <- june16_42 %>% 
  filter(`Time [s]` != "Temp. [°C]") %>%
  pivot_longer(c(`Time [s]`, A1))
#View(june16_42_p)

  
  
###---------------------------------------------------------------------
#let's do this again, now using the algae script (RFU import Joey) 
june16_42_1 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/Growth curves/summer2024/June11.2024.42C.Shaking.xlsx", sheet = "raw") %>% 
    clean_names()
View(june16_42_1)

#fixing time, sneaky way
june16_42_1$Time %>% 
  mutate(time = hms::as_hms(Time)) #nope

#gather, take bazillion
str(june16_42_1)

meh <- as.numeric(june16_42_1$time)
str(meh)

june16_42_2 <- june16_42_1 %>% 
  select(-t_600) %>% 
  as.numeric(june16_42_1$time) %>% 
  gather(key = time, value = od600, 2:97)
#list' object cannot be coerced to type 'double'

june16_42_2 <- june16_42_1 %>% 
  select(-t_600) %>% 
  unlist(june16_42_1$time) %>% 
  as.numeric(june16_42_1$time) %>% 
  gather(key = time, value = od600, 1:97)
#no applicable method for 'gather' applied to an object of class "c('double', 'numeric')"
#View(june16_42_2)  


june16_42_2 <- june16_42_1 %>% 
  select(-t_600) %>% 
  gather(key = time, value = od600, 2:97)
View(june16_42_2) #doens't have time column

#making time column by repeating time from the sheet
hehe <- data.frame(june16_42_1$time)
View(hehe) #97 observations of 1 variable

hehe2 <- rep(hehe, times = 96)
View(hehe2) #list of length 96

june16_42_3 <- june16_42_2 %>% 
  mutate(time = hehe2)

