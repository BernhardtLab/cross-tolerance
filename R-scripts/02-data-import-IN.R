## Ijeoma Nwafor
## July 3rd, 2023
# Growth Curves for Preliminary Data of June Temperature Reads

##Installing packages
install.packages("tidyverse")
install.packages("readxl")
install.packages("cowplot")
install.packages("ggplot2")
install.packages("usethis")
install.packages("dplyr")


library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot) ## this package is an add-on to ggplot which automatically gives a theme
theme_set(theme_cowplot())


## TESTER : JUNE 16, 42C
##read in plate
june16_42 <- read_excel("data-raw/June1623_42C.xlsx", range = "A40:CL137")

##filter table to time in seconds, temperature and OD value
june16_42 <- june16_42 %>% 
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))

## Assign treatments to wells
june16_42 <- june16_42 %>% 
  mutate(treatment = case_when(str_detect(well, "A") ~ "WT",
                               str_detect(well, "B") ~ "FLZ 1",
                               str_detect(well, "C") ~ "FLZ 2",
                               str_detect(well, "D") ~ "FLZ 3",
                               str_detect(well, "E") ~ "CASP 1",
                               str_detect(well, "F") ~ "CASP 2",
                               str_detect(well, "G") ~ "CASP 3",
                               str_detect(well, "H1|H2|H3") ~ "blank",
                               TRUE ~ "water"))

##plot of wells together in facets by treatment
june16_42 %>% 
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +  ## can remove the plus and next line to plot in one graph!
  facet_wrap(~treatment)

## CALCULATING GROWTH RATE 
## make a table with the growth rates per temperature
### estimate growth rates, this code gets us the growth rate for one well, now need to scale this across each well
library(devtools)
install_github("ctkremer/mleTools")
install_github("ctkremer/growthTools")
library(growthTools)

### filtered by one singular well
b1 <- june16_42 %>% 
  filter(well == "B2") %>% 
  mutate(log_od = log(OD600)) %>% 
  mutate(time_days = time / 86400) ### to get time in days

##get.growth.rate function not found
res <- get.growth.rate(b1$time_days,b1$log_od,plot.best.Q = TRUE,id = 'Population A')
res$best.model
res$best.slope

### Ije's way 
install.packages("growthrates")
##packages required for growthrates
install.packages("lattice")
install.packages("deSolve")

library(growthrates)

str(june16_42)

##"easy linear method"
splitted.data <- multisplit(june16_42, c("treatment", "OD600", "well"))
dat <- splitted.data[[1]]


### June 28th Growth Curves 
## 30 Degrees C
june28_30 <- read_excel("data-raw/June2823_30C.xlsx", range = "A40:CL137")

#filter
june28_30 <- june28_30 %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))

june28_30 <- june28_30 %>% 
  mutate(treatment = case_when(str_detect(well, "A") ~ "fRS585",
                               str_detect(well, "B") ~ "Blank",
                               TRUE ~ "water"))

june28_30 %>% 
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("June 28th, 30C")

#34 Degrees C
june28_34 <- read_excel("data-raw/June28_34C.xlsx", range = "A40:CL137")

june28_34 <- june28_34 %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))

june28_34 <- june28_34 %>% 
  mutate(treatment = case_when(str_detect(well, "A") ~ "fRS585",
                               str_detect(well, "B") ~ "Blank",
                               TRUE ~ "water"))

june28_34 %>% 
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("June 28th, 34C")
## nothing was contaminated - but weird bump in water

##June 29th 
#37 C
june29_37 <- read_excel("data-raw/June2923_37C.xlsx", range = "A40:CL137")

june29_37 <- june29_37 %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))

june29_37 <- june29_37 %>%
  mutate(treatment = case_when(str_detect(well, "A") ~ "fRS585",
                               str_detect(well, "B") ~ "Blank",
                               TRUE ~ "water"))

june29_37 %>%
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("June 29th, 37C")
## blanks contaminated


##JUNE 30TH
##41 C (labelled as the 29th but it is june 30th)
june29_41 <- read_excel("data-raw/June2923_41C.xlsx", range = "A40:CL137")

june29_41 <- june29_41 %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))

june29_41 <- june29_41 %>%
  mutate(treatment = case_when(str_detect(well, "A") ~ "fRS585",
                               str_detect(well, "B") ~ "Blank",
                               TRUE ~ "water"))

june29_41 %>%
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("June 30th, 41C")
## whats going on with the water on this one > 41C is where it did because it was mostly at 41.5C 
# water must be an error in the plate reader

#40C
june30_40C <- read_excel("data-raw/June3023_40C.xlsx", range = "A40:CL137")

june30_40C <- june30_40C %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))

june30_40C <- june30_40C %>%
  mutate(treatment = case_when(str_detect(well, "A") ~ "fRS585",
                               str_detect(well, "B") ~ "Blank",
                               TRUE ~ "water"))
june30_40C %>%
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("June 30th, 40C")

##JULY Growth Curves
##july 1st 25C
## how to plot when its randomized??
july01_25C <- read_excel("data-raw/July0123_25C.xlsx", sheet = "Sheet1", range = "A2:CL19")

july01_25C <- july01_25C %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))

july01_25C <- july01_25C %>%
  mutate(treatment = case_when(str_detect(well, "fRS585") ~ "fRS585",
                               str_detect(well, "Blank") ~ "Blank"))

july01_25C %>%
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("July 1st, 25C")

##july 5th 
## 30 C - ERROR MESSAGE IN EXCEL
july05_30C <- read_excel("data-raw/July5_30deg.xlsx", sheet = "Sheet3", range = "A2:CL19")

##40 C


