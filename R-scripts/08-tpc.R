


#### Data import process for OD data from thermal performance experiments with N. glabrata

# Author: Joey Bernhardt
# Input: Excel spreadsheets that are outputs from the Tecan plate reader
# Output: Merged OD data across all temperatures
# Written for R version 4.2.3
# Last updated: July 10 2025


# load packages -----------------------------------------------------------


library(tidyverse)
library(readxl)
library(cowplot)
theme_set(theme_cowplot())


# read in data ------------------------------------------------------------

### 18C
temp18C <- read_excel("data-raw/old-unused/Growth curves/July0423_18C.xlsx", sheet = "Sheet1", range = "A2:I9", col_names = c("time", "rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8")) %>% 
  gather(2:ncol(.), key = "well", value = "OD") %>% 
  mutate(treatment = "fRS585") %>% 
  mutate(test_temperature = 18) %>% 
  mutate(time = time*3600) ## convert hours to seconds


temp18C %>% 
  ggplot(aes(x = time, y = OD)) + geom_point()

temp20C <- read_excel("data-raw/old-unused/Growth curves/July25_20C_rep2_combined_results.xlsx", sheet = "Sheet1") %>% 
  gather(2:ncol(.), key = "well", value = "OD") %>% 
  mutate(treatment = "fRS585") %>% 
  mutate(test_temperature = 20) %>% 
  rename(time = Time) %>% 
  mutate(time = time*3600) ## convert hours to seconds




temp30C <- read_excel(
  "data-raw/old-unused/Growth curves/July2523_30C.xlsx",
  sheet = "Sheet2",
  range = "A34:CT132",
  col_names = c("cycle", 1:97)) %>% 
  filter(cycle != "Temp. [°C]") %>%
  filter(cycle != "Cycle Nr.") %>% 
  row_to_names(row_number = 1, remove_row = TRUE) %>% 
  gather(2:ncol(.), key = "time", value = "OD") %>% 
  rename(well = "Time [s]") %>% 
  # mutate(treatment = "fRS585") %>% 
  mutate(test_temperature = 30)

temp30C_key <- read_excel("data-raw/old-unused/Well label keys/25.07, 30 deg.xlsx")

df1 <- data.frame(A1 = "A1", water = "water")

temp30C_key2 <- bind_rows(temp30C_key, df1) %>% 
  rename(well = A1, treatment = water)

temp30C2 <- temp30C %>% 
  left_join(temp30C_key2, by = "well") %>% 
  mutate(time = as.numeric(time))



temp40.5 <- read_excel("data-raw/old-unused/Growth curves/July0623_40.5C.xlsx", range = "A34:CS132",
                       col_names = c("cycle", 1:96)) %>% 
  filter(cycle != "Temp. [°C]") %>% 
  filter(cycle != "Cycle Nr.") %>% 
  row_to_names(row_number = 1, remove_row = TRUE) %>% 
  gather(2:ncol(.), key = "time", value = "OD") %>% 
  rename(well = "Time [s]") %>% 
  # mutate(treatment = "fRS585") %>% 
  mutate(test_temperature = 40.5)

temp40.5C_key <- read_excel("data-raw/old-unused/Well label keys/06.07, 40.5 deg.xlsx") %>% 
  mutate(remove = ifelse(well == "F10" & treatment == "water", "remove", "keep")) %>% 
 filter(remove == "keep") %>% ### this looks like it has two entries for well F10 -- this needs to be fixed, based on labnotebook notes from Sveta "In the sheet, there is a comment with a copy of the note from the paper lab book:
#   blank (blue) - F2, D4, F6, B8, C9, D10, G11, G8
# culture (purple) - D2, G4, C5, E5, E8, F10, B10, D11
# Based on this, I think well F10 is culture (fRS585) and B5 is blank/water and can be excluded from analysis"
  select(-remove)

temp40.5C2 <- temp40.5 %>% 
  left_join(temp40.5C_key, by = "well") %>% 
  mutate(time = as.numeric(time))

### I'm seeing there are a few wells at 40.5C here that have much lower carrying capacity -- it's weird and I'm not sure why. Come back to this.







all_temps <- bind_rows(temp18C, temp20C, temp30C2, temp40.5C2)


all_temps %>% 
  filter(treatment == "fRS585") %>%
  filter(test_temperature == 40.5) %>% 
  ggplot(aes(x = time, y = OD, group = well, color = factor(test_temperature))) + geom_line() +
  scale_color_viridis_d()
