


#### Fitting a thermal performance curve to yeast growth rates

library(tidyverse)
library(readxl)


# read in data ------------------------------------------------------------

### 18C
temp18C <- read_excel("data-raw/old-unused/Growth curves/July0423_18C.xlsx", sheet = "Sheet1", range = "A2:I9", col_names = c("time", "rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8")) %>% 
  gather(2:ncol(.), key = "replicate", value = "OD") %>% 
  mutate(treatment = "fRS585") %>% 
  mutate(test_temperature = 18)


temp18C %>% 
  ggplot(aes(x = time, y = OD)) + geom_point()

temp20C <- read_excel("data-raw/old-unused/Growth curves/July25_20C_rep2_combined_results.xlsx", sheet = "Sheet1") %>% 
  gather(2:ncol(.), key = "replicate", value = "OD") %>% 
  mutate(treatment = "fRS585") %>% 
  mutate(test_temperature = 20) %>% 
  rename(time = Time)


temp30C <- read_excel("data-raw/old-unused/Growth curves/July2523_30C.xlsx", sheet = "Sheet2", range = "A40:CL138")




all_temps <- bind_rows(temp18C, temp20C)


all_temps %>% 
  ggplot(aes(x = time, y = OD, color = test_temperature)) + geom_point() +
  scale_color_viridis_c()
