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

##34C from june 28, july 26 30C, 37C from June 29, 40C from July 5


library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot) ## this package is an add-on to ggplot which automatically gives a theme
theme_set(theme_cowplot())




#34 Degrees C THIS ONE
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

#40C this one as well
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



##40 C
july05_40C_raw <- read_excel("data-raw/July0523_40C.xlsx", sheet = "Sheet3", range = "A2:CL19")

july05_40C <- july05_40C_raw %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))

july05_40C <- july05_40C %>%
  mutate(treatment = case_when(str_detect(well, "fRS585") ~ "fRS585",
                               str_detect(well, "blank") ~ "Blank"))

july05_40C %>%
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("July 5th, 40C - REDO")


#### combined graph
june30_40C_temp <- june30_40C %>% 
  mutate(temperature = 40)


june28_34_temp <- june28_34 %>% 
  mutate(temperature = 34)

june29_41_temp <- june29_41 %>%
  mutate(temperature = 41)

july25_30C_temp <- july25_30C %>% 
  mutate(temperature = 30)

all_temps <- bind_rows(june29_41_temp, june28_34_temp, july25_30C_temp) %>% 
  filter(treatment == "fRS585") %>% 
  mutate(unique_well = paste(well,temperature, sep = "_"))


all_temps %>% 
  ggplot(aes(x = time, y = OD600, colour = factor(temperature), group = unique_well)) + geom_line(size = 1.5) +
  scale_colour_manual(values = c("darkblue", "darkorange1", "red")) +
  xlab("Time (s)") +
  # theme(axis.text = element_text(size = 20)) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  ) +
  theme(text=element_text(size=20, family="sans"))
ggsave("figures/temperature-growth.pdf", width = 7, height = 5)



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

##July 4th
##18C - manual reads
july04_18C <- read_excel("data-raw/July0423_18C.xlsx", sheet = "Sheet2", range = "A2:I19")

july04_18C <- july04_18C %>%
  filter(`Time [h]` != "Temp. [°C]") %>% 
  gather(2:9, key = time, value = OD600) %>% 
  rename(well = `Time [h]`) %>% 
  mutate(time = as.numeric(time))

july04_18C <- july04_18C %>%
  mutate(treatment = case_when(str_detect(well, "fRS585") ~ "fRS585",
                               str_detect(well, "Blank") ~ "Blank"))

july04_18C %>%
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("July 4th, 18C")


##july 5th 
## 30 C - ERROR MESSAGE IN EXCEL
july05_30C <- read_excel("data-raw/July0523_30C.xlsx", sheet = "Sheet3", range = "A2:CL19")

july05_30C <-july05_30C %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))

july05_30C <- july05_30C %>%
  mutate(treatment = case_when(str_detect(well, "fRS585") ~ "fRS585",
                               str_detect(well, "blank") ~ "Blank"))

july05_30C %>%
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("July 5th, 30C - plate reader error")

##40 C
july05_40C <- read_excel("data-raw/July0523_40C.xlsx", sheet = "Sheet3", range = "A2:CL19")

july05_40C <- july05_40C %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))

july05_40C <- july05_40C %>%
  mutate(treatment = case_when(str_detect(well, "fRS585") ~ "fRS585",
                               str_detect(well, "blank") ~ "Blank"))

july05_40C %>%
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("July 5th, 40C - REDO")

##July 6th 
## 37 C

july06_37C_raw <- read_excel("data-raw/July0623_37C.xlsx", sheet = "Sheet3", range = "A2:CL19")

july06_37C <- july06_37C_raw %>% 
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>%
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))

july06_37C <- july06_37C %>%
  mutate(treatment = case_when(str_detect(well, "fRS585") ~ "fRS585",
                               str_detect(well, "Blank") ~ "Blank"))

july06_37C %>% 
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("July 6th, 37C")

##40.5C
july06_40.5C <- read_excel("data-raw/July0623_40.5C.xlsx", sheet = "Sheet3", range = "A2:CL19")

july06_40.5C <- july06_40.5C %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))

july06_40.5C <- july06_40.5C %>%
  mutate(treatment = case_when(str_detect(well, "fRS585") ~ "fRS585",
                               str_detect(well, "Blank") ~ "Blank"))

july06_40.5C %>%
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("July 6th, 40.5C")

##July 13th 41C 48hr
july13_41C <- read_excel("data-raw/July1323_41C_48H.xlsx", sheet = "Sheet3", range = "A2:GL19")

july13_41C <- july13_41C %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  filter(is.numeric(OD600)) %>%
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) ##NAs introduced by coercion

july13_41C <- july13_41C %>%
  mutate(treatment = case_when(str_detect(well, "fRS585") ~ "fRS585",
                               str_detect(well, "Blank") ~ "Blank"))

july13_41C %>%
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("July 13th, 41C, 48hr")
## warning - removed 208 rows containing missing values 

##July 14th - 30C 
july14_30C <- read_excel("data-raw/July1423_30C.xlsx", sheet = "Sheet3", range = "A2:CS19")

july14_30C <- july14_30C %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) ## error here NAs introduced by coercion 

july14_30C <- july14_30C %>%
  mutate(treatment = case_when(str_detect(well, "fRS585") ~ "fRS585",
                               str_detect(well, "Blank") ~ "Blank"))

july14_30C %>%
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("July 14th, 30C")
### looks very very weird ...???
## doesnt note blank and everything is grey 
#another error with plate reader

## July 17th, 42C
july17_42C <- read_excel("data-raw/July1723_42C_72h.xlsx", sheet = "growthcurves", range = "A2:KH99")

july17_42C <- july17_42C %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>%
  mutate(time = as.numeric(time)) ## warning here NAs introduced by coercion 

july17_42C <- july17_42C %>%
  mutate(treatment = case_when(str_detect(well, "D3|B4|G4|G7|D8|B10|F9|F11") ~ "fRS585",
                               str_detect(well, "G2|C4|F4|B7|E6|C9|F10|D11") ~ "Blank"))

july17_42C %>%
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("July 17th, 42C, 72 hour read")
### weird NA line that is meant to be culture - grew well 

#July 25th, 30C
july25_30C <- read_excel("data-raw/July2523_30C.xlsx", sheet = "Sheet2", range = "A35:CT132")

july25_30C <- july25_30C %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>%
  mutate(time = as.numeric(time)) 

july25_30C <- july25_30C %>%
  mutate(treatment = case_when(str_detect(well, "B9|C6|D2|D8|E3|E10|F5|G8") ~ "fRS585",
                               str_detect(well, "B3|B6|C10|D4|E7|F11|G3|G6") ~ "Blank"))

july25_30C %>%
  ggplot(aes(x = time, y = OD600, group = well, color = treatment)) + geom_line() +
  ggtitle("July 25th, 30C")

#July 25th, 20C
july25_20C <- read_excel("data-raw/July25_20C_rep2_combined_results.xlsx", sheet = "Sheet2")

july25_20C <- july25_20C %>%
  filter(`Time` != "Temp. [°C]") %>% 
  gather(2:9, key = time, value = OD600) %>% 
  rename(well = `Time`) %>%
  mutate(time = as.numeric(time)) 

july25_20C <- july25_20C %>%
  mutate(treatment = case_when(str_detect(well, "A2|A3|A4|A5|A6|A7|A8|A9") ~ "fRS585"))

july25_20C %>%
  ggplot(aes(x = time, y = OD600, group = well, color = "fRS585")) + geom_line() +
  ggtitle("July 27th, 20C")
