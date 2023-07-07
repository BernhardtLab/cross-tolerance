#SVETA
#GROWTH RATES

###CODE FROM JOEY (SLACK)
### the very first growth curve, all flz/casp res strains
### plotting growth curves from yeast data

library(tidyverse)
library(readxl)
library(cowplot)
theme_set(theme_cowplot())


plate <- read_excel("data-raw/June1623_30C.xlsx", range = "A40:CL137")

plate2 <- plate %>% 
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) %>% 
  view


### this is one way of assigning treatments to wells. Alternatively, you could list out all the wells in an csv or xls file with their treatments and then do a left join. This would be my preferred appraoach
plate3 <- plate2 %>% 
  mutate(treatment = case_when(str_detect(well, "A") ~ "wt",
                               str_detect(well, "B") ~ "ftz1",
                               str_detect(well, "C") ~ "ftz2",
                               str_detect(well, "D") ~ "ftz3",
                               str_detect(well, "E") ~ "casp1",
                               str_detect(well, "F") ~ "casp2",
                               str_detect(well, "G") ~ "casp3",
                               str_detect(well, "H1|H2|H3") ~ "blank",
                               TRUE ~ "water")) ### this is a shortcut to assign all the remaining wells to water

## plot them all together (together)
plate3 %>% 
  ggplot(aes(x = time, y = OD, group = well, color = treatment)) + geom_line() 
  
## plot in facets (every in each window)
  plate3 %>% 
  ggplot(aes(x = time, y = OD, group = well, color = treatment)) + geom_line() + facet_wrap( ~ treatment)


### estimate growth rates
library(devtools)
install_github("ctkremer/mleTools") ## leaving these here so you can see how to install them
install_github("ctkremer/growthTools")
library(growthTools)


b1 <- plate3 %>% 
  filter(well == "B2") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) ### to get time in days (24*60*60)
b1

res<-get.growth.rate(b1$time_days,b1$log_od,plot.best.Q = TRUE,id = 'Population A')
res$best.model
res$best.slope

###TRYING ON MY OWN
###June 30, 40 deg
june30_40 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/June3023_40C.xlsx", range = "A40:CL137") %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) %>% 
  mutate(treatment = case_when(str_detect(well, "A") ~ "fRS585",
                               str_detect(well, "B") ~ "blank"))

gr_june30_40 <- june30_40 %>% 
  filter(well == "B2") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

res<-get.growth.rate(gr_june30_40$time_days, gr_june30_40$log_od,plot.best.Q = TRUE,id = 'Population A')
res$best.model
res$best.slope




         
                               

