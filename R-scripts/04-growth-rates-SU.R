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
view(june30_40)
#the wells are still names A1, A2, etc

###A1
gr_june30_40A1 <- june30_40 %>% 
  filter(well == "A1") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

A1<-get.growth.rate(gr_june30_40$time_days, gr_june30_40$log_od,plot.best.Q = TRUE,id = 'A1')
A1$best.model
#"gr.lagsat"
A1$best.slope
#8.956667

###A2
gr_june30_40A2 <- june30_40 %>% 
  filter(well == "A2") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A2<-get.growth.rate(gr_june30_40$time_days, gr_june30_40$log_od,plot.best.Q = TRUE,id = 'A1')
A2$best.model
#"gr.lagsat"
A2$best.slope
#8.76243

###A3
gr_june30_40A3 <- june30_40 %>% 
  filter(well == "A3") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A3<-get.growth.rate(gr_june30_40$time_days, gr_june30_40$log_od,plot.best.Q = TRUE,id = 'A1')
A3$best.model
#"gr.lagsat"
A3$best.slope
#8.76243

###A4
gr_june30_40A4 <- june30_40 %>% 
  filter(well == "A4") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A4<-get.growth.rate(gr_june30_40$time_days, gr_june30_40$log_od,plot.best.Q = TRUE,id = 'A1')
A4$best.model
#"gr.lagsat"
A4$best.slope
#8.76243

###A5
gr_june30_40A5 <- june30_40 %>% 
  filter(well == "A5") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A5<-get.growth.rate(gr_june30_40$time_days, gr_june30_40$log_od,plot.best.Q = TRUE,id = 'A1')
A5$best.model
#"gr.lagsat"
A5$best.slope
#8.76243

###A6
gr_june30_40A6 <- june30_40 %>% 
  filter(well == "A6") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A6<-get.growth.rate(gr_june30_40$time_days, gr_june30_40$log_od,plot.best.Q = TRUE,id = 'A1')
A6$best.model
#"gr.lagsat"
A6$best.slope
#8.76243

###A7         
gr_june30_40A7 <- june30_40 %>% 
  filter(well == "A7") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A7<-get.growth.rate(gr_june30_40$time_days, gr_june30_40$log_od,plot.best.Q = TRUE,id = 'A1')
A7$best.model
#"gr.lagsat"
A7$best.slope                               
#8.76243

###A8
gr_june30_40A8 <- june30_40 %>% 
  filter(well == "A8") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A8<-get.growth.rate(gr_june30_40$time_days, gr_june30_40$log_od,plot.best.Q = TRUE,id = 'A1')
A8$best.model
#"gr.lagsat"
A8$best.slope                               
#8.76243

###WHY ARE ALL OF THEM THE SAME???

###AVERAGING GROWTH RATES
mean(c(8.956667, 8.76243, 8.76243, 8.76243, 8.76243, 8.76243, 8.76243, 8.76243))
#8.78671 