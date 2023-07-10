#SVETA
#GROWTH RATES

###CODE FROM JOEY (SLACK)
### the very first growth curve, all flz/casp res strains

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
  mutate(time_days = time / 86400) #%>% 
  view

A1<-get.growth.rate(gr_june30_40A1$time_days, gr_june30_40A1$log_od,plot.best.Q = TRUE,id = 'A1')
A1$best.model
#"gr.lagsat"
A1$best.slope
#8.956667

###A2
gr_june30_40A2 <- june30_40 %>% 
  filter(well == "A2") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A2<-get.growth.rate(gr_june30_40A2$time_days, gr_june30_40A2$log_od,plot.best.Q = TRUE,id = 'A2')
A2$best.model
#"gr.lagsat"
A2$best.slope
#8.76243

###A3
gr_june30_40A3 <- june30_40 %>% 
  filter(well == "A3") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A3<-get.growth.rate(gr_june30_40A3$time_days, gr_june30_40A3$log_od,plot.best.Q = TRUE,id = 'A3')
A3$best.model
#"gr.lagsat"
A3$best.slope
#10.45224

###A4
gr_june30_40A4 <- june30_40 %>% 
  filter(well == "A4") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A4<-get.growth.rate(gr_june30_40A4$time_days, gr_june30_40A4$log_od,plot.best.Q = TRUE,id = 'A4')
A4$best.model
#"gr.lagsat"
A4$best.slope
#10.12948

###A5
gr_june30_40A5 <- june30_40 %>% 
  filter(well == "A5") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A5<-get.growth.rate(gr_june30_40A5$time_days, gr_june30_40A5$log_od,plot.best.Q = TRUE,id = 'A5')
A5$best.model
#"gr.lagsat"
A5$best.slope
#9.300395

###A6
gr_june30_40A6 <- june30_40 %>% 
  filter(well == "A6") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A6<-get.growth.rate(gr_june30_40A6$time_days, gr_june30_40A6$log_od,plot.best.Q = TRUE,id = 'A6')
A6$best.model
#"gr.lagsat"
A6$best.slope
#9.390542

###A7         
gr_june30_40A7 <- june30_40 %>% 
  filter(well == "A7") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A7<-get.growth.rate(gr_june30_40A7$time_days, gr_june30_40A7$log_od,plot.best.Q = TRUE,id = 'A7')
A7$best.model
#"gr.lagsat"
A7$best.slope                               
#8.741537

###A8
gr_june30_40A8 <- june30_40 %>% 
  filter(well == "A8") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A8<-get.growth.rate(gr_june30_40A8$time_days, gr_june30_40A8$log_od,plot.best.Q = TRUE,id = 'A8')
A8$best.model
#"gr.lagsat"
A8$best.slope                               
#5.309089

###A9
gr_june30_40A9 <- june30_40 %>% 
  filter(well == "A9") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A9<-get.growth.rate(gr_june30_40A9$time_days, gr_june30_40A9$log_od,plot.best.Q = TRUE,id = 'A9')
A9$best.model
#"gr.lagsat"
A9$best.slope     
#4.707004

###A10
gr_june30_40A10 <- june30_40 %>% 
  filter(well == "A10") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A10<-get.growth.rate(gr_june30_40A10$time_days, gr_june30_40A10$log_od,plot.best.Q = TRUE,id = 'A10')
A10$best.model
#"gr.lagsat"
A10$best.slope    
#6.062713

###A11
gr_june30_40A11 <- june30_40 %>% 
  filter(well == "A11") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A11<-get.growth.rate(gr_june30_40A11$time_days, gr_june30_40A11$log_od,plot.best.Q = TRUE,id = 'A11')
A11$best.model
#"gr.lagsat"
A11$best.slope  
#8.68364

###A12
gr_june30_40A12 <- june30_40 %>% 
  filter(well == "A12") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A12<-get.growth.rate(gr_june30_40A12$time_days, gr_june30_40A12$log_od,plot.best.Q = TRUE,id = 'A12')
A12$best.model
#"gr.lagsat"
A12$best.slope  
#9.335999

###AVERAGING GROWTH RATES
#A1 8.956667
#A2 8.76243
#A3 10.45224
#A4 10.12948
#A5 9.300395
#A6 9.390542
#A7 8.741537
#A8 5.309089
#A9 4.707004
#A10 6.062713
#A11 8.68364
#A12 9.335999

mean(c(8.956667, 8.76243, 10.45224, 10.12948, 9.300395,9.390542, 8.741537, 5.309089, 4.707004, 6.062713,9.335999))   
# 8.286191

###June 30, 41 deg
june30_41 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/June2923_41C.xlsx", range = "A40:CL137") %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) %>% 
  mutate(treatment = case_when(str_detect(well, "A") ~ "fRS585",
                               str_detect(well, "B") ~ "blank"))
view(june30_41)

###A1
gr_june30_41A1 <- june30_41 %>% 
  filter(well == "A1") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
view

A1<-get.growth.rate(gr_june30_41A1$time_days, gr_june30_41A1$log_od,plot.best.Q = TRUE,id = 'A1')
A1$best.model
#"gr.lagsat"
A1$best.slope
#1.248718

###A2
gr_june30_41A1 <- june30_41 %>% 
  filter(well == "A2") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A2<-get.growth.rate(gr_june30_41A2$time_days, gr_june30_41A2$log_od,plot.best.Q = TRUE,id = 'A2')
A2$best.model
#"gr.lagsat"
A2$best.slope
#8.76243

###A3
gr_june30_41A3 <- june30_41 %>% 
  filter(well == "A3") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A3<-get.growth.rate(gr_june30_41A3$time_days, gr_june30_41A3$log_od,plot.best.Q = TRUE,id = 'A3')
A3$best.model
#"gr.lagsat"
A3$best.slope
#0.7634547

###A4
gr_june30_41A4 <- june30_41 %>% 
  filter(well == "A4") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A4<-get.growth.rate(gr_june30_41A4$time_days, gr_june30_41A4$log_od,plot.best.Q = TRUE,id = 'A4')
A4$best.model
#"gr.lagsat"
A4$best.slope
#0.5987903

###A5
gr_june30_41A5 <- june30_41 %>% 
  filter(well == "A5") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A5<-get.growth.rate(gr_june30_41A5$time_days, gr_june30_41A5$log_od,plot.best.Q = TRUE,id = 'A5')
A5$best.model
#"gr.lagsat"
A5$best.slope
#0.4896916

###A6
gr_june30_41A6 <- june30_41 %>% 
  filter(well == "A6") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A6<-get.growth.rate(gr_june30_41A6$time_days, gr_june30_41A6$log_od,plot.best.Q = TRUE,id = 'A6')
A6$best.model
#"gr.lagsat"
A6$best.slope
#0.4827641

###A7
gr_june30_41A7 <- june30_41 %>% 
  filter(well == "A7") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A7<-get.growth.rate(gr_june30_41A7$time_days, gr_june30_41A7$log_od,plot.best.Q = TRUE,id = 'A7')
A7$best.model
#"gr.lagsat"
A7$best.slope
#0.4814375

###A8
gr_june30_41A8 <- june30_41 %>% 
  filter(well == "A8") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A8<-get.growth.rate(gr_june30_41A8$time_days, gr_june30_41A8$log_od,plot.best.Q = TRUE,id = 'A8')
A8$best.model
#"gr.lagsat"
A8$best.slope
#0.5088377

###AVERAGING GROWTH RATES
#A1 1.248718
#A2 8.76243
#A3 0.7634547
#A4 0.5987903
#A5 0.4896916
#A6 0.4827641
#A7 0.4814375
#A8 0.5088377
#these are all over the place?

mean(c(1.248718, 8.76243, 0.7634547, 0.5987903, 0.4896916, 0.4827641, 0.4814375, 0.5088377))
#1.667015

###June 29, 37 deg
june29_37 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/June2923_37C.xlsx", range = "A40:CL137") %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) %>% 
  mutate(treatment = case_when(str_detect(well, "A") ~ "fRS585",
                               str_detect(well, "B") ~ "blank"))
view(june29_37)

###A1
gr_june29_37A1 <- june29_37 %>% 
  filter(well == "A1") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

A1<-get.growth.rate(gr_june29_37A1$time_days, gr_june29_37A1$log_od,plot.best.Q = TRUE,id = 'A1')
A1$best.model
#"gr.lagsat"
A1$best.slope
#8.449666

###A2
gr_june29_37A2 <- june29_37 %>% 
  filter(well == "A2") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

A2<-get.growth.rate(gr_june29_37A2$time_days, gr_june29_37A2$log_od,plot.best.Q = TRUE,id = 'A2')
A2$best.model
#"gr.lagsat"
A2$best.slope
#8.454333

###A3
gr_june29_37A3 <- june29_37 %>% 
  filter(well == "A3") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

A3<-get.growth.rate(gr_june29_37A3$time_days, gr_june29_37A3$log_od,plot.best.Q = TRUE,id = 'A3')
A3$best.model
#"gr.lagsat"
A3$best.slope
#8.360957

###A4
gr_june29_37A4 <- june29_37 %>% 
  filter(well == "A4") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

A4<-get.growth.rate(gr_june29_37A4$time_days, gr_june29_37A4$log_od,plot.best.Q = TRUE,id = 'A4')
A4$best.model
#"gr.lagsat"
A4$best.slope
#8.494267

###A5
gr_june29_37A5 <- june29_37 %>% 
  filter(well == "A5") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

A5<-get.growth.rate(gr_june29_37A5$time_days, gr_june29_37A5$log_od,plot.best.Q = TRUE,id = 'A5')
A5$best.model
#"gr.lagsat"
A5$best.slope
#8.516955

###A6
gr_june29_37A6 <- june29_37 %>% 
  filter(well == "A6") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

A6<-get.growth.rate(gr_june29_37A6$time_days, gr_june29_37A6$log_od,plot.best.Q = TRUE,id = 'A6')
A6$best.model
#"gr.lagsat"
A6$best.slope
#8.414727

###A7
gr_june29_37A7 <- june29_37 %>% 
  filter(well == "A7") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

A7<-get.growth.rate(gr_june29_37A7$time_days, gr_june29_37A7$log_od,plot.best.Q = TRUE,id = 'A7')
A7$best.model
#"gr.lagsat"
A7$best.slope
#8.568349

###A8
gr_june29_37A8 <- june29_37 %>% 
  filter(well == "A8") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

A8<-get.growth.rate(gr_june29_37A8$time_days, gr_june29_37A8$log_od,plot.best.Q = TRUE,id = 'A8')
A8$best.model
#"gr.lagsat"
A8$best.slope
#8.433414

###A9
gr_june29_37A9 <- june29_37 %>% 
  filter(well == "A9") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

A9<-get.growth.rate(gr_june29_37A9$time_days, gr_june29_37A9$log_od,plot.best.Q = TRUE,id = 'A9')
A9$best.model
#"gr.lagsat"
A9$best.slope
#8.196117

###A10
gr_june29_37A10 <- june29_37 %>% 
  filter(well == "A10") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

A10<-get.growth.rate(gr_june29_37A10$time_days, gr_june29_37A10$log_od,plot.best.Q = TRUE,id = 'A10')
A10$best.model
#"gr.lagsat"
A10$best.slope
#8.157105

###A11
gr_june29_37A11 <- june29_37 %>% 
  filter(well == "A11") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

A11<-get.growth.rate(gr_june29_37A11$time_days, gr_june29_37A11$log_od,plot.best.Q = TRUE,id = 'A11')
A11$best.model
#"gr.lagsat"
A11$best.slope
#8.532824

###A12
gr_june29_37A12 <- june29_37 %>% 
  filter(well == "A12") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

A12<-get.growth.rate(gr_june29_37A12$time_days, gr_june29_37A12$log_od,plot.best.Q = TRUE,id = 'A12')
A12$best.model
#"gr.lagsat"
A12$best.slope
#8.79377

#AVERAGING GROWTH RATES
#A1 8.449666
#A2 8.454333
#A3 8.360957
#A4 8.494267
#A5 8.516955
#A6 8.414727
#A7 8.568349
#A8 8.433414
#A9 8.196117
#A10 8.157105
#A11 8.532824
#A12 8.79377

mean(c(8.449666, 8.454333, 8.360957, 8.494267, 8.516955, 8.414727, 8.568349,8.433414, 8.196117, 8.157105, 8.532824,  8.79377))
#8.447707