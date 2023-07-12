#SVETA
#July 9th, 2023
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

###COMPARING THIS CODE WITH JOEY'S CODE FROM SLACK SENT ON JULY 11
### estimate growth rates
library(growthTools)
### plotting growth curves from yeast data
library(tidyverse)
library(readxl)
library(cowplot)
theme_set(theme_cowplot())

## import well plate key
wells <- read_excel("data-raw/Growth curve well labels.xlsx", sheet = 2) #%>% 
  view()

## define growth rate function
fit_growth <- function(df){
  res <- try(get.growth.rate(df$time_days, df$log_od, plot.best.Q = FALSE))
  if(class(res)!="try-error"){
    out1 <- data.frame(best_model = res$best.model)
    out2 <- data.frame(growth_rate = res$best.slope)
  }
  all <- bind_cols(out1, out2)
  all
}

plate_30 <- read_excel("data-raw/June1623_30C.xlsx", range = "A40:CL137") %>% 
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) %>% 
  mutate(temperature = 30)
view(plate_30)

plate_42 <- read_excel("data-raw/June1623_42C.xlsx", range = "A40:CL137") %>% 
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) %>% 
  mutate(temperature = 42)

all_plates <- bind_rows(plate_30, plate_42) %>% 
  mutate(unique_well = paste(well, temperature, sep = "_")) %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

df_split2 <- all_plates %>% 
  split(.$unique_well)  ## here we split the data frame into little mini dataframes, splitting by "unique_well" which is combination of well and temperature

#stops working here
output2 <- df_split2 %>%
  map_df(fit_growth, .id = "unique_well") 
## this map function allows us to apply the fit_growth function to each well 

View(output2)
View(wells)

o3 <- output2 %>% 
  separate(unique_well, into = c("well", "temperature")) %>% 
  left_join(., wells)
View(o3)

o3 %>% 
  ggplot(aes(x = treatment, y = growth_rate, color = temperature)) + geom_point()

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

###June 28, 30 deg
june28_30 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/June2823_30C.xlsx", range = "A40:CL137") %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) %>% 
  mutate(treatment = case_when(str_detect(well, "A") ~ "fRS585",
                               str_detect(well, "B") ~ "blank"))
view(june28_30)
#12 replicates

###A1
gr_june28_30A1 <- june28_30 %>% 
  filter(well == "A1") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A1<-get.growth.rate(gr_june28_30A1$time_days, gr_june28_30A1$log_od,plot.best.Q = TRUE,id = 'A1')
A1$best.model
#"gr.lagsat"
A1$best.slope   
#7.879176

###A2
gr_june28_30A2 <- june28_30 %>% 
  filter(well == "A2") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A2<-get.growth.rate(gr_june28_30A2$time_days, gr_june28_30A2$log_od,plot.best.Q = TRUE,id = 'A2')
A2$best.model
#"gr.lagsat"
A2$best.slope   
#7.871784

###A3
gr_june28_30A3 <- june28_30 %>% 
  filter(well == "A3") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A3<-get.growth.rate(gr_june28_30A3$time_days, gr_june28_30A3$log_od,plot.best.Q = TRUE,id = 'A3')
A3$best.model
#"gr.lagsat"
A3$best.slope 
# 10.26144

###A4
gr_june28_30A4 <- june28_30 %>% 
  filter(well == "A4") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A4<-get.growth.rate(gr_june28_30A4$time_days, gr_june28_30A4$log_od,plot.best.Q = TRUE,id = 'A4')
A4$best.model
#"gr.lagsat"
A4$best.slope 
#8.070424

###A5
gr_june28_30A5 <- june28_30 %>% 
  filter(well == "A5") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A5<-get.growth.rate(gr_june28_30A5$time_days, gr_june28_30A5$log_od,plot.best.Q = TRUE,id = 'A5')
A5$best.model
#"gr.lagsat"
A5$best.slope 
#7.978008

###A6
gr_june28_30A6 <- june28_30 %>% 
  filter(well == "A6") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A6<-get.growth.rate(gr_june28_30A6$time_days, gr_june28_30A6$log_od,plot.best.Q = TRUE,id = 'A6')
A6$best.model
#"gr.lagsat"
A6$best.slope 
#8.026189

###A7
gr_june28_30A7 <- june28_30 %>% 
  filter(well == "A7") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A7<-get.growth.rate(gr_june28_30A7$time_days, gr_june28_30A7$log_od,plot.best.Q = TRUE,id = 'A7')
A7$best.model
#"gr.lagsat"
A7$best.slope 
#7.912374

###A8
gr_june28_30A8 <- june28_30 %>% 
  filter(well == "A8") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A8<-get.growth.rate(gr_june28_30A8$time_days, gr_june28_30A8$log_od,plot.best.Q = TRUE,id = 'A8')
A8$best.model
#"gr.lagsat"
A8$best.slope 
#9.871143

###A9
gr_june28_30A9 <- june28_30 %>% 
  filter(well == "A9") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A9<-get.growth.rate(gr_june28_30A9$time_days, gr_june28_30A9$log_od,plot.best.Q = TRUE,id = 'A9')
A9$best.model
#"gr.lagsat"
A9$best.slope 
#7.969469

###A10
gr_june28_30A10 <- june28_30 %>% 
  filter(well == "A10") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A10<-get.growth.rate(gr_june28_30A10$time_days, gr_june28_30A10$log_od,plot.best.Q = TRUE,id = 'A10')
A10$best.model
#"gr.lagsat"
A10$best.slope 
#7.947368

###A11
gr_june28_30A11 <- june28_30 %>% 
  filter(well == "A11") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A11<-get.growth.rate(gr_june28_30A11$time_days, gr_june28_30A11$log_od,plot.best.Q = TRUE,id = 'A11')
A11$best.model
#"gr.lagsat"
A11$best.slope 
#8.075332

###A12
gr_june28_30A12 <- june28_30 %>% 
  filter(well == "A12") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400)

A12<-get.growth.rate(gr_june28_30A12$time_days, gr_june28_30A12$log_od,plot.best.Q = TRUE,id = 'A12')
A12$best.model
#"gr.lagsat"
A12$best.slope 
#10.22589

#AVERAGING June 28, 30 deg
#A1 7.879176
#A2 7.871784
#A3 10.26144
#A4 8.070424
#A5 7.978008
#A6 8.026189
#A7 7.912374
#A8 9.871143
#A9 7.969469
#A10 7.947368
#A11 8.075332
#A12 10.22589

mean(c(7.879176, 7.871784, 10.26144, 8.070424, 7.978008, 8.026189, 7.912374, 9.871143,7.969469, 7.947368, 8.075332, 10.22589))
#8.507383

###June 28, 34 deg
june28_34 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/June28_34C.xlsx", range = "A40:CL137") %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) %>% 
  mutate(treatment = case_when(str_detect(well, "A") ~ "fRS585",
                               str_detect(well, "B") ~ "blank"))
view(june28_34)
#12 replicates

###A1
gr_june28_34A1 <- june28_34 %>% 
  filter(well == "A1") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) #%>% 
view

A1<-get.growth.rate(gr_june28_34A1$time_days, gr_june28_34A1$log_od,plot.best.Q = TRUE,id = 'A1')
A1$best.model
#"gr.lagsat"
A1$best.slope
#8.887984

###A2
gr_june28_34A2 <- june28_34 %>% 
  filter(well == "A2") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
view

A2<-get.growth.rate(gr_june28_34A2$time_days, gr_june28_34A2$log_od,plot.best.Q = TRUE,id = 'A2')
A2$best.model
#"gr.lagsat"
A2$best.slope
#9.143289

###A3
gr_june28_34A3 <- june28_34 %>% 
  filter(well == "A3") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A3<-get.growth.rate(gr_june28_34A3$time_days, gr_june28_34A3$log_od,plot.best.Q = TRUE,id = 'A3')
A3$best.model
#"gr.lagsat"
A3$best.slope
#8.895575

###A4
gr_june28_34A4 <- june28_34 %>% 
  filter(well == "A4") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A4<-get.growth.rate(gr_june28_34A4$time_days, gr_june28_34A4$log_od,plot.best.Q = TRUE,id = 'A4')
A4$best.model
#"gr.lagsat"
A4$best.slope
#9.047159

###A5
gr_june28_34A5 <- june28_34 %>% 
  filter(well == "A5") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A5<-get.growth.rate(gr_june28_34A5$time_days, gr_june28_34A5$log_od,plot.best.Q = TRUE,id = 'A5')
A5$best.model
#"gr.lagsat"
A5$best.slope
#9.002168

###A6
gr_june28_34A6 <- june28_34 %>% 
  filter(well == "A6") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A6<-get.growth.rate(gr_june28_34A6$time_days, gr_june28_34A6$log_od,plot.best.Q = TRUE,id = 'A6')
A6$best.model
#"gr.lagsat"
A6$best.slope
#8.989232

###A7
gr_june28_34A7 <- june28_34 %>% 
  filter(well == "A7") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A7<-get.growth.rate(gr_june28_34A7$time_days, gr_june28_34A7$log_od,plot.best.Q = TRUE,id = 'A7')
A7$best.model
#"gr.lagsat"
A7$best.slope
#10.67995

###A8
gr_june28_34A8 <- june28_34 %>% 
  filter(well == "A8") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A8<-get.growth.rate(gr_june28_34A8$time_days, gr_june28_34A8$log_od,plot.best.Q = TRUE,id = 'A8')
A8$best.model
#"gr.lagsat"
A8$best.slope
#8.9228

###A9
gr_june28_34A9 <- june28_34 %>% 
  filter(well == "A9") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A9<-get.growth.rate(gr_june28_34A9$time_days, gr_june28_34A9$log_od,plot.best.Q = TRUE,id = 'A9')
A9$best.model
#"gr.lagsat"
A9$best.slope
#8.913923

###A10
gr_june28_34A10 <- june28_34 %>% 
  filter(well == "A10") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A10<-get.growth.rate(gr_june28_34A10$time_days, gr_june28_34A10$log_od,plot.best.Q = TRUE,id = 'A10')
A10$best.model
#"gr.lagsat"
A10$best.slope
#9.008103

###A11
gr_june28_34A11 <- june28_34 %>% 
  filter(well == "A11") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A11<-get.growth.rate(gr_june28_34A11$time_days, gr_june28_34A11$log_od,plot.best.Q = TRUE,id = 'A11')
A11$best.model
#"gr.lagsat"
A11$best.slope
#8.789357

###A12
gr_june28_34A12 <- june28_34 %>% 
  filter(well == "A12") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) 

A12<-get.growth.rate(gr_june28_34A12$time_days, gr_june28_34A12$log_od,plot.best.Q = TRUE,id = 'A12')
A12$best.model
#"gr.lagsat"
A12$best.slope
#8.718535

#AVERGAING JUNE 28 34 DEG
#A1 8.887984
#A2 9.143289
#A3 8.895575
#A4 9.047159
#A5 9.002168
#A6 8.989232
#A7 10.67995
#A8 8.9228
#A9 8.913923
#A10 9.008103
#A11 8.789357
#A12 8.718535

mean(c(8.887984, 9.143289, 8.895575, 9.047159, 9.002168, 8.989232, 10.67995, 8.9228, 8.913923, 9.008103, 8.789357, 8.718535))
#9.083173

###July 4, 18 deg
#manual read
#things here are because Sveta tried to merge 
<<<<<<< HEAD:R-scripts/04-growth-rates-SU.R
#downloaded the combined results (currently not in Git)
#then found the wells that had fRS585
#renamed them manually on the excel file (fRS585 --> well plate number (ie B5, etc))
#need to change sheet, range, time from s to h, gather in the first function
#for the rest, business as usual
======
  july4_18 <- read_excel("C:/Users/sveta/Downloads/July4_18deg.xlsx", sheet = 2, range = "A2:I11") %>% 
  filter(`Time [h]` != "Temp. [°C]") %>% 
  gather(2:9, key = time, value = OD) %>% 
  rename(well = `Time [h]`) %>% 
  mutate(time = as.numeric(time)) %>% 
  view()

###B5
july4_18B5 <- july4_18 %>% 
  filter(well == "B5") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 24) %>% 
  view()

B5 <- get.growth.rate(july4_18B5$time,july4_18B5$log_od,plot.best.Q = TRUE,id = 'B5')
B5$best.model
#"gr.sat"
B5$best.slope
#0.1003318

###B10
july4_18B10 <- july4_18 %>% 
  filter(well == "B10") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 24) #%>% 
view()

B10 <- get.growth.rate(july4_18B10$time,july4_18B10$log_od,plot.best.Q = TRUE,id = 'B10')
B10$best.model
#"gr.sat"
B10$best.slope
#0.09947257

###C3
july4_18C3 <- july4_18 %>% 
  filter(well == "C3") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 24) 

C3 <- get.growth.rate(july4_18C3$time,july4_18C3$log_od,plot.best.Q = TRUE,id = 'C3')
C3$best.model
#"gr.sat"
C3$best.slope
#0.09949161

###D8
july4_18D8 <- july4_18 %>% 
  filter(well == "D8") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 24) 

D8 <- get.growth.rate(july4_18D8$time,july4_18D8$log_od,plot.best.Q = TRUE,id = 'D8')
D8$best.model
#"gr.sat"
D8$best.slope
#0.09896106

###E6
july4_18E6 <- july4_18 %>% 
  filter(well == "E6") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 24) 

E6 <- get.growth.rate(july4_18E6$time,july4_18E6$log_od,plot.best.Q = TRUE,id = 'E6')
E6$best.model
#"gr.sat"
E6$best.slope
#0.1018986

###F11
july4_18F11 <- july4_18 %>% 
  filter(well == "F11") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 24) 

F11 <- get.growth.rate(july4_18F11$time,july4_18F11$log_od,plot.best.Q = TRUE,id = 'F11')
F11$best.model
#"gr.sat"
F11$best.slope
#0.0979968

###G2
july4_18G2 <- july4_18 %>% 
  filter(well == "G2") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 24) 

G2 <- get.growth.rate(july4_18G2$time,july4_18G2$log_od,plot.best.Q = TRUE,id = 'G2')
G2$best.model
#"gr.sat"
G2$best.slope
# 0.09764423

###G7
july4_18G7 <- july4_18 %>% 
  filter(well == "G7") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 24) 

G7 <- get.growth.rate(july4_18G7$time,july4_18G7$log_od,plot.best.Q = TRUE,id = 'G7')
G7$best.model
#"gr.sat"
G7$best.slope

#AVERAGING JULY 4 18 DEG
#B5 0.1003318
#B10 0.09947257
#C3 0.09949161
#D8 0.09896106
#E6 0.1018986
#F11 0.0979968
#G2  0.09764423
#G7 0.1009986

mean(c(0.1003318, 0.09947257, 0.09949161, 0.09896106, 0.1018986, 0.0979968, 0.09764423, 0.1009986))
#0.09959941


>>>>>>> bb4bc9c41bf8acce42eabb94236504528c4099c5:R-scripts/04-growth-rates.R

gr_july4_18B5 <- july4_18 %>% 
  filter(well == "V1") %>% 
  

gr_june30_41A1 <- june30_41 %>% 
  filter(well == "A1") %>% 
  mutate(log_od = log(OD)) %>% 
  mutate(time_days = time / 86400) %>% 
  view

## Ije's Growth Rates 
##july 1st, 8 replicates 
library(dplyr)

library(growthTools)

### plotting growth curves from yeast data

library(tidyverse)
library(readxl)
library(cowplot)
theme_set(theme_cowplot())

## import well plate key
wells <- read_excel("data-raw/Growth curve well labels.xlsx", sheet = "01.07")

## define growth rate function - reminder to as more about these lines of cone 
fit_growth <- function(df){
  res <- try(get.growth.rate(df$time_days, df$log_od, plot.best.Q = FALSE))
  if(class(res)!="try-error"){
    out1 <- data.frame(best_model = res$best.model)
    out2 <- data.frame(growth_rate = res$best.slope)
  }
  all <- bind_cols(out1, out2)
  all
}

july01_25C <- read_excel("data-raw/July0123_25C.xlsx", range = "A40:CL137") %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) %>%
  mutate(temperature = 42)

july01_25C_all <- july01_25C %>% 
  mutate(unique_well = paste(well, temperature, sep = "_")) %>% 
  mutate(log_od = log(OD600)) %>% 
  mutate(time_days = time / 86400) 

df_split2 <- july01_25C_all %>% 
  split(.$unique_well) ## here we split the data frame into little mini dataframes, splitting by "unique_well" which is combination of well and temperature

output2 <- df_split2 %>%
  map_df(fit_growth, .id = "unique_well") ## this map function allows us to apply the fit_growth function to each well 

o3 <- output2 %>% 
  separate(unique_well, into = c("well", "temperature")) %>% 
  left_join(., wells)
## error in left join - possibly because I am only using one temperature?
o3 %>% 
  ggplot(aes(x = treatment, y = growth_rate, color = temperature)) + geom_point()


### did all code for each well using the same lines (913 - 923), just deleted and replaced with according well



A4 <- get.growth.rate(july01_25C_A4 $time_days, july01_25C_A4$log_od, plot.best.Q = TRUE,id = 'fRS585')
B10$best.model

B10$best.slope

##G2 6.978653
##C3 7.13187
##B5 5.427084
##E6 7.139992
##G7 7.023783
##D8 7.258182
## code randomly stopped working?? might have been in error in how i did it so am redoing 
##B10
##F11



##37*C
#Blank (Green) - C2, F3, G5, E6, B8, F8, D10, G10
#Culture (Red) - G2, D3, B4, D7, G7, C9, B11, E11

#40.5*C
#Blank (blue) - F2, D4, F6, B8, C9, D10, G11, G8
#Culture (Purple) - D2, G4, C5, E5, E8, F10, B10, D11