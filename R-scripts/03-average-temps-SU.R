#SVETA
#GROWTH CURVE AVG ACTUAL TEMPS
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(janitor)
install.packages("cowplot")
library(cowplot) #this package is an add-on to ggplot which automatically gives a theme

#June 30, 40 deg
###reading in June 30, 40 deg
#couldn't find a way to exclude the temp row when doing mean
#so ended up just selecting from B onwards when pulling in data
june30_40 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/June3023_40C.xlsx", range = "B40:CL137") %>%
  clean_names() %>% 
  view()

###avg temp
june30_40 %>% 
  top_n(n = 1) %>% 
  t() %>% 
  mean()
#answer: 40.04607
#checking in excel: 40.04606742

###June 30, 41 deg
june30_41 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/June2923_41C.xlsx", range = "B40:CL137") %>% 
  clean_names() %>% 
  view()

june30_41 %>% 
  top_n(n = 1) %>% 
  t() %>% 
  mean()
#answer: 41.1809

###June 29, 37 deg
june29_37 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/June2923_37C.xlsx", range = "B40:CL137") %>%
  clean_names() %>% 
  view()

june29_37 %>% 
  top_n(n = 1) %>% 
  t() %>% 
  mean()
#answer: 37.1191

###June 28, 30 deg
june28_30 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/June2823_30C.xlsx", range = "B40:CL137") %>% 
  clean_names() %>% 
  view()

june28_30 %>% 
  top_n(n = 1) %>% 
  t() %>% 
  mean()
#answer: 30.44719

###June 28, 34 deg
june28_34 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/June28_34C.xlsx", range = "B40:CL137") %>% 
  clean_names() %>% 
  view()

june28_34 %>% 
  top_n(n = 1) %>% 
  t() %>% 
  mean()
#answer: 34.13596

###July 1, 25 deg
july1_25 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/July0123_25C.xlsx", range = "B40:CL137") %>%
  clean_names() %>% 
  view()

july1_25 %>% 
  top_n(n = 1) %>% 
  t() %>% 
  mean()
#answer: 25.36742

###July 6, 40.5 deg
july6_40.5 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/July0623_40.5C.xlsx", range = "B35:CS132") %>%
  clean_names() %>% 
  view()

july6_40.5 %>% 
  top_n(n = 1) %>% 
  t() %>% 
  mean()
#answer: 40.53542

###July 6, 37 deg
july6_37 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/July0623_37C.xlsx", range = "B35:CS132") %>%
  clean_names() %>% 
  view()

july6_37 %>% 
  top_n(n = 1) %>% 
  t() %>% 
  mean()
#answer: 37.31042

###July 5, 40 deg
july5_40 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/July0523_40C.xlsx", range = "B40:CL137") %>% 
 clean_names() %>% 
  view()

july5_40 %>% 
  top_n(n = 1) %>% 
  t() %>% 
  mean()
#answer: 40.11685

###July 5, 30 deg
july5_30 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/July0523_30C.xlsx", range = "B41:CL137") %>% 
  clean_names() %>% 
  view()

july5_30 %>% 
  top_n(n = 1) %>% 
  t() %>% 
  mean()
#answer: 30.30562

#July 4, 18 deg
#this one was a long term manual read 
#it was growing at 18 deg but the reads were taken at 25 
#because the incubator can go to 18, but the plate reader can only go down to 25
#so no accurate actual temp data to calculate

## July 13th 
july13_41 <- read_excel("data-raw/July1323_41C_48H.xlsx", sheet = 3, range = "B2:gl19") %>%
  clean_names()%>%
  view()

july13_41 %>%
  top_n(n=1) %>% 
  t() %>% 
  mean()
#Selecting by x85509_7
#[1] 40.98083
#first temp value is 21.3 --> pull temps down

##July 14, 30 deg
july14_30 <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/July14_30C.xlsx", sheet = "Sheet2_no_chr", range = "b3:cs4")
View(july14_30)

july14_30 %>%
  top_n(n = 1) %>%
  t() %>%
  mean()
#Selecting by 85509.7
#[1] 30.29688
