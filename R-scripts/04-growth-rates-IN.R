## 04-GROWTH-RATES-IN.R
## July 12, 2023 - starting voer after meeting and problems with pulling
##july 1st, 8 replicates 
library(dplyr)
library(growthTools)

##Joey's Curve
### plotting growth curves from yeast data
library(tidyverse)
library(readxl)
library(cowplot)
theme_set(theme_cowplot())

## import well plate key
wells <- read_excel("data-raw/Growth curve well labels.xlsx", sheet = "01.07")

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
  separate(unique_well, into = c("well", "temperature")) #%>% 
#left_join(., wells)
## error in left join - possibly because I am only using one temperature?
o3 %>% 
  ggplot(aes(x = treatment, y = growth_rate, color = temperature)) + geom_point()


##July 1st
july01_25C <- read_excel("data-raw/July0123_25C.xlsx", range = "A40:CL137") %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) # %>% 
##mutate(temperature = 42)

### did all code for each well using the same lines (913 - 923), just deleted and replaced with according well
july01_25C_G2 <- july01_25C %>% 
  filter(well == "G2") %>% 
  mutate(log_od = log(OD600)) %>% 
  mutate(time_days = time / 86400)

G2 <- get.growth.rate(july01_25C_G2$time_days, july01_25C_G2$log_od, plot.best.Q = TRUE,id = 'fRS585')
G2$best.model

G2$best.slope

##G2 6.978653 
##C3 7.13187 
##B5 5.427084 
##E6 7.139992 
##G7 7.023783 
##D8 7.258182 
##B10 7.156545 
##F11 7.035524

#mean: 6.893954

##July 5th 
#40*C
july05_40C <- read_excel("data-raw/July0523_40C.xlsx", range = "A40:CL137") %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) # %>% 
##mutate(temperature = 42)

### did all code for each well using the same lines, just deleted and replaced with according well
july05_40C_G9 <- july05_40C %>% 
  filter(well == "G9") %>% 
  mutate(log_od = log(OD600)) %>% 
  mutate(time_days = time / 86400)

G9 <- get.growth.rate(july05_40C_G9$time_days, july05_40C_G9$log_od, plot.best.Q = TRUE,id = 'fRS585')
G9$best.model

G9$best.slope

#B3, 10.18729
#C9, 7.463965
#D3, 9.723999
#D6, 5.415899 - randomly very low compared to the rest
#E11, 10.54608
#F2, 7.591047
#G4, 10.43612
#G9, 10.63997

mean(c(10.18729, 7.463965, 9.723999, 5.415899, 10.54608, 7.591047, 10.43612, 10.63997))
##9.000546

##30*C BAD READ 
{
july05_30C <- read_excel("data-raw/July0523_30C.xlsx", range = "A41:CL138") %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) 

july05_30C_G11 <- july05_30C %>% 
  filter(well == "G11") %>% 
  mutate(log_od = log(OD600)) %>% 
  mutate(time_days = time / 86400)

G11 <- get.growth.rate(july05_30C_G11$time_days, july05_30C_G11$log_od, plot.best.Q = TRUE,id = 'fRS585')
G11$best.model

G11$best.slope

#B5 0.03180882
#D2 - 0.1986335
#D7 0.07403113
#D9 -0.06777814
#E4 0.09235145
#G3 0.2319891
#G7 0.1713046
#G11 0.1846953

mean(c(0.03180882, -0.1986335, 0.07403113, -0.06777814, 0.09235145, 0.2319891, 0.1713046, 0.1846953))
#0.0649711
}

##July 6th 
##37*C
july06_37C <- read_excel("data-raw/July0623_37C.xlsx", range = "A35:CT132") %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) # %>% 
##mutate(temperature = 42)

### did all code for each well using the same lines, just deleted and replaced with according well
july06_37C_E11 <- july06_37C %>% 
  filter(well == "E11") %>% 
  mutate(log_od = log(OD600)) %>% 
  mutate(time_days = time / 86400)

E11 <- get.growth.rate(july06_37C_E11$time_days, july06_37C_E11$log_od, plot.best.Q = TRUE,id = 'fRS585')
E11$best.model

E11$best.slope

#G2, 8.051096
#D3, 8.119615
#B4, 11.44259
#D7, 10.72757
#G7, 10.68343
#C9, 11.225
#B11, 10.61753
#E11, 8.046793

mean(c(8.051096, 8.119615, 11.44259, 10.72757, 10.68343, 11.225, 10.61753, 8.046793))
##9.864203

#40.5*C
july06_40.5C <- read_excel("data-raw/July0623_40.5C.xlsx", range = "A35:CS132") %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) # %>% 
##mutate(temperature = 42)

### did all code for each well using the same lines, just deleted and replaced with according well
july06_40.5C_D11 <- july06_40.5C %>% 
  filter(well == "D11") %>% 
  mutate(log_od = log(OD600)) %>% 
  mutate(time_days = time / 86400)

D11 <- get.growth.rate(july06_40.5C_D11$time_days, july06_40.5C_D11$log_od, plot.best.Q = TRUE,id = 'fRS585')
D11$best.model

D11$best.slope
#D2, 6.7881
#G4, 6.373724
#C5, 3.125253
#E5, 3.972342
#E8, 3.28597
#F10, 6.06593
#B10, 7.904557
#D11, 8.573697

mean(c(6.7881, 6.373724, 3.125253, 3.972342, 3.28597, 6.06593, 7.904557, 8.573697))
##5.761197

##July 13th
## 41*C 48 hour read ##EXCLUDE G11
july13_41C <- read_excel("data-raw/July1323_41C_48H.xlsx", sheet = "Sheet1")%>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  filter(is.numeric(OD600)) %>%
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))

### did all code for each well using the same lines, just deleted and replaced with according well
july13_41C_D3 <- july13_41C %>% 
  filter(well == "G11") %>% 
  mutate(log_od = log(OD600)) %>% 
  mutate(time_days = time / 86400)

D3 <- get.growth.rate(july13_41C_D3$time_days, july13_41C_D3$log_od, plot.best.Q = TRUE,id = 'fRS585')
D3$best.model

D3$best.slope
#D3 - 2.772583
#B4 - 2.677185
#G4 - 2.568138
#G7 - 2.593191
#D8 - 1.970546
#F9 - 2.75412
#B10 - 2.607848
#G11 - 2.433683

mean(c(2.772583, 2.677185, 2.568138, 2.593191, 1.970546, 2.75412, 2.607848))
#2.563373 - without G11

mean(c(2.772583, 2.677185, 2.568138, 2.593191, 1.970546, 2.75412, 2.607848, 2.433683))
# 2.547162 - wiht G11


##July 14th - 30C
july14_30C <- read_excel("data-raw/July1423_30C.xlsx", sheet = "Sheet2_no_chr", range = "A3:CS88")

july14_30C <- july14_30C %>%
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD600) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time)) ## error here NAs introduced by coercion 

july14_30C_D2 <- july14_30C %>% 
  filter(well == "C10") %>% 
  mutate(log_od = log(OD600)) %>% 
  mutate(time_days = time / 86400)

D2 <- get.growth.rate(july14_30C_D2$time_days, july14_30C_D2$log_od, plot.best.Q = TRUE,id = 'fRS585')
## WARNING MESSAGE + LOOKS WEIRD
D2$best.model

D2$best.slope

#D2, 0.01724928
#F3, 0.03782801
#C4,0.006924409
#D6,0.006924409
#B7, 0.01913698
#E8,0.0084912
#G9, -0.009061736
#C10 0.00436073

mean(c(0.01724928, 0.03782801, 0.006924409, 0.006924409, 0.01913698, 0.0084912, -0.009061736, 0.00436073))
#0.01148166

##July 17th, 42C, 72hr read
july17_42C <- read_excel("data-raw/July1723_42C_72h.xlsx", sheet = "Working", range = "A3:KH100")


## import well plate key
wells <- read_excel("data-raw/Growth curve well labels.xlsx", sheet = "01.07")

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
  separate(unique_well, into = c("well", "temperature")) #%>% 
#left_join(., wells)
## error in left join - possibly because I am only using one temperature?
o3 %>% 
  ggplot(aes(x = treatment, y = growth_rate, color = temperature)) + geom_point()




