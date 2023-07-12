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

#D2, 
#G4, 
#C5, 
#E5, 
#E8, 
#F10, 
#B10, 
#D11
