


#### growth curve fitting
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(cowplot)
theme_set(theme_cowplot())



data <- read_excel("data-raw/Growth-Curves/Block 1/Block1_42C/Nick_Feb1_25_Nglab_Block1_42C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))

str(data)

plate_layout <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block1") %>% 
  mutate(well = str_to_lower(well))


d2 <- left_join(data, plate_layout)



d2 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()


library(growthTools)
library(growthrates)

library(dplyr)

gdat <- d2 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    fpath = "figures/"           # NA to display plots interactively
  ))


summary_df <- gdat %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout)


### growth at 42 degrees

summary_df %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()
