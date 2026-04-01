
### Estimate growth rates in common garden experiments

# Author: Joey Bernhardt
# Input: Excel spreadsheets that are outputs from the Biotek plate reader
# Output: 
# Written for R version 4.2.3
# Last updated: July 10 2025



#### growth curve fitting
library(tidyverse)
library(readxl)
library(janitor)
library(plotrix)
library(lubridate)
library(cowplot)
theme_set(theme_cowplot())
library(growthTools)
library(growthrates)

library(dplyr)



# Block 1 -----------------------------------------------------------------

### 42 degrees
data <- read_excel("data-raw/Growth-Curves/Block 1/Block1_42C/Nick_Feb1_25_Nglab_Block1_42C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))



plate_layout <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block1") %>% 
  mutate(well = str_to_lower(well))


d2 <- left_join(data, plate_layout)



d2 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_42C_block1.png", width = 8, height = 6)


gdat <- d2 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    methods = c("sat", "flr"),
    fpath = "figures/block1/block1_42_no_lag/"           # NA to display plots interactively
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
  left_join(plate_layout) %>% 
  mutate(test_temperature = 42) %>% 
  mutate(block = 1)


### growth at 42 degrees

summary_df %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()


# block 1 41 degrees ------------------------------------------------------


data41 <- read_excel("data-raw/Growth-Curves/Block 1/Block1_41C/Nick_Feb7_25_Nglab_Block1_41C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


d41 <- left_join(data41, plate_layout)

d41 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_41C_block1.png", width = 8, height = 6)


gdat41 <- d41 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    methods = c("sat", "flr"),
    fpath = "figures/block1/block1_41_no_lag/"           # NA to display plots interactively
  ))


summary_df_41 <- gdat41 %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout) %>% 
  mutate(test_temperature = 41) %>% 
  mutate(block = 1)

summary_df_41 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()


# block 1 35 degrees ------------------------------------------------------


data35 <- read_excel("data-raw/Growth-Curves/Block 1/Block1_35C/Nick_Feb13_25_Nglab_Block1_35C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


d35 <- left_join(data35, plate_layout)
d35 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_35C_block1.png", width = 8, height = 6)

d35 %>% 
  filter(grepl("CASP", strain)) %>% 
  mutate(casp = ifelse(grepl("CASP_1", strain), "casp", "not_casp")) %>% 
  ggplot(aes(x = days, y = od, color = well, group = well)) + geom_line()
ggsave("figures/casp-od-time-35.png", width = 8, height = 6)


gdat35 <- d35 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    methods = c("sat", "flr"),
    fpath = "figures/block1/block1_35_no_lag/"           # NA to display plots interactively
  ))


summary_df_35 <- gdat35 %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout) %>% 
  mutate(test_temperature = 35) %>% 
  mutate(block = 1)

summary_df_35 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()



summary_df_35 %>% 
  filter(grepl("CASP", strain)) %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = well)) + geom_point()
ggsave("figures/35-casp-od.png", width = 8, height = 6)




# block 1 25 degrees ------------------------------------------------------


data25 <- read_excel("data-raw/Growth-Curves/Block 1/Block1_25C/Nick_July19_25_Nglab_Block1_25C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


d25 <- left_join(data25, plate_layout)
d25 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_25C_block1.png", width = 8, height = 6)

d25 %>% 
  filter(grepl("CASP", strain)) %>% 
  mutate(casp = ifelse(grepl("CASP_1", strain), "casp", "not_casp")) %>% 
  ggplot(aes(x = days, y = od, color = well, group = well)) + geom_line()
ggsave("figures/casp-od-time-25.png", width = 8, height = 6)


gdat25 <- d25 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    methods = c("sat", "flr"),
    plot.best.Q = TRUE,
    fpath = "figures/block1/block1_25_no_lag/"           # NA to display plots interactively
  ))


summary_df_25 <- gdat25 %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout) %>% 
  mutate(test_temperature = 25) %>% 
  mutate(block = 1)

summary_df_25 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()



summary_df_25 %>% 
  filter(grepl("CASP", strain)) %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = well)) + geom_point()
ggsave("figures/25-casp-od.png", width = 8, height = 6)



# block 1 38 degrees ------------------------------------------------------



data38 <- read_excel("data-raw/Growth-Curves/Block 1/Block1_38C/Nick_Aug22_25_Nglab_Block1_38C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


d38 <- left_join(data38, plate_layout)
d38 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_38C_block1.png", width = 8, height = 6)

d38 %>% 
  filter(grepl("CASP", strain)) %>% 
  mutate(casp = ifelse(grepl("CASP_1", strain), "casp", "not_casp")) %>% 
  ggplot(aes(x = days, y = od, color = well, group = well)) + geom_line()
ggsave("figures/casp-od-time-25.png", width = 8, height = 6)


gdat38 <- d38 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    methods = c("sat", "flr"),
    fpath = "figures/block1/block1_38_no_lag/"           # NA to display plots interactively
  ))


summary_df_38 <- gdat38 %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout) %>% 
  mutate(test_temperature = 38) %>% 
  mutate(block = 1)

summary_df_38 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()



summary_df_38 %>% 
  filter(grepl("CASP", strain)) %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = well)) + geom_point()
ggsave("figures/38-casp-od.png", width = 8, height = 6)









block1_all <- bind_rows(summary_df, summary_df_41, summary_df_41, summary_df_25, summary_df_38)



# block 2 -----------------------------------------------------------------


# block 2 42 degrees ------------------------------------------------------


### 42 degrees
data_b2_42 <- read_excel("data-raw/Growth-Curves/Block 2/Block2_42C/Nick_Feb2_25_Nglab_Block2_42C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block2 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block2") %>% 
  mutate(well = str_to_lower(well))


d42_b2 <- left_join(data_b2_42, plate_layout_block2)



d42_b2 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_42C_block2.png", width = 8, height = 6)



gdatb2_42 <- d42_b2 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    methods = c("sat", "flr"),
    fpath = "figures/block2/block2_42_no_lag/"           # NA to display plots interactively
  ))


summary_df_b2_42 <- gdatb2_42 %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout_block2) %>% 
  mutate(test_temperature = 42) %>% 
  mutate(block = 2)


### growth at 42 degrees

summary_df_b2_42 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()


# block 2 41 degrees ------------------------------------------------------


### 41 degrees
datab2_41 <- read_excel("data-raw/Growth-Curves/Block 2/Block2_41C/Nick_Feb11_25_Nglab_Block2_41C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block2 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block2") %>% 
  mutate(well = str_to_lower(well))


d41_b2 <- left_join(datab2_41, plate_layout_block2)



d41_b2 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_41C_block2.png", width = 8, height = 6)


gdatb2_41 <- d41_b2 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    methods = c("sat", "flr"),
    fpath = "figures/block2/block2_41_no_lag/"           # NA to display plots interactively
  ))


summary_df_b2_41 <- gdatb2_41 %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout_block2) %>% 
  mutate(test_temperature = 41) %>% 
  mutate(block = 2)


### growth at 41 degrees

summary_df_b2_41 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()

# block 2 35 degrees ------------------------------------------------------


### 35 degrees
datab2_35 <- read_excel("data-raw/Growth-Curves/Block 2/Block2_35C/Nick_Feb14_25_Nglab_Block2_35C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block2 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block2") %>% 
  mutate(well = str_to_lower(well))


d35_b2 <- left_join(datab2_35, plate_layout_block2)



d35_b2 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_35C_block2.png", width = 8, height = 6)



gdatb2_35 <- d35_b2 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    methods = c("sat", "flr"),
    fpath = "figures/block2/block2_35_no_lag/"           # NA to display plots interactively
  ))

View(gdatb2_35)

summary_df_b2_35 <- gdatb2_35 %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout_block2) %>% 
  mutate(test_temperature = 35) %>% 
  mutate(block = 2)
View(summary_df_b2_35)

### growth at 35 degrees

summary_df_b2_35 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()



# block 2 25 degrees ------------------------------------------------------



### 25 degrees
datab2_25 <- read_excel("data-raw/Growth-Curves/Block 2/Block2_25C/Nick_July20_25_Nglab_Block2_25C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block2 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block2") %>% 
  mutate(well = str_to_lower(well))


d25_b2 <- left_join(datab2_25, plate_layout_block2)



d25_b2 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_25C_block2.png", width = 8, height = 6)



gdatb2_25 <- d25_b2 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    methods = c("sat", "flr"),
    fpath = "figures/block2/block2_25_no_lag/"           # NA to display plots interactively
  ))

View(gdatb2_35)

summary_df_b2_25 <- gdatb2_25 %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout_block2) %>% 
  mutate(test_temperature = 25) %>% 
  mutate(block = 2)
View(summary_df_b2_35)

### growth at 25 degrees

summary_df_b2_25 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()





# block 2 38 degrees ------------------------------------------------------




### 38 degrees
datab2_38 <- read_excel("data-raw/Growth-Curves/Block 2/Block2_38C/Nick_Aug23_25_Nglab_Block2_38C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block2 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block2") %>% 
  mutate(well = str_to_lower(well))


d38_b2 <- left_join(datab2_38, plate_layout_block2)



d38_b2 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_38C_block2.png", width = 8, height = 6)



gdatb2_38 <- d38_b2 %>%
  mutate(ln_abundance = log(od)) %>% 
  filter(strain != "blank") %>% ### adding this here because some of the models failed, and looking above, the blanks all look flat, so no need to estimate growth rates
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    methods = c("sat", "flr"),
    fpath = "figures/block2/block2_38_no_lag/"           # NA to display plots interactively
  ))



summary_df_b2_38 <- gdatb2_38 %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout_block2) %>% 
  mutate(test_temperature = 38) %>% 
  mutate(block = 2)
View(summary_df_b2_35)

### growth at 28 degrees

summary_df_b2_38 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()






# block 3 -----------------------------------------------------------------


# block 3 42 degrees ------------------------------------------------------


### 42 degrees
data_b3_42 <- read_excel("data-raw/Growth-Curves/Block 3/Block3_42C/Nick_Feb3_25_Nglab_Block3_42C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block3 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block3") %>% 
  mutate(well = str_to_lower(well))


d42_b3 <- left_join(data_b3_42, plate_layout_block3)



d42_b3 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_42C_block3.png", width = 8, height = 6)



gdatb3_42 <- d42_b3 %>%
  filter(strain != "blank") %>% 
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    methods = c("sat", "flr"),
    fpath = "figures/block3/block3_42_no_lag/"           # NA to display plots interactively
  ))


summary_df_b3_42 <- gdatb3_42 %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout_block3) %>% 
  mutate(test_temperature = 42) %>% 
  mutate(block = 3)


### growth at 42 degrees

summary_df_b3_42 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()


# block 3 41 degrees ------------------------------------------------------


### 41 degrees
datab3_41 <- read_excel("data-raw/Growth-Curves/Block 3/Block3_41C/Nick_Feb12_25_Nglab_Block3_41C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block3 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block3") %>% 
  mutate(well = str_to_lower(well))


d41_b3 <- left_join(datab3_41, plate_layout_block3)



d41_b3 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_41C_block3.png", width = 8, height = 6)


gdatb3_41 <- d41_b3 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    methods = c("sat", "flr"),
    fpath = "figures/block3/block3_41_no_lag/"           # NA to display plots interactively
  ))


summary_df_b3_41 <- gdatb3_41 %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout_block3) %>% 
  mutate(test_temperature = 41) %>% 
  mutate(block = 3)


### growth at 41 degrees

summary_df_b3_41 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()

# block 3 35 degrees ------------------------------------------------------


### 35 degrees
datab3_35 <- read_excel("data-raw/Growth-Curves/Block 3/Block3_35C/Nick_Feb22_25_Nglab_Block3_35C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block3 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block3") %>% 
  mutate(well = str_to_lower(well))


d35_b3 <- left_join(datab3_35, plate_layout_block3)



d35_b3 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_35C_block3.png", width = 8, height = 6)



gdatb3_35 <- d35_b3 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    methods = c("sat", "flr"),
    fpath = "figures/block3/block3_35_no_lag/"           # NA to display plots interactively
  ))


summary_df_b3_35 <- gdatb3_35 %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout_block3) %>% 
  mutate(test_temperature = 35) %>% 
  mutate(block = 3)


### growth at 35 degrees

summary_df_b3_35 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()





# block 3 25 degrees ------------------------------------------------------


### 25 degrees
datab3_25 <- read_excel("data-raw/Growth-Curves/Block 3/Block3_25C/Nick_July24_25_Nglab_Block3_25C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block3 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block3") %>% 
  mutate(well = str_to_lower(well))


d25_b3 <- left_join(datab3_25, plate_layout_block3)



d35_b3 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_35C_block3.png", width = 8, height = 6)



gdatb3_25 <- d25_b3 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    methods = c("sat", "flr"),
    fpath = "figures/block3/block3_25_no_lag/"           # NA to display plots interactively
  ))


summary_df_b3_25 <- gdatb3_25 %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout_block3) %>% 
  mutate(test_temperature = 25) %>% 
  mutate(block = 3)


### growth at 25 degrees

summary_df_b3_25 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()






# block 3 38 degrees ------------------------------------------------------


### 38 degrees
datab3_38 <- read_excel("data-raw/Growth-Curves/Block 3/Block3_38C/Nick_Aug24_25_Nglab_Block3_38C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  dplyr::select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block3 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block3") %>% 
  mutate(well = str_to_lower(well))


d38_b3 <- left_join(datab3_38, plate_layout_block3)



d38_b3 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()
ggsave("figures/od_time_38C_block3.png", width = 8, height = 6)



gdatb3_38 <- d38_b3 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    methods = c("sat", "flr"),
    fpath = "figures/block3/block3_38_no_lag/"           # NA to display plots interactively
  ))


summary_df_b3_38 <- gdatb3_38 %>%
  summarise(
    well,
    mu = grs$best.slope,
    best_model = grs$best.model,
    se = grs$best.se,
    R2 = grs$best.model.rsqr,
    n_obs = grs$best.model.slope.n
  ) %>% 
  left_join(plate_layout_block3) %>% 
  mutate(test_temperature = 38) %>% 
  mutate(block = 3)


### growth at 38 degrees

summary_df_b3_38 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()








all_blocks <- bind_rows(summary_df, summary_df_35, summary_df_41, summary_df_25, summary_df_38, summary_df_b2_35,
                        summary_df_b2_38, summary_df_b2_25, summary_df_b2_41, summary_df_b2_42, summary_df_b3_35, summary_df_b3_41, summary_df_b3_42, summary_df_b3_25, summary_df_b3_38) %>% 
  filter(!grepl("LIG", strain)) %>% 
  filter(!grepl("blank", strain)) %>% 
  filter(!grepl("YPD", strain)) %>%
  filter(!grepl("588", strain)) %>% 
  filter(!grepl("FLZ_3", strain)) %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT_FLZ", strain) ~ "Fluconazole evolved",
                                       grepl("WT_CASP", strain) ~ "Caspofungin evolved",
                                       TRUE ~ strain))


write_csv(all_blocks, "data-processed/all-blocks-growth-no-lag.csv")


all_blocks_no_lag <- read_csv("data-processed/all-blocks-growth-no-lag.csv")

all_blocks %>% 
  filter(test_temperature == 41) %>% 
  filter(evolution_history == "Fluconazole evolved") %>%
  ggplot(aes(x = strain, y = mu)) + geom_point() +
  ggtitle("Growth rates at 41C")
ggsave("figures/growth-41-fluconazole.png", width = 8, height = 6)

all_blocks %>% 
  filter(test_temperature == 35) %>% 
  filter(evolution_history == "Caspofungin evolved") %>%
  ggplot(aes(x = strain, y = mu, color = factor(block))) + geom_point() +
  ggtitle("Growth rates at 35C")
ggsave("figures/growth-35-caspofungin.png", width = 8, height = 6)

all_blocks_no_lag %>% 
  ggplot(aes(x = evolution_history, y = mu, color = factor(block))) + geom_point()

all_sum <- all_blocks_no_lag %>% 
  group_by(evolution_history, test_temperature, block) %>% 
  summarise(mean_growth_rate = mean(mu),
            se_growth_rate = std.error(mu))

all_sum %>% 
  ggplot(aes(x = evolution_history, y = mean_growth_rate, color = factor(block))) + geom_point(size = 4) +
  geom_errorbar(aes(x = evolution_history, ymin = mean_growth_rate - se_growth_rate, ymax = mean_growth_rate + se_growth_rate), width = 0.4) +
  facet_wrap(~ test_temperature, scales = "free") +
  # theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave("figures/all-growth-rates.png", width = 14, height = 6)

all_sum %>% 
  ggplot(aes(x = evolution_history, y = mean_growth_rate, color = evolution_history)) + geom_point() +
  geom_errorbar(aes(x = evolution_history, ymin = mean_growth_rate - se_growth_rate, ymax = mean_growth_rate + se_growth_rate), width = 0.1) +
  facet_grid(test_temperature ~ block) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# ggsave("figures/all-growth-rates-same-scale.png", width = 14, height = 6)
ggsave("figures/all-growth-rates-same-scale-grid.png", width = 14, height = 6)

all_sum2 <- all_sum %>% 
  dplyr::select(-se_growth_rate) %>%
  ungroup() %>% 
  mutate(test_temperature = as.factor(test_temperature)) %>% 
  dplyr::select(-block)

View(all_sum2)

all_sum3 <- all_sum2 %>% 
  group_by(evolution_history, test_temperature) %>%
  summarise(mean_growth_rate2 = mean(mean_growth_rate),
            se_growth_rate = plotrix::std.error(mean_growth_rate))

all_sum3 %>% 
  ggplot(aes(x = evolution_history, y = mean_growth_rate2, color = evolution_history)) + geom_point() +
  geom_errorbar(aes(x = evolution_history, ymin = mean_growth_rate2 - se_growth_rate, ymax = mean_growth_rate2 + se_growth_rate), width = 1) +
  facet_wrap( ~ test_temperature, scales = "free") +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave("figures/all-growth-rates-same-scale-wrap.png", width = 14, height = 6)

all_sum3 %>% 
  ggplot(aes(x = evolution_history, y = mean_growth_rate2, color = evolution_history)) + geom_point() +
  geom_errorbar(aes(x = evolution_history, ymin = mean_growth_rate2 - se_growth_rate, ymax = mean_growth_rate2 + se_growth_rate), width = 1) +
  facet_wrap( ~ test_temperature) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave("figures/all-growth-rates-wrap.png", width = 14, height = 6)

all_sum3b <- all_sum3 %>% 
  mutate(test_temperature = as.numeric(as.character(test_temperature)))

all_sum3b %>% 
  ggplot(aes(x = test_temperature, y = mean_growth_rate2, color = evolution_history)) + geom_point() +
  geom_errorbar(aes(x = test_temperature, ymin = mean_growth_rate2 - se_growth_rate, ymax = mean_growth_rate2 + se_growth_rate), width = 1) 
ggsave("figures/all-growth-rates-wrap-no.png", width = 14, height = 6)




ab2 <- all_blocks %>% 
  filter(!evolution_history %in% c("Caspofungin evolved", "Fluconazole evolved")) %>% 
  mutate(test_temperature = as.numeric(as.character(test_temperature)))

# all_sum2 %>% 
#   mutate(test_temperature = as.numeric(as.character(test_temperature))) %>% 
#   filter(!evolution_history %in% c("Caspofungin evolved", "Fluconazole evolved")) %>% 
#   ggplot(aes(x = test_temperature, y = mean_growth_rate, color = evolution_history)) 
  
  ggplot() + 
  geom_jitter(aes( x= test_temperature, y = mu, color = evolution_history), data = ab2, alpha = 0.6) + 
  geom_point() +
  # geom_smooth(method = "lm", aes(color = evolution_history)) +
    
    geom_pointrange(aes(x = test_temperature, y = mean_growth_rate2, ymin = mean_growth_rate2 - se_growth_rate, ymax = mean_growth_rate2 + se_growth_rate, color = evolution_history), position = position_dodge2(width = 2), data = filter(all_sum3b,!evolution_history %in% c("Caspofungin evolved", "Fluconazole evolved")), size = 1) +
    geom_pointrange(aes(x = test_temperature, y = mean_growth_rate2, ymin = mean_growth_rate2 - se_growth_rate, ymax = mean_growth_rate2 + se_growth_rate), shape = 21, color = "black", position = position_dodge2(width = 2), data = filter(all_sum3b,!evolution_history %in% c("Caspofungin evolved", "Fluconazole evolved")), size = 1) +
    ylab("Growth rate (per day)") +
    xlab("Temperature (C)")
ggsave("figures/growth-rates-temp.png", width = 8, height = 6)
