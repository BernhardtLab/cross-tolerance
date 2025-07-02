


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
  select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))

str(data)

plate_layout <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block1") %>% 
  mutate(well = str_to_lower(well))


d2 <- left_join(data, plate_layout)



d2 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()


gdat <- d2 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    fpath = "figures/block1/block1_42/"           # NA to display plots interactively
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
  select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


d41 <- left_join(data41, plate_layout)

gdat41 <- d41 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    fpath = "figures/block1/block1_41/"           # NA to display plots interactively
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
  select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


d35 <- left_join(data35, plate_layout)

gdat35 <- d35 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    fpath = "figures/block1/block1_35/"           # NA to display plots interactively
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



block1_all <- bind_rows(summary_df, summary_df_41, summary_df_41)



# block 2 -----------------------------------------------------------------


# block 2 42 degrees ------------------------------------------------------


### 42 degrees
data_b2_42 <- read_excel("data-raw/Growth-Curves/Block 2/Block2_42C/Nick_Feb2_25_Nglab_Block2_42C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block2 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block2") %>% 
  mutate(well = str_to_lower(well))


d42_b2 <- left_join(data_b2_42, plate_layout_block2)



d42_b2 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()


gdatb2_42 <- d42_b2 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    fpath = "figures/block2/block2_42/"           # NA to display plots interactively
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
  left_join(plate_layout) %>% 
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
  select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block2 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block2") %>% 
  mutate(well = str_to_lower(well))


d41_b2 <- left_join(datab2_41, plate_layout_block2)



d41_b2 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()


gdatb2_41 <- d41_b2 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    fpath = "figures/block2/block2_41/"           # NA to display plots interactively
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
  left_join(plate_layout) %>% 
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
  select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block2 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block2") %>% 
  mutate(well = str_to_lower(well))


d35_b2 <- left_join(datab2_35, plate_layout_block2)



d35_b2 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()


gdatb2_35 <- d35_b2 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    fpath = "figures/block2/block2_35/"           # NA to display plots interactively
  ))


summary_df_b2_35 <- gdatb2_35 %>%
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
  mutate(block = 2)


### growth at 35 degrees

summary_df_b2_35 %>% 
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
  select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block3 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block3") %>% 
  mutate(well = str_to_lower(well))


d42_b3 <- left_join(data_b3_42, plate_layout_block3)



d42_b3 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()


gdatb3_42 <- d42_b3 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    fpath = "figures/block3/block3_42/"           # NA to display plots interactively
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
  left_join(plate_layout) %>% 
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
  select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block3 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block3") %>% 
  mutate(well = str_to_lower(well))


d41_b3 <- left_join(datab3_41, plate_layout_block3)



d41_b3 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()


gdatb3_41 <- d41_b3 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    fpath = "figures/block3/block3_41/"           # NA to display plots interactively
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
  left_join(plate_layout) %>% 
  mutate(test_temperature = 41) %>% 
  mutate(block = 3)


### growth at 41 degrees

summary_df_b3_41 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()

# block 2 35 degrees ------------------------------------------------------


### 35 degrees
datab3_35 <- read_excel("data-raw/Growth-Curves/Block 3/Block3_35C/Nick_Feb22_25_Nglab_Block3_35C.xlsx", range = "B32:CU129") %>% 
  clean_names() %>% 
  mutate(time = ymd_hms(time)) %>%
  mutate(start_time = min(time)) %>% 
  mutate(days = interval(start_time, time)/ddays(1)) %>% 
  select(start_time, days, everything()) %>% 
  gather(key = well, value = od, 5:ncol(.))


plate_layout_block3 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block3") %>% 
  mutate(well = str_to_lower(well))


d35_b3 <- left_join(datab3_35, plate_layout_block3)



d35_b3 %>% 
  ggplot(aes(x = days, y = od, color = strain, group = well)) + geom_line()


gdatb3_35 <- d35_b3 %>%
  mutate(ln_abundance = log(od)) %>% 
  group_by(well) %>%
  do(grs = get.growth.rate(
    x = .$days,
    y = .$ln_abundance,   # or whatever your log-transformed response is
    id = unique(.$well),
    plot.best.Q = TRUE,
    fpath = "figures/block3/block3_35/"           # NA to display plots interactively
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
  left_join(plate_layout) %>% 
  mutate(test_temperature = 35) %>% 
  mutate(block = 3)


### growth at 35 degrees

summary_df_b3_35 %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain)) %>% 
  ggplot(aes(x = strain, y = mu, color = evolution_history)) + geom_point()



all_blocks <- bind_rows(summary_df, summary_df_41, summary_df_41, summary_df_b2_35, summary_df_b2_41, summary_df_b2_42, summary_df_b3_35, summary_df_b3_41, summary_df_b3_42) %>% 
  mutate(evolution_history = case_when(grepl("35", strain) ~ "35 evolved",
                                       grepl("40", strain) ~ "40 evolved",
                                       grepl("WT", strain) ~ "WT",
                                       TRUE ~ strain))

all_blocks %>% 
  ggplot(aes(x = evolution_history, y = mu)) + geom_point()

all_sum <- all_blocks %>% 
  group_by(evolution_history, test_temperature) %>% 
  summarise(mean_growth_rate = mean(mu),
            se_growth_rate = std.error(mu))

all_sum %>% 
  ggplot(aes(x = evolution_history, y = mean_growth_rate)) + geom_point() +
  geom_errorbar(aes(x = evolution_history, ymin = mean_growth_rate - se_growth_rate, ymax = mean_growth_rate + se_growth_rate)) +
  facet_wrap( ~ test_temperature)

ggsave("figures/all-growth-rates.png", width = 8, height = 6)
