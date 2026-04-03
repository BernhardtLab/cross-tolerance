

#### ok here I want to try to fit the curves with the sliding window approach which gets the max slope


# load packages -----------------------------------------------------------

library(tidyverse)
library(glue)
library(readxl)
library(janitor)
library(plotrix)
library(lubridate)
library(cowplot)
library(nls.multstart)
theme_set(theme_cowplot())




# read plate layouts (once per block) -------------------------------------

plate_layout_b1 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block1") |>
  mutate(well = str_to_lower(well))
plate_layout_b2 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block2") |>
  mutate(well = str_to_lower(well))
plate_layout_b3 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block3") |>
  mutate(well = str_to_lower(well))




read_in_ods <- function(file_path, plate_layout, temperature, block,
                                        filter_blank = FALSE) {
  
  raw <- read_excel(file_path, range = "B32:CU129") |>
    clean_names() |>
    mutate(time       = ymd_hms(time),
           start_time = min(time),
           days       = interval(start_time, time) / ddays(1)) |>
    dplyr::select(start_time, days, everything()) |>
    pivot_longer(cols = 5:last_col(), names_to = "well", values_to = "od")
  
  d <- left_join(raw, plate_layout) |> 
    mutate(test_temperature = temperature, block = block)
  
}


# file manifest -----------------------------------------------------------

manifest <- tribble(
  ~file_path,                                                                         ~plate_layout,   ~temperature, ~block, ~filter_blank,
  "data-raw/Growth-Curves/Block 1/Block1_42C/Nick_Feb1_25_Nglab_Block1_42C.xlsx",   plate_layout_b1, 42,           1,      FALSE,
  "data-raw/Growth-Curves/Block 1/Block1_41C/Nick_Feb7_25_Nglab_Block1_41C.xlsx",   plate_layout_b1, 41,           1,      FALSE,
  "data-raw/Growth-Curves/Block 1/Block1_35C/Nick_Feb13_25_Nglab_Block1_35C.xlsx",  plate_layout_b1, 35,           1,      FALSE,
  "data-raw/Growth-Curves/Block 1/Block1_25C/Nick_July19_25_Nglab_Block1_25C.xlsx", plate_layout_b1, 25,           1,      FALSE,
  "data-raw/Growth-Curves/Block 1/Block1_38C/Nick_Aug22_25_Nglab_Block1_38C.xlsx",  plate_layout_b1, 38,           1,      FALSE,
  "data-raw/Growth-Curves/Block 2/Block2_42C/Nick_Feb2_25_Nglab_Block2_42C.xlsx",   plate_layout_b2, 42,           2,      FALSE,
  "data-raw/Growth-Curves/Block 2/Block2_41C/Nick_Feb11_25_Nglab_Block2_41C.xlsx",  plate_layout_b2, 41,           2,      FALSE,
  "data-raw/Growth-Curves/Block 2/Block2_35C/Nick_Feb14_25_Nglab_Block2_35C.xlsx",  plate_layout_b2, 35,           2,      FALSE,
  "data-raw/Growth-Curves/Block 2/Block2_25C/Nick_July20_25_Nglab_Block2_25C.xlsx", plate_layout_b2, 25,           2,      FALSE,
  "data-raw/Growth-Curves/Block 2/Block2_38C/Nick_Aug23_25_Nglab_Block2_38C.xlsx",  plate_layout_b2, 38,           2,      TRUE,
  "data-raw/Growth-Curves/Block 3/Block3_42C/Nick_Feb3_25_Nglab_Block3_42C.xlsx",   plate_layout_b3, 42,           3,      TRUE,
  "data-raw/Growth-Curves/Block 3/Block3_41C/Nick_Feb12_25_Nglab_Block3_41C.xlsx",  plate_layout_b3, 41,           3,      FALSE,
  "data-raw/Growth-Curves/Block 3/Block3_35C/Nick_Feb22_25_Nglab_Block3_35C.xlsx",  plate_layout_b3, 35,           3,      FALSE,
  "data-raw/Growth-Curves/Block 3/Block3_25C/Nick_July24_25_Nglab_Block3_25C.xlsx", plate_layout_b3, 25,           3,      FALSE,
  "data-raw/Growth-Curves/Block 3/Block3_38C/Nick_Aug24_25_Nglab_Block3_38C.xlsx",  plate_layout_b3, 38,           3,      FALSE
)


# fit growth models for all blocks and temperatures -------------------------

all_blocks_raw <- pmap(manifest, read_in_ods)


# combine, filter controls, and tag evolution history -----------------------

all_blocks <- bind_rows(all_blocks_raw) |>
  filter(!grepl("LIG|blank|YPD|588|FLZ_3", strain)) |>
  mutate(evolution_history = case_when(
    grepl("WT_FLZ",  strain) ~ "Fluconazole evolved",
    grepl("WT_CASP", strain) ~ "Caspofungin evolved",
    grepl("35",      strain) ~ "35 evolved",
    grepl("40",      strain) ~ "40 evolved",
    TRUE                     ~ strain
  ))


### plot

all_blocks |> 
  filter(evolution_history %in% c("35 evolved", "40 evolved", "fRS585")) |> 
  mutate(unique_well = paste(test_temperature, block, well, sep = "_")) |>
  ggplot(aes(x = days, y = od, color = factor(evolution_history), group = unique_well)) + geom_line() +
  facet_wrap( ~ test_temperature)
