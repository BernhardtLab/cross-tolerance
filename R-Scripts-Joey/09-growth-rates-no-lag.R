### Estimate growth rates in common garden experiments

# Author: Joey Bernhardt
# Input: Excel spreadsheets that are outputs from the Biotek plate reader
# Output: 
# Written for R version 4.2.3
# Last updated: July 10 2025


# load packages -----------------------------------------------------------

library(tidyverse)
library(glue)
library(readxl)
library(janitor)
library(plotrix)
library(lubridate)
library(cowplot)
theme_set(theme_cowplot())
library(growthTools)
library(growthrates)
library(dplyr)


# read plate layouts (once per block) -------------------------------------

plate_layout_b1 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block1") |>
  mutate(well = str_to_lower(well))
plate_layout_b2 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block2") |>
  mutate(well = str_to_lower(well))
plate_layout_b3 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block3") |>
  mutate(well = str_to_lower(well))


# growth rate fitting function --------------------------------------------

# Reads one Biotek Excel file, fits growth rates per well using multistart
# nonlinear least squares, and returns a summary data frame of per-well
# growth rate estimates. Also saves an OD time series plot and per-well
# growth rate plots to figures/.
# Arguments:
#   file_path    - path to the Excel file
#   plate_layout - data frame mapping wells to strain names for this block
#   temperature  - numeric test temperature (used for tagging and figure names)
#   block        - numeric block number (used for tagging and figure names)
#   filter_blank - if TRUE, blank wells are excluded before growth rate fitting
#                  (needed where flat blank curves cause model fitting to fail)
process_block_temp <- function(file_path, plate_layout, temperature, block,
                               filter_blank = FALSE) {
  od_fig_path   <- glue("figures/od_time_{temperature}C_block{block}.png")
  grow_fig_path <- glue("figures/block{block}/block{block}_{temperature}_no_lag/")
  dir.create(grow_fig_path, recursive = TRUE, showWarnings = FALSE)

  raw <- read_excel(file_path, range = "B32:CU129") |>
    clean_names() |>
    mutate(time       = ymd_hms(time),
           start_time = min(time),
           days       = interval(start_time, time) / ddays(1)) |>
    dplyr::select(start_time, days, everything()) |>
    pivot_longer(cols = 5:last_col(), names_to = "well", values_to = "od")

  d <- left_join(raw, plate_layout)

  print(
    ggplot(d, aes(x = days, y = od, color = strain, group = well)) + geom_line()
  )
  ggsave(od_fig_path, width = 8, height = 6)

  d_fit <- if (filter_blank) filter(d, strain != "blank") else d

  gdat <- d_fit |>
    mutate(ln_abundance = log(od)) |>
    group_by(well) |>
    do(grs = get.growth.rate(
      x           = .$days,
      y           = .$ln_abundance,
      id          = unique(.$well),
      plot.best.Q = TRUE,
      methods     = c("sat", "flr"),
      fpath       = grow_fig_path
    ))

  gdat |>
    summarise(
      well,
      mu         = grs$best.slope,
      best_model = grs$best.model,
      se         = grs$best.se,
      R2         = grs$best.model.rsqr,
      n_obs      = grs$best.model.slope.n
    ) |>
    left_join(plate_layout) |>
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


# fit growth rates for all blocks and temperatures ------------------------

all_blocks_raw <- pmap(manifest, process_block_temp)


# combine, filter controls, and tag evolution history ---------------------

all_blocks <- bind_rows(all_blocks_raw) |>
  filter(!grepl("LIG|blank|YPD|588|FLZ_3", strain)) |> 
  mutate(evolution_history = case_when(
    grepl("WT_FLZ",  strain) ~ "Fluconazole evolved",
    grepl("WT_CASP", strain) ~ "Caspofungin evolved",
    grepl("35",      strain) ~ "35 evolved",
    grepl("40",      strain) ~ "40 evolved",
    TRUE                     ~ strain
  ))






# save output -------------------------------------------------------------

write_csv(all_blocks, "data-processed/all-blocks-growth-no-lag.csv")


all_blocks_no_lag <- read_csv("data-processed/all-blocks-growth-no-lag.csv")




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

  
  ggplot() + 
  geom_jitter(aes( x= test_temperature, y = mu, color = evolution_history), data = ab2, alpha = 0.6) + 
  geom_point() +
  # geom_smooth(method = "lm", aes(color = evolution_history)) +
    
    geom_pointrange(aes(x = test_temperature, y = mean_growth_rate2, ymin = mean_growth_rate2 - se_growth_rate, ymax = mean_growth_rate2 + se_growth_rate, color = evolution_history), position = position_dodge2(width = 2), data = filter(all_sum3b,!evolution_history %in% c("Caspofungin evolved", "Fluconazole evolved")), size = 1) +
    geom_pointrange(aes(x = test_temperature, y = mean_growth_rate2, ymin = mean_growth_rate2 - se_growth_rate, ymax = mean_growth_rate2 + se_growth_rate), shape = 21, color = "black", position = position_dodge2(width = 2), data = filter(all_sum3b,!evolution_history %in% c("Caspofungin evolved", "Fluconazole evolved")), size = 1) +
    ylab("Growth rate (per day)") +
    xlab("Temperature (C)")
ggsave("figures/growth-rates-temp.png", width = 8, height = 6)



