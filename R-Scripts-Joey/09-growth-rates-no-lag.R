### Estimate growth rates in common garden experiments

# Author: Joey Bernhardt
# Description: Imports OD growth curve data from the Biotek plate reader across
#              three experimental blocks and five temperatures, and estimates
#              per-well growth rates using get.growth.rate() from growthTools
#              (saturation and linear regression methods). All time points are
#              included in the fit — the lag phase is not separated out or
#              excluded. Results are combined across all blocks and temperatures.
# Input: data-raw/Growth-Curves/Block {1,2,3}/Block{N}_{T}C/*.xlsx
#        data-raw/Growth-Curves/well-plate-layout.xlsx
# Output: "data-processed/all-blocks-growth-no-lag.csv"
#         figures/od_time_{temp}C_block{N}.png (OD time series, all blocks/temps)
#         figures/block{N}/block{N}_{temp}_no_lag/ (per-well growth rate fits)
#         figures/casp-od-time-{35,25,38}.png
#         figures/{35,25,38}-casp-od.png

# Written for R version 4.2.3
# Last updated: April 01 2026


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

# Reads one Biotek Excel file, fits growth rates per well using growthTools, and returns a summary data frame of per-well
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



