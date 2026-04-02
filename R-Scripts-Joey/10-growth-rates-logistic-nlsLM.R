### Estimate growth rates using logistic growth model with nlsLM

# Author: Joey Bernhardt
# Description: Imports OD growth curve data from the Biotek plate reader across
#              three experimental blocks and five temperatures, and estimates
#              per-well growth rates (r) and carrying capacity (K) using a logistic
#              growth model fit with nls multstart. All time points are included in the fit —
#              the lag phase is not separated out or excluded.
#              Results are combined across all blocks and temperatures.
# Input: data-raw/Growth-Curves/Block {1,2,3}/Block{N}_{T}C/*.xlsx
#        data-raw/Growth-Curves/well-plate-layout.xlsx
# Output: "data-processed/all-blocks-growth-logistic.csv"
#         figures/od_time_{temp}C_block{N}.png (OD time series, all blocks/temps)
#         figures/block{N}/block{N}_{temp}_logistic/ (per-well growth model fits)

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
library(nls.multstart)
theme_set(theme_cowplot())


# read plate layouts (once per block) -------------------------------------

plate_layout_b1 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block1") |>
  mutate(well = str_to_lower(well))
plate_layout_b2 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block2") |>
  mutate(well = str_to_lower(well))
plate_layout_b3 <- read_excel("data-raw/Growth-Curves/well-plate-layout.xlsx", sheet = "block3") |>
  mutate(well = str_to_lower(well))


# logistic growth model fitting function ----------------------------------

# 3-parameter logistic growth model: K / (1 + exp(-r * (t - t_mid)))
# where K is carrying capacity, r is growth rate,
# and t_mid is the inflection point (time at K/2)
# Uses nls_multstart for robustness with multiple starting points

fit_logistic_growth <- function(time_data, od_data) {
  # Remove NAs
  valid_idx <- !is.na(od_data) & !is.na(time_data)
  time_clean <- time_data[valid_idx]
  od_clean <- od_data[valid_idx]

  # Handle edge cases
  if (length(od_clean) < 3 || sd(od_clean) == 0) {
    return(NULL)
  }

  # Create data frame for fitting
  fit_data <- data.frame(time = time_clean, od = od_clean)

  tryCatch(
    {
      fit <- nls_multstart(
        od ~ K / (1 + exp(-r * (time - t_mid))),
        data = fit_data,
        iter = 500,
        start_lower = c(K = max(od_clean) * 1.02, r = 0.05, t_mid = min(time_clean) + 0.1),
        start_upper = c(K = max(od_clean) * 1.3, r = 2, t_mid = max(time_clean) - 0.1),
        lower = c(K = max(od_clean) * 1.0, r = 0.001, t_mid = min(time_clean) - 2),
        upper = c(K = max(od_clean) * 2, r = 50, t_mid = max(time_clean) + 2),
        supp_errors = 'N',
        na.action = na.omit,
        control = nls.control(maxiter = 1000, minFactor = 1/204800000)
      )
      return(fit)
    },
    error = function(e) {
      return(NULL)
    }
  )
}

# Reads one Biotek Excel file, fits logistic growth models per well, and returns
# a summary data frame of per-well estimates. Also saves an OD time series plot
# and per-well growth model plots to figures/.
# Arguments:
#   file_path    - path to the Excel file
#   plate_layout - data frame mapping wells to strain names for this block
#   temperature  - numeric test temperature (used for tagging and figure names)
#   block        - numeric block number (used for tagging and figure names)
#   filter_blank - if TRUE, blank wells are excluded before growth rate fitting
process_block_temp_logistic <- function(file_path, plate_layout, temperature, block,
                                        filter_blank = FALSE) {
  od_fig_path   <- glue("figures/od_time_{temperature}C_block{block}.png")
  grow_fig_path <- glue("figures/block{block}/block{block}_{temperature}_logistic/")
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

  # Fit logistic growth model for each well
  results_list <- list()

  for (well_id in unique(d_fit$well)) {
    well_data <- filter(d_fit, well == well_id)
    strain_name <- unique(well_data$strain)

    fit <- fit_logistic_growth(
      time_data = well_data$days,
      od_data = well_data$od
    )

    if (!is.null(fit)) {
      # Extract parameter estimates
      coefs <- coef(fit)
      r_est <- as.numeric(coefs["r"])
      K_est <- as.numeric(coefs["K"])
      t_mid_est <- as.numeric(coefs["t_mid"])

      # Calculate fit quality
      residuals_fit <- residuals(fit)
      ss_res <- sum(residuals_fit^2)
      ss_tot <- sum((well_data$od - mean(well_data$od))^2)
      r_squared <- 1 - (ss_res / ss_tot)

      # Create fitted line for plotting
      time_seq <- seq(min(well_data$days), max(well_data$days), length.out = 100)
      fitted_vals <- K_est / (1 + exp(-r_est * (time_seq - t_mid_est)))
      fitted_df <- data.frame(time_seq = time_seq, fitted_vals = fitted_vals)

      # Create and save plot
      p <- ggplot(well_data, aes(x = days, y = od)) +
        geom_point() +
        geom_line(aes(x = time_seq, y = fitted_vals), data = fitted_df, color = "blue") +
        labs(title = glue("Well {well_id} ({strain_name})"),
             subtitle = glue("r = {round(r_est, 4)}, K = {round(K_est, 4)}, R² = {round(r_squared, 4)}"),
             x = "Time (days)",
             y = "OD") +
        theme_cowplot()

      ggsave(
        glue("{grow_fig_path}{well_id}.png"),
        plot = p,
        width = 5,
        height = 4
      )

      # Store results
      results_list[[well_id]] <- data.frame(
        well = well_id,
        strain = strain_name,
        r = r_est,
        K = K_est,
        t_mid = t_mid_est,
        R2 = r_squared,
        n_obs = nrow(well_data)
      )
    } else {
      # If fit failed, record NA
      results_list[[well_id]] <- data.frame(
        well = well_id,
        strain = unique(well_data$strain),
        r = NA,
        K = NA,
        t_mid = NA,
        R2 = NA,
        n_obs = nrow(well_data)
      )
    }
  }

  bind_rows(results_list) |>
    left_join(plate_layout, by = "well") |>
    dplyr::select(well, strain = strain.x, r, K, t_mid, R2, n_obs, everything()) |>
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

all_blocks_raw <- pmap(manifest, process_block_temp_logistic)


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


# save output -------------------------------------------------------------

write_csv(all_blocks, "data-processed/all-blocks-growth-logistic.csv")

