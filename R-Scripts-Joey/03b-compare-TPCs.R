#### Comparing TPC model fits to growth rates across temperatures

# Author: Joey Bernhardt
# Description: Fits four TPC models (Sharpe-Schoolfield, Thomas, Briere2, Lactin2) to
#              per-population growth rate data and compares them by AIC to select the
#              best-fitting model. The sharpeschoolhigh model was the best fit for all
#              but 6 populations and is used in downstream analyses.
# Input: "data-processed/all-blocks-growth-no-lag.csv" (growth rate data without lag phase)
# Output: "data-processed/tpc-model-fits-aic-comparison.csv"
# Requires: 09-growth-rates-no-lag.R (produces input file)
# Written for R version 4.2.3
# Last updated: April 01 2026

# load packages -----------------------------------------------------------

library(tidyverse)
library(rTPC)
library(minpack.lm)
library(conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
library(nls.multstart)


# read in growth rate data ------------------------------------------------

df <- read_csv("data-processed/all-blocks-growth-no-lag.csv") |>
  mutate(curve_id = strain, temp = test_temperature, rate = mu)


# fit TPC models ----------------------------------------------------------

# The function fit_tpc_model fits a named rTPC model to a single curve's temperature-rate data using
# nonlinear least squares. Automatically retrieves start values and
# parameter bounds from rTPC, and handles models with or without a tref argument.
# Returns a list with the fitted model object, its AIC, and an error flag.
# If fitting fails or start values contain NA, returns a list with fit = NULL
# and error = TRUE.

fit_tpc_model <- function(curve_data, model_name, tref = 20, iter = 2000) {

  starts <- get_start_vals(curve_data$temp, curve_data$rate, model_name = model_name)
  lower  <- get_lower_lims(curve_data$temp, curve_data$rate, model_name = model_name)
  upper  <- get_upper_lims(curve_data$temp, curve_data$rate, model_name = model_name)

  # Guard: model cannot be fit if rTPC produced NA values
  if (any(is.na(starts)) || any(is.na(lower)) || any(is.na(upper))) {
    return(list(fit = NULL, AIC = NA_real_, error = TRUE,
                message = "Start/bound values contained NA"))
  }

  param_names <- names(starts)

  model_fun  <- get(model_name)
  model_args <- names(formals(model_fun))

  if ("tref" %in% model_args) {
    rhs <- paste0(
      model_name, "(temp, ",
      paste(param_names, collapse = ", "),
      ", tref = ", tref, ")"
    )
  } else {
    rhs <- paste0(
      model_name, "(temp, ",
      paste(param_names, collapse = ", "),
      ")"
    )
  }

  formula <- as.formula(paste("rate ~", rhs))

  tryCatch({
    fit <- nls_multstart(
      formula = formula,
      data = curve_data,
      iter = iter,
      start_lower = starts * 0.5,
      start_upper = starts * 1.5,
      lower = lower,
      upper = upper,
      supp_errors = "N"
    )
    list(fit = fit, AIC = AIC(fit), error = FALSE)
  }, error = function(e) {
    list(fit = NULL, AIC = NA_real_, error = TRUE,
         message = e$message)
  })
}


# fit all four models and compare AIC -------------------------------------
# SS was best in a preliminary SS-vs-Thomas comparison; briere and lactin
# added here to confirm.

fits_all <- df |>
  group_by(curve_id) |>
  nest() |>
  mutate(
    sharpeschool = map(data, ~ fit_tpc_model(.x, "sharpeschoolhigh_1981")),
    thomas       = map(data, ~ fit_tpc_model(.x, "thomas_2012")),
    briere       = map(data, ~ fit_tpc_model(.x, "briere2_1999")),
    lactin       = map(data, ~ fit_tpc_model(.x, "lactin2_1995"))
  ) |>
  mutate(
    AIC_sharp   = map_dbl(sharpeschool, "AIC"),
    AIC_thomas  = map_dbl(thomas,       "AIC"),
    AIC_briere  = map_dbl(briere,       "AIC"),
    AIC_lactin  = map_dbl(lactin,       "AIC")
  )


fits_aic <- fits_all |>
  mutate(
    best_model = pmap_chr(
      list(AIC_sharp, AIC_thomas, AIC_briere, AIC_lactin),
      ~ {
        AICs <- c(
          sharpeschoolhigh_1981 = ..1,
          thomas_2012           = ..2,
          briere2_1999          = ..3,
          lactin2_1995          = ..4
        )
        if (all(is.na(AICs))) return("no fit")
        names(which.min(AICs))
      }
    )
  )

# For all but 6 populations, the sharpeschoolhigh model is the best fit,
# so we will go with that

fits_aic |>
  select(curve_id, starts_with("AIC_"), best_model) |>
  write_csv("data-processed/tpc-model-fits-aic-comparison.csv")
