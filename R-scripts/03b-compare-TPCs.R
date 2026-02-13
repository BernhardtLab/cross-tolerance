

#### compare the Thomas and the SS model

# load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(lubridate)
library(growthTools)
library(rTPC)
library(minpack.lm)
library(car) ### for bootstrapping
library(plotrix) ### for SE calculations
library(broom)
library(tidyverse)
library(conflicted)
conflict_prefer("select", "dplyr")
conflicts_prefer(dplyr::filter)
library(nls.multstart)
library(rTPC)


# read in data ------------------------------------------------------------



all_blocks_no_lag <- read_csv("data-processed/all-blocks-growth-no-lag.csv")

d <- all_blocks %>% 
  mutate(curve_id = strain) %>% 
  mutate(temp = test_temperature, 
         rate = mu)

length(unique(d$curve_id))




# fit SS model ------------------------------------------------------------

df <- d  


fit_sharp_high <- function(df, 
                           model_name = "sharpeschoolhigh_1981", 
                           tref = 20, 
                           iter = 2000) {
  
  # get start values and bounds
  starts <- get_start_vals(df$temp, df$rate, model_name = model_name)
  lower  <- get_lower_lims(df$temp, df$rate, model_name = model_name)
  upper  <- get_upper_lims(df$temp, df$rate, model_name = model_name)
  
  # construct formula with tref fixed
  param_names <- names(starts)
  rhs <- paste0(
    model_name, "(temp, ",
    paste(param_names, collapse = ", "),
    ", tref = ", tref, ")"
  )
  formula <- as.formula(paste("rate ~", rhs))
  
  # fit with error handling
  out <- tryCatch(
    {
      fit <- nls_multstart(
        formula = formula,
        data = df,
        iter = iter,
        start_lower = starts * 0.5,
        start_upper = starts * 1.5,
        lower = lower,
        upper = upper,
        supp_errors = "N"
      )
      
      # add AIC to return object
      list(
        fit  = fit,
        AIC  = AIC(fit),
        error = FALSE
      )
    },
    error = function(e) {
      list(
        fit = NULL,
        AIC = NA_real_,
        error = TRUE,
        message = e$message
      )
    }
  )
  
  return(out)
}



fits <- df %>%
  group_by(curve_id) %>%
  nest() %>%
  mutate(fit = map(data, ~ fit_sharp_high(.x, tref = 20, iter = 2000))) %>%
  mutate(
    error = map_lgl(fit, "error"),
    AIC   = map_dbl(fit, ~ .x$AIC)
  )

fit_status <- fits %>%
  mutate(ok = map_lgl(fit, ~ !inherits(.x, "tpc_fit_error") && !is.null(.x))) %>%
  select(curve_id, ok)

# fit both SS and thomas --------------------------------------------------

fit_tpc_model <- function(df, model_name, tref = 20, iter = 2000) {
  
  starts <- get_start_vals(df$temp, df$rate, model_name = model_name)
  lower  <- get_lower_lims(df$temp, df$rate, model_name = model_name)
  upper  <- get_upper_lims(df$temp, df$rate, model_name = model_name)
  
  param_names <- names(starts)
  
  # --- KEY FIX: include tref only if the model has it ---
  model_fun <- get(model_name)  # get function object from rTPC namespace
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
      paste(param_names, collapse = ", "), ")"
    )
  }
  
  formula <- as.formula(paste("rate ~", rhs))
  
  # ---- Fit & return AIC ----
  tryCatch(
    {
      fit <- nls_multstart(
        formula = formula,
        data = df,
        iter = iter,
        start_lower = starts * 0.5,
        start_upper = starts * 1.5,
        lower = lower,
        upper = upper,
        supp_errors = "N"
      )
      list(fit = fit, AIC = AIC(fit), error = FALSE)
    },
    error = function(e) {
      list(fit = NULL, AIC = NA_real_, error = TRUE, message = e$message)
    }
  )
}



fits_both <- df %>%
  group_by(curve_id) %>%
  nest() %>%
  mutate(
    sharp_high = map(data, ~ fit_tpc_model(.x, "sharpeschoolhigh_1981", tref = 20)),
    thomas     = map(data, ~ fit_tpc_model(.x, "thomas_2012"))
  ) %>%
  mutate(
    AIC_sharp  = map_dbl(sharp_high, "AIC"),
    AIC_thomas = map_dbl(thomas, "AIC")
  )


fits_both2 <- fits_both %>%
  mutate(
    best_model = case_when(
      AIC_sharp < AIC_thomas ~ "sharpeschoolhigh_1981",
      AIC_thomas < AIC_sharp ~ "thomas_2012",
      TRUE ~ "tie"
    )
  )





### ok, in all cases the SS model is the best model via AIC


# now add briere and lactin -----------------------------------------------
fit_tpc_model <- function(df, model_name, tref = 20, iter = 2000) {
  
  starts <- get_start_vals(df$temp, df$rate, model_name = model_name)
  lower  <- get_lower_lims(df$temp, df$rate, model_name = model_name)
  upper  <- get_upper_lims(df$temp, df$rate, model_name = model_name)
  
  # ---- Guard: model cannot be fit if rTPC produced NA values ----
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
      data = df,
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



fits_all <- df %>%
  group_by(curve_id) %>%
  nest() %>%
  mutate(
    sharpeschool = map(data, ~ fit_tpc_model(.x, "sharpeschoolhigh_1981", tref = 20)),
    thomas       = map(data, ~ fit_tpc_model(.x, "thomas_2012")),
    briere       = map(data, ~ fit_tpc_model(.x, "briere2_1999")),
    lactin       = map(data, ~ fit_tpc_model(.x, "lactin2_1995"))
  ) %>%
  mutate(
    AIC_sharp   = map_dbl(sharpeschool, "AIC"),
    AIC_thomas  = map_dbl(thomas,       "AIC"),
    AIC_briere  = map_dbl(briere,       "AIC"),
    AIC_lactin  = map_dbl(lactin,       "AIC")
  )


fits_all2 <- fits_all %>%
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



