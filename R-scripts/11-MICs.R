

### MICs
library(tidyverse)
library(readxl)
library(drc)       # for dose–response models
library(pracma)    # for AUC via trapezoid


thing1 <- read_excel("data-raw/MICs/FINAL/Mar6.25(35C-ev_ALL3+40C-ev_Res)/24h.xlsx", sheet = "Set3_Rep3_AMPB", range = "A24:M32") %>% 
  rename("population" = "<>")


# Path to the Excel file
file_path <- "data-raw/MICs/FINAL/Mar6.25(35C-ev_ALL3+40C-ev_Res)/24h.xlsx"

# Get all sheet names
sheet_names <- excel_sheets(file_path)

# Read and combine all sheets, adding a column with the sheet name
all_data <- map_dfr(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet,range = "A24:M32") %>%
    mutate(sheet_name = sheet)
})


casp <- all_data %>% 
  filter(grepl("CASP", sheet_name)) %>%
  rename("population" = "<>") %>% 
  dplyr::select(population, sheet_name, everything()) %>% 
  gather(key = concentration, value = OD, 3:ncol(all_data)) %>%
  filter(!is.na(OD)) %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  mutate(concentration = ifelse(concentration == 0, 0.001, concentration))


casp %>% 
  ggplot(aes(x = concentration, y = OD, color = population)) + geom_point() +
  scale_x_log10()
  


library(drc)       # for dose–response models
library(pracma)    # for AUC via trapezoid

ggplot(casp, aes(x = concentration, y = OD, color = population)) +
  geom_point() +
  geom_line() +
  scale_x_log10() +
  theme_minimal()

# Split and fit model
models <- casp %>%
  group_by(population) %>%
  group_split() %>%
  setNames(unique(casp$population)) %>%
  lapply(function(sub_df) {
    tryCatch(
      drm(OD ~ concentration, data = sub_df, fct = LL.4()),
      error = function(e) NULL
    )
  })

ic50_vals <- sapply(models, function(m) {
  if (is.null(m)) return(NA)
  ED(m, 50, interval = "none")[1]  # 50% effective dose
})

ic50_df <- data.frame(population = names(ic50_vals), IC50 = ic50_vals)


auc_df <- casp %>%
  arrange(population, concentration) %>%
  group_by(population) %>%
  summarise(AUC = trapz(concentration, OD))

mic_vals <- casp %>%
  group_by(population) %>%
  summarise(MIC = min(concentration[OD < 0.1], na.rm = TRUE))  # adjust threshold as needed

resistance_metrics <- ic50_df %>%
  left_join(auc_df, by = "population") %>%
  left_join(mic_vals, by = "population") %>% 
  filter(population != "Blank")

kruskal.test(IC50 ~ population, data = resistance_metrics)

resistance_metrics %>% 
  filter(IC50 < .5) %>%  ### just getting rid of one outlier for now
  ggplot(aes(x = population, y = IC50)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Create prediction data frame for each model
library(purrr)

# For each model, generate predictions over a smooth grid of concentrations
predicted_df <- imap_dfr(models, function(mod, pop_id) {
  if (is.null(mod)) return(NULL)
  
  # Create new data for prediction
  conc_seq <- exp(seq(log(min(casp$concentration)), log(max(casp$concentration)), length.out = 100))
  
  pred_vals <- predict(mod, newdata = data.frame(concentration = conc_seq))
  
  tibble(
    population = pop_id,
    concentration = conc_seq,
    OD_pred = pred_vals
  )
})

ggplot() +
  geom_point(data = casp, aes(x = concentration, y = OD, color = population), alpha = 0.5) +
  geom_line(data = predicted_df, aes(x = concentration, y = OD_pred, color = population), size = 1) +
  scale_x_log10() +
  labs(title = "Fitted dose–response curves by population",
       x = "Drug concentration",
       y = "OD (24h)",
       color = "Population") +
  theme_minimal()
ggsave("figures/casp-drc.png", width = 8, height = 6)


# fluconazole -------------------------------------------------------------

fluc <- all_data %>% 
  filter(grepl("FLZ", sheet_name)) %>%
  rename("population" = "<>") %>% 
  dplyr::select(population, sheet_name, everything()) %>% 
  gather(key = concentration, value = OD, 3:ncol(all_data)) %>%
  filter(!is.na(OD)) %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  mutate(concentration = ifelse(concentration == 0, 0.1, concentration))


fluc %>% 
  ggplot(aes(x = concentration, y = OD, color = population)) + geom_point() +
  scale_x_log10()



library(drc)       # for dose–response models
library(pracma)    # for AUC via trapezoid

ggplot(fluc, aes(x = concentration, y = OD, color = population)) +
  geom_point() +
  geom_line() +
  scale_x_log10() +
  theme_minimal()

fit_ic50_models <- function(data) {
  # Ensure concentration is numeric
  data$concentration <- as.numeric(as.character(data$concentration))
  
  # Get unique populations
  populations <- unique(data$population)
  
  # Initialize storage
  results <- data.frame(population = character(),
                        IC50 = numeric(),
                        stringsAsFactors = FALSE)
  
  # Optional: also store models
  models <- list()
  
  # Loop over populations
  for (pop in populations) {
    sub_df <- subset(data, population == pop)
    
    # Fit model
    fit <- tryCatch(
      drm(OD ~ concentration, data = sub_df, fct = LL.4()),
      error = function(e) NULL
    )
    
    # Extract IC50 if model is valid
    ic50 <- NA
    if (!is.null(fit)) {
      ic50 <- tryCatch(ED(fit, 50, interval = "none")[1], error = function(e) NA)
    }
    
    # Store results
    results <- rbind(results, data.frame(population = pop, IC50 = ic50))
    
    # Optional: store model
    models[[pop]] <- fit
  }
  
  # Optionally return models too
  # return(list(summary = results, models = models))
  
  return(results)
}

ic50_df_fluc <- fit_ic50_models(fluc)


auc_df_fluc <- fluc %>%
  arrange(population, concentration) %>%
  group_by(population, sheet_name) %>%
  summarise(AUC = trapz(concentration, OD))


auc_df_fluc %>% 
  ggplot(aes(x = population, y = AUC)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("figures/fluc-auc.png", width = 10, height = 6)

mic_vals_fluc <- fluc %>%
  group_by(population) %>%
  summarise(MIC = min(concentration[OD < 0.1], na.rm = TRUE))  # adjust threshold as needed

resistance_metrics_fluc <- ic50_df_fluc$summary %>%
  left_join(auc_df_fluc, by = "population") %>%
  left_join(mic_vals_fluc, by = "population") %>% 
  filter(population != "Blank")


resistance_metrics_fluc %>% 
  # filter(IC50 < .5) %>%  ### just getting rid of one outlier for now
  ggplot(aes(x = population, y = AUC)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Create prediction data frame for each model
library(purrr)

# For each model, generate predictions over a smooth grid of concentrations
predicted_df <- imap_dfr(models, function(mod, pop_id) {
  if (is.null(mod)) return(NULL)
  
  # Create new data for prediction
  conc_seq <- exp(seq(log(min(fluc$concentration)), log(max(fluc$concentration)), length.out = 100))
  
  pred_vals <- predict(mod, newdata = data.frame(concentration = conc_seq))
  
  tibble(
    population = pop_id,
    concentration = conc_seq,
    OD_pred = pred_vals
  )
})

ggplot() +
  geom_point(data = fluc, aes(x = concentration, y = OD, color = population), alpha = 0.5) +
  geom_line(data = predicted_df, aes(x = concentration, y = OD_pred, color = population), size = 1) +
  scale_x_log10() +
  labs(title = "Fitted dose–response curves by population",
       x = "Drug concentration",
       y = "OD (24h)",
       color = "Population") +
  theme_minimal()
ggsave("figures/fluc-drc.png", width = 8, height = 6)


# amphotericine B -------------------------------------------------------------

amph <- all_data %>% 
  filter(grepl("AMPB", sheet_name)) %>%
  rename("population" = "<>") %>% 
  # filter(population != "35_C7") %>% 
  # filter(population != "35_C5") %>% 
  # filter(population != "35_B8") %>% 
  # filter(population != "35_B6") %>% 
  # filter(population != "35_B2") %>% 
  filter(population != "Blank") %>% 
  dplyr::select(population, sheet_name, everything()) %>% 
  gather(key = concentration, value = OD, 3:ncol(all_data)) %>%
  filter(!is.na(OD)) %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  mutate(concentration = ifelse(concentration == 0, 0.1, concentration))


amph %>% 
  ggplot(aes(x = concentration, y = OD, color = population)) + geom_point() +
  scale_x_log10()





ggplot(amph, aes(x = concentration, y = OD, color = population)) +
  geom_point() +
  geom_line() +
  scale_x_log10() +
  theme_minimal()



# try again to get ic 50 --------------------------------------------------
library(drc)

fit_ic50_models <- function(data) {
  # Ensure concentration is numeric
  data$concentration <- as.numeric(as.character(data$concentration))
  
  # Get unique populations
  populations <- unique(data$population)
  
  # Initialize storage
  results <- data.frame(population = character(),
                        IC50 = numeric(),
                        stringsAsFactors = FALSE)
  
  # Optional: also store models
  models <- list()
  
  # Loop over populations
  for (pop in populations) {
    sub_df <- subset(data, population == pop)
    
    # Fit model
    fit <- tryCatch(
      drm(OD ~ concentration, data = sub_df, fct = LL.4()),
      error = function(e) NULL
    )
    
    # Extract IC50 if model is valid
    ic50 <- NA
    if (!is.null(fit)) {
      ic50 <- tryCatch(ED(fit, 50, interval = "none")[1], error = function(e) NA)
    }
    
    # Store results
    results <- rbind(results, data.frame(population = pop, IC50 = ic50))
    
    # Optional: store model
    models[[pop]] <- fit
  }
  
  # Optionally return models too
  # return(list(summary = results, models = models))
  
  return(results)
}

ic50_df <- fit_ic50_models(amph)




auc_df <- amph %>%
  arrange(population, concentration) %>%
  group_by(population) %>%
  summarise(AUC = trapz(concentration, OD))

mic_vals <- amph %>%
  group_by(population) %>%
  summarise(MIC = min(concentration[OD < 0.1], na.rm = TRUE))  # adjust threshold as needed

resistance_metrics <- ic50_df %>%
  left_join(auc_df, by = "population") %>%
  left_join(mic_vals, by = "population") %>% 
  filter(population != "Blank")


resistance_metrics %>% 
  # filter(IC50 < .5) %>%  ### just getting rid of one outlier for now
  ggplot(aes(x = population, y = AUC)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("figures/auc-amph.png", width = 8, height = 6)

resistance_metrics %>% 
  # filter(IC50 < .5) %>%  ### just getting rid of one outlier for now
  ggplot(aes(x = population, y = IC50)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("figures/ic50-amph.png", width = 8, height = 6)

resistance_metrics %>% 
  # filter(IC50 < .5) %>%  ### just getting rid of one outlier for now
  ggplot(aes(x = population, y = MIC)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("figures/mic-amph.png", width = 8, height = 6)



# Create prediction data frame for each model
library(purrr)

# For each model, generate predictions over a smooth grid of concentrations
predicted_df <- imap_dfr(models, function(mod, pop_id) {
  if (is.null(mod)) return(NULL)
  
  # Create new data for prediction
  conc_seq <- exp(seq(log(min(amph$concentration)), log(max(amph$concentration)), length.out = 100))
  
  pred_vals <- predict(mod, newdata = data.frame(concentration = conc_seq))
  
  tibble(
    population = pop_id,
    concentration = conc_seq,
    OD_pred = pred_vals
  )
})

ggplot() +
  geom_point(data = amph, aes(x = concentration, y = OD, color = population), alpha = 0.5) +
  geom_line(data = predicted_df, aes(x = concentration, y = OD_pred, color = population), size = 1) +
  scale_x_log10() +
  labs(title = "Fitted dose–response curves by population",
       x = "Drug concentration",
       y = "OD (24h)",
       color = "Population") +
  theme_minimal()
ggsave("figures/amph-drc.png", width = 8, height = 6)



# read in data from time point 2 ------------------------------------------

file_path <- "data-raw/MICs/FINAL/Feb26.25(40C-ev_FLZ)/24h.xlsx"

# Get all sheet names
sheet_names <- excel_sheets(file_path)

# Read and combine all sheets, adding a column with the sheet name
all_data_fluc <- map_dfr(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet,range = "A24:M32") %>%
    mutate(sheet_name = sheet)
})


fluc_raw <- all_data_fluc %>% 
  # filter(grepl("FLZ", sheet_name)) %>%
  rename("population" = "<>") %>% 
  dplyr::select(population, sheet_name, everything()) %>% 
  gather(key = concentration, value = OD, 3:ncol(all_data_fluc)) %>% 
  filter(!is.na(OD)) 
write_csv(fluc_raw, "data-processed/fluconazole-mic-feb262025.csv")



fluc1 <- fluc_raw %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  mutate(concentration = ifelse(concentration == 0, 0.1, concentration))

fluc1 %>% 
  ggplot(aes(x = concentration, y = OD, color = population)) + geom_point() +
  scale_x_log10()

auc_df_fluc1 <- fluc1 %>%
  arrange(population, concentration, sheet_name) %>%
  group_by(population, sheet_name) %>%
  summarise(AUC = trapz(concentration, OD))

auc_df_fluc1 %>% 
  ggplot(aes(x = population, y = AUC)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("figures/fluc-1-auc.png", width = 8, height = 6)



# fit fluc curves ---------------------------------------------------------

library(drc)
library(dplyr)

# Get unique population names
populations <- unique(fluc1$population)

# Create an empty named list to store models
models <- list()

# Loop through each population
for (pop in populations) {
  sub_df <- fluc1 %>% filter(population == pop)
  
  model <- tryCatch(
    drm(OD ~ concentration, data = sub_df, fct = LL.4()),
    error = function(e) {
      message("Model failed for ", pop, ": ", e$message)
      return(NULL)
    }
  )
  
  models[[pop]] <- model
}


# Check which models fit successfully
sapply(models, function(m) !is.null(m))

# Inspect one model
summary(models[["40_F2"]])
plot(models[["40_F2"]])




library(ggplot2)

# Create an empty list to hold prediction data frames
fitted_data_list <- list()

for (pop in names(models)) {
  model <- models[[pop]]
  if (is.null(model)) next  # skip failed fits
  
  # Extract original concentration values
  x_vals <- tryCatch(model[["data"]][["concentration"]], error = function(e) NULL)
  if (is.null(x_vals)) next
  
  # Remove zero and invalid values for log scale
  x_vals <- x_vals[is.finite(x_vals) & x_vals > 0]
  if (length(x_vals) < 2) next
  
  # Create a prediction grid
  conc_grid <- exp(seq(log(min(x_vals)), log(max(x_vals)), length.out = 100))
  
  # Predict from the model
  pred <- tryCatch(
    predict(model, newdata = data.frame(concentration = conc_grid)),
    error = function(e) rep(NA, length(conc_grid))
  )
  
  # Create fitted data frame
  fitted_df <- data.frame(
    concentration = conc_grid,
    OD = pred,
    population = pop
  )
  
  fitted_data_list[[pop]] <- fitted_df
}

# Combine all predictions
fitted_data <- do.call(rbind, fitted_data_list)


ggplot(fluc1, aes(x = concentration, y = OD, color = population)) +
  geom_point(alpha = 0.6) +
  geom_line(data = fitted_data, aes(x = concentration, y = OD, color = population), linewidth = 1) +
  scale_x_log10() +
  labs(title = "Logistic model fits by population",
       x = "Drug concentration",
       y = "Optical Density (OD)") +
  theme_minimal()


library(drc)

# Initialize an empty data frame
ic50_df <- data.frame(
  population = character(),
  IC50 = numeric(),
  stringsAsFactors = FALSE
)

# Loop through models and extract IC50
for (pop in names(models)) {
  model <- models[[pop]]
  if (is.null(model)) {
    ic50_val <- NA
  } else {
    ic50_val <- tryCatch(
      ED(model, 50, interval = "none")[1],  # 50% effective dose
      error = function(e) NA
    )
  }
  
  ic50_df <- rbind(ic50_df, data.frame(population = pop, IC50 = ic50_val))
}


library(ggplot2)

# Merge fitted values and IC50s (optional, but helpful for faceting)
# Ensure population is a character/factor in all frames
fitted_data$population <- as.character(fitted_data$population)
fluc1$population <- as.character(fluc1$population)
ic50_df$population <- as.character(ic50_df$population)

# Plot
ggplot() +
  # Raw data points
  geom_point(data = fluc1, aes(x = concentration, y = OD), alpha = 0.6) +
  
  # Model fit lines
  geom_line(data = fitted_data, aes(x = concentration, y = OD), color = "blue", linewidth = 1) +
  
  # IC50 vertical lines
  geom_vline(data = ic50_df, aes(xintercept = IC50), linetype = "dashed", color = "red") +
  
  # Log x-axis
  scale_x_log10() +
  
  facet_wrap(~ population, scales = "free") +
  
  labs(title = "Logistic fits with IC50 by population",
       x = "Drug concentration (log scale)",
       y = "Optical Density (OD)") +
  theme_minimal()
ggsave("figures/ic50-fits.png", width = 14, height = 12)  


ic50_df %>% 
  filter(population != "Blank") %>% 
  ggplot(aes(x = population, y = IC50)) + geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("figures/fluc1-ic50.png", width = 8, height = 6)



# bootstrap the dose response curves --------------------------------------

bootstrap_curve <- function(data, n_boot = 100, conc_grid = NULL) {
  # Filter to valid data
  data <- data %>% filter(is.finite(OD), is.finite(concentration), concentration > 0)
  
  if (nrow(data) < 4) return(NULL)
  
  # Define concentration grid if not supplied
  if (is.null(conc_grid)) {
    conc_grid <- exp(seq(log(min(data$concentration)), log(max(data$concentration)), length.out = 100))
  }
  
  # Matrix to store bootstrap predictions
  preds <- matrix(NA, nrow = n_boot, ncol = length(conc_grid))
  
  for (i in 1:n_boot) {
    boot_sample <- data[sample(nrow(data), replace = TRUE), ]
    
    fit <- tryCatch(
      drm(OD ~ concentration, data = boot_sample, fct = LL.4()),
      error = function(e) NULL
    )
    
    if (!is.null(fit)) {
      pred <- tryCatch(
        predict(fit, newdata = data.frame(concentration = conc_grid)),
        error = function(e) rep(NA, length(conc_grid))
      )
      preds[i, ] <- pred
    }
  }
  
  # Compute 2.5%, 50%, and 97.5% percentiles
  ci_df <- data.frame(
    concentration = conc_grid,
    OD_lower = apply(preds, 2, quantile, 0.025, na.rm = TRUE),
    OD_median = apply(preds, 2, quantile, 0.5, na.rm = TRUE),
    OD_upper = apply(preds, 2, quantile, 0.975, na.rm = TRUE)
  )
  
  return(ci_df)
}

# Create list to store bootstrap curves
bootstrap_results <- list()

for (pop in unique(fluc1$population)) {
  pop_data <- fluc1 %>% filter(population == pop)
  ci_curve <- bootstrap_curve(pop_data, n_boot = 100)
  if (!is.null(ci_curve)) {
    ci_curve$population <- pop
    bootstrap_results[[pop]] <- ci_curve
  }
}

# Combine into one data frame
ci_all <- do.call(rbind, bootstrap_results)


ggplot() +
  geom_point(data = fluc1, aes(x = concentration, y = OD), alpha = 0.5) +
  geom_ribbon(data = ci_all, aes(x = concentration, ymin = OD_lower, ymax = OD_upper), fill = "blue", alpha = 0.2) +
  geom_line(data = ci_all, aes(x = concentration, y = OD_median), color = "blue") +
  scale_x_log10() +
  facet_wrap(~ population, scales = "free") +
  labs(title = "Bootstrapped Logistic Curves with 95% CI",
       x = "Drug concentration (log scale)",
       y = "Optical Density (OD)") +
  theme_minimal()
ggsave("figures/ic50-fits-boot.png", width = 14, height = 12)  


# get the bootstrapped ic 50 values ---------------------------------------

bootstrap_ic50 <- function(data, n_boot = 100) {
  data <- data %>% filter(is.finite(OD), is.finite(concentration), concentration > 0)
  if (nrow(data) < 4) return(NULL)
  
  ic50_vals <- numeric(n_boot)
  
  for (i in 1:n_boot) {
    boot_sample <- data[sample(nrow(data), replace = TRUE), ]
    
    fit <- tryCatch(
      drm(OD ~ concentration, data = boot_sample, fct = LL.4()),
      error = function(e) NULL
    )
    
    ic <- tryCatch(
      if (!is.null(fit)) ED(fit, 50, interval = "none")[1] else NA_real_,
      error = function(e) NA_real_
    )
    
    # Only accept plausible IC50s
    if (is.finite(ic) && ic > 0 && ic < max(data$concentration) * 10) {
      ic50_vals[i] <- ic
    } else {
      ic50_vals[i] <- NA
    }
  }
  
  valid_ics <- ic50_vals[!is.na(ic50_vals)]
  
  if (length(valid_ics) < 30) return(NULL)  # drop unreliable estimates
  
  quantiles <- quantile(valid_ics, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
  
  return(data.frame(
    IC50_lower = quantiles[1],
    IC50_median = quantiles[2],
    IC50_upper = quantiles[3]
  ))
}


ic50_boot_results <- list()

for (pop in unique(fluc1$population)) {
  pop_data <- fluc1 %>% filter(population == pop)
  res <- bootstrap_ic50(pop_data, n_boot = 100)
  if (!is.null(res)) {
    res$population <- pop
    ic50_boot_results[[pop]] <- res
  }
}

# Combine into a tidy data frame
ic50_boot_df <- bind_rows(ic50_boot_results)
ic50_boot_df <- ic50_boot_df %>%
  dplyr::select(population, IC50_median, IC50_lower, IC50_upper) %>% 
  filter(population != "Blank")

ggplot(ic50_boot_df, aes(x = population, y = IC50_median)) +
  geom_point() +
  geom_errorbar(aes(ymin = IC50_lower, ymax = IC50_upper), width = 0.2) +
  labs(y = "IC50 (bootstrap 95% CI)", x = "Population") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# next steps for assessing resistance -------------------------------------

### first bring in all the fluconazole data
### then figure out bootstrapping and a way to assess statistical differences
## remember that the replicates can be treated as real replicates, so this should be the unique identifier that we fit with the non-linear model


