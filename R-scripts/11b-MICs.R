

### MICs
library(tidyverse)
library(readxl)
library(drc)       # for dose–response models
library(pracma)    # for AUC via trapezoid
library(cowplot)
theme_set(theme_cowplot())
library(minpack.lm)
library(dplyr)
library(car)

# 
# thing1 <- read_excel("data-raw/MICs/FINAL/Mar6.25(35C-ev_ALL3+40C-ev_Res)/24h.xlsx", sheet = "Set3_Rep3_AMPB", range = "A24:M32") %>% 
#   rename("population" = "<>")


# Path to the Excel file
### ok these are all the data now

march_data <- "data-raw/MICs/FINAL/Mar6.25(35C-ev_ALL3+40C-ev_Res)/24h.xlsx" ## has all 3 drugs
january_data <- "data-raw/MICs/FINAL/Jan31.25(40C-ev_CASP_AMPB)/24h.xlsx" ## casp and amph
february_data <- "data-raw/MICs/FINAL/Feb26.25(40C-ev_FLZ)/24h.xlsx" ### fluc only
november_data <- "data-raw/MICs/FINAL/Nov9.25(Casp-ev_CASP)/24h.xlsx"

# Get all sheet names
sheet_names_march <- excel_sheets(march_data)

# Read and combine all sheets, adding a column with the sheet name
all_data_march <- map_dfr(sheet_names_march, function(sheet) {
  read_excel(march_data, sheet = sheet,range = "A24:M32") %>%
    mutate(sheet_name = sheet)
}) %>% 
  rename("population" = "<>") %>% 
  dplyr::select(population, sheet_name, everything()) %>% 
  gather(key = concentration, value = OD, 3:ncol(.)) %>%
  filter(!is.na(OD)) %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  mutate(drug = case_when(grepl("CASP", sheet_name) ~ "caspofungin",
                          grepl("AMPB", sheet_name) ~ "amphotericine",
                          grepl("FLZ", sheet_name) ~ "fluconazole",
                          TRUE ~ NA))



# Get all sheet names january
sheet_names_jan <- excel_sheets(january_data)

# Read and combine all sheets, adding a column with the sheet name
all_data_jan <- map_dfr(sheet_names_jan, function(sheet) {
  read_excel(january_data, sheet = sheet,range = "A24:M32") %>%
    mutate(sheet_name = sheet)
}) %>% 
  rename("population" = "<>") %>% 
  dplyr::select(population, sheet_name, everything()) %>% 
  gather(key = concentration, value = OD, 3:ncol(.)) %>%
  filter(!is.na(OD)) %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  mutate(drug = case_when(grepl("CASP", sheet_name) ~ "caspofungin",
                          grepl("AMPB", sheet_name) ~ "amphotericine",
                          TRUE ~ NA))


# Get all sheet names february
sheet_names_feb <- excel_sheets(february_data)

# Read and combine all sheets, adding a column with the sheet name
all_data_feb <- map_dfr(sheet_names_feb, function(sheet) {
  read_excel(february_data, sheet = sheet,range = "A24:M32") %>%
    mutate(sheet_name = sheet)
}) %>% 
  rename("population" = "<>") %>% 
  dplyr::select(population, sheet_name, everything()) %>% 
  gather(key = concentration, value = OD, 3:ncol(.)) %>%
  filter(!is.na(OD)) %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  mutate(drug = "fluconazole")


# Get all sheet names november
sheet_names_nov <- excel_sheets(november_data)

# Read and combine all sheets, adding a column with the sheet name
all_data_nov <- map_dfr(sheet_names_nov, function(sheet) {
  read_excel(november_data, sheet = sheet,range = "A25:M30") %>%
    mutate(sheet_name = sheet)
}) %>% 
  rename("population" = "<>") %>% 
  dplyr::select(population, sheet_name, everything()) %>% 
  gather(key = concentration, value = OD, 3:ncol(.)) %>%
  filter(!is.na(OD)) %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  mutate(drug = "caspofungin")



all_mic_data <- bind_rows(all_data_feb, all_data_march, all_data_jan, all_data_nov) 



all_mic_data %>% 
  ggplot(aes(x = concentration, y = OD, color = population)) + geom_point() +
  scale_x_log10() +
  facet_wrap( ~ drug)

write_csv(all_mic_data, "data-processed/all_mic_data.csv")


# older data read in ------------------------------------------------------


fluc_raw_march <- all_data %>% 
  filter(grepl("FLZ", sheet_name)) %>%
  rename("population" = "<>") %>% 
  dplyr::select(population, sheet_name, everything()) %>% 
  gather(key = concentration, value = OD, 3:ncol(all_data)) %>%
  filter(!is.na(OD)) %>% 
  mutate(concentration = as.numeric(concentration)) 
write_csv(fluc_raw_march, "data-raw/fluc-mic-march2025.csv")


fluc <- fluc_raw_march  %>% 
  mutate(concentration = ifelse(concentration == 0, 0.1, concentration))


casp <- all_data %>% 
  filter(grepl("CASP", sheet_name)) %>%
  rename("population" = "<>") %>% 
  dplyr::select(population, sheet_name, everything()) %>% 
  gather(key = concentration, value = OD, 3:ncol(all_data)) %>%
  filter(!is.na(OD)) %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  mutate(concentration = ifelse(concentration == 0, 0.001, concentration)) %>% 
  mutate(block_time = "March")


casp %>% 
  ggplot(aes(x = concentration, y = OD, color = population)) + geom_point() +
  scale_x_log10()
  




# getting more of the caspofungin data ------------------------------------

file_path_casp <- "data-raw/MICs/FINAL/Jan31.25(40C-ev_CASP_AMPB)/24h.xlsx"

# Get all sheet names
sheet_names_casp <- excel_sheets(file_path_casp)

# Read and combine all sheets, adding a column with the sheet name
all_data_casp <- map_dfr(sheet_names_casp , function(sheet) {
  read_excel(file_path, sheet = sheet,range = "A24:M32") %>%
    mutate(sheet_name = sheet)
})

casp_raw_jan <- all_data_casp %>% 
  filter(grepl("CASP", sheet_name)) %>%
  rename("population" = "<>") %>% 
  dplyr::select(population, sheet_name, everything()) %>% 
  gather(key = concentration, value = OD, 3:ncol(all_data_casp)) %>%
  filter(!is.na(OD)) %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  mutate(concentration = ifelse(concentration == 0, 0.001, concentration)) %>% 
  mutate(block_time = "January")
write_csv(casp_raw_jan, "data-raw/casp-mic-jan2025.csv")

all_casp <- bind_rows(casp, casp_raw_jan) %>% 
  unite(unique_pop, population, block_time, remove = FALSE)


library(drc)       # for dose–response models
library(pracma)    # for AUC via trapezoid

ggplot(all_casp, aes(x = concentration, y = OD, color = population)) +
  geom_point() +
  geom_line() +
  scale_x_log10() +
  theme_minimal()

# Split and fit model
models <- all_casp %>%
  group_by(unique_pop) %>%
  group_split() %>%
  setNames(unique(all_casp$unique_pop)) %>%
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

ic50_df <- data.frame(unique_pop = names(ic50_vals), IC50 = ic50_vals)


auc_df <- all_casp %>%
  arrange(unique_pop, concentration) %>%
  group_by(unique_pop) %>%
  summarise(AUC = trapz(concentration, OD))

mic_vals <- all_casp %>%
  group_by(unique_pop) %>%
  summarise(MIC = min(concentration[OD < 0.1], na.rm = TRUE))  # adjust threshold as needed

resistance_metrics <- ic50_df %>%
  left_join(auc_df, by = "unique_pop") %>%
  left_join(mic_vals, by = "unique_pop") %>% 
  filter(population != "Blank")

kruskal.test(IC50 ~ population, data = resistance_metrics)

resistance_metrics %>% 
  filter(IC50 < .5) %>%  ### just getting rid of one outlier for now
  ggplot(aes(x = population, y = IC50)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("figures/caspofungin-ic-50.png", width = 8, height = 6)

ic50_df %>% ### ok somehow now the 35 degrees are missing
  filter(IC50 < .5) %>%  ### just getting rid of one outlier for now
  ggplot(aes(x = unique_pop, y = IC50)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("figures/caspofungin-ic-50.png", width = 8, height = 6)


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
## maybe find the reverse logistic model from chlamee rstar?


fluc1b <- read_csv("data-raw/fluc-mic-march2025.csv") %>% 
  mutate(block = "march2025")
fluc1c <- read_csv("data-processed/fluconazole-mic-feb262025.csv") %>% 
  mutate(block = "feb2025")


all_fluc <- bind_rows(fluc1b, fluc1c) %>%
  separate(sheet_name, into = c("set", "rep", "drug"), sep = "_")

a2 <- all_fluc %>%
  unite("pop_rep", population, rep, set, sep = "_")
  
str(a2)

a2 %>% 
  arrange(pop_rep, concentration) %>%
  drop_na(concentration, OD) %>% 
  # filter(pop_rep == "40_E7_Rep3") %>% 
  filter(!grepl("Blank", pop_rep)) %>% 
  filter(grepl(40, pop_rep)) %>% 
ggplot(aes(x = concentration, y = OD, group = interaction(pop_rep), color = pop_rep)) + geom_point() + geom_path() +
  theme(legend.position="none") + scale_x_log10()
  

### now let's figure out how to fit these curves using nlsLM


library(minpack.lm)
library(car)


# a3 <- a2 %>% 
#   filter(concentration > 0) %>% 
#   filter(!grepl("Blank", pop_rep)) %>% 
#   filter(pop_rep == "40_C9_Rep3_Set4")

start_vals <- list(
  d = max(a3$OD),
  b = 10,
  e = median(a3$concentration)
)

# Fit the model
fit <- nlsLM(
  OD ~ d / (1 + exp(b * (log(concentration) - log(e)))),
  data = a3,
  start = start_vals,
  lower = c(d = 0, b = 0.01, e = min(a3$concentration)),
  upper = c(d = 2, b = 30, e = max(a3$concentration) * 5)
)

summary(fit)
boot_fit <- Boot(fit, f = coef, method = "residual", R = 5000)

summary(boot_fit)
ic50_vals <- boot_fit$t[, "e"]

hist(ic50_vals, breaks = 50, col = "skyblue", main = "Bootstrapped IC₅₀", xlab = "IC₅₀")
abline(v = quantile(ic50_vals, c(0.025, 0.975)), col = "red", lty = 2)



# 95% percentile CIs for each parameter
confint(boot_fit, level = 0.95, type = "perc")


concentration_grid <- data.frame(
  concentration = exp(seq(
    log(min(a3$concentration)),
    log(max(a3$concentration)),
    length.out = 100
  ))
)


# Extract bootstrapped parameter matrix (each row: one bootstrap)
boot_params <- boot_fit$t
boot_params <- boot_params[complete.cases(boot_params), ]  # drop failures

# For each row of parameters, predict the fitted values
pred_matrix <- apply(boot_params, 1, function(pars) {
  d <- pars["d"]
  b <- pars["b"]
  e <- pars["e"]
  d / (1 + exp(b * (log(concentration_grid$concentration) - log(e))))
})

# Convert to data frame and compute ribbons
pred_df <- concentration_grid %>%
  mutate(
    fit = predict(fit, newdata = .),
    lower = apply(pred_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
    upper = apply(pred_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)
  )


ggplot() +
  geom_point(alpha = 0.6, color = "black", data = a3, aes(x = concentration, y = OD)) +
  geom_ribbon(
    data = pred_df,
    aes(ymin = lower, ymax = upper, x = concentration),
    fill = "skyblue", alpha = 0.3
  ) +
  geom_line(data = pred_df, aes(y = fit, x = concentration), color = "blue", size = 1) +
  scale_x_log10() +
  labs(title = "Dose-Response with 95% CI Ribbon",
       x = "Drug concentration (log scale)",
       y = "OD") +
  theme_minimal()



# Predict over a smooth range
newdata <- data.frame(concentration = exp(seq(log(min(a3$concentration)),
                                              log(max(a3$concentration)),
                                              length.out = 100)))

newdata$fit <- predict(fit, newdata)

# Plot
plot(OD ~ concentration, data = a3, log = "x", pch = 16)
lines(fit ~ concentration, data = newdata, col = "blue", lwd = 2)



# now fit all of them -----------------------------------------------------




# fluc1b <- read_csv("data-raw/fluc-mic-march2025.csv") %>% 
#   mutate(block = "march2025")
# fluc1c <- read_csv("data-processed/fluconazole-mic-feb262025.csv") %>% 
#   mutate(block = "feb2025")
# # 
# # 
# all_fluc <- bind_rows(fluc1b, fluc1c) %>%
#   separate(sheet_name, into = c("set", "rep", "drug"), sep = "_")
# 
# a2 <- all_fluc %>%
#   unite("pop_rep", population, rep, set, sep = "_")



# start here for fitting all the MICs (nov 2025) --------------------------




a2 <- all_mic_data %>%
  separate(sheet_name, into = c("set", "rep", "drug_type"), sep = "_", remove = FALSE) %>% 
  unite("pop_rep", population, rep, set, drug, sep = "_")

### nov 25 JB come back here


a3 <- a2 %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  mutate(concentration = ifelse(concentration == 0, 0.000000000001, concentration)) %>% 
  # filter(concentration > 0) %>% 
  filter(!grepl("Blank", pop_rep)) 

# data <- a3


# boostrapping function ---------------------------------------------------



fit_bootstrap_ic50 <- function(data, group_var = "pop_rep", R = 5000, seed = 123) {
  set.seed(seed)
  
  warning_log <- list()  # NEW: capture warnings
  pops <- unique(data[[group_var]])
  ic50_list <- list()
  pred_list <- list()
  raw_list <- list()
  boot_param_list <- list()
  fit_param_list <- list()  # NEW
  error_log <- list()
  
  for (pop in pops) {
    message("Fitting: ", pop)
    
    subdata <- data %>%
      filter(.data[[group_var]] == pop, concentration > 0)
    
    if (nrow(subdata) < 5) {
      error_log[[pop]] <- data.frame(
        pop_rep = pop,
        error_message = "Insufficient data points"
      )
      next
    }
    
    start_vals <- list(
      d = max(subdata$OD),
      b = 10,
      e = median(subdata$concentration)
    )
    
    fit_warnings <- character(0)  # container for warnings
    
    fit <- tryCatch(
      withCallingHandlers(
        expr = eval(bquote(
          nlsLM(
            OD ~ d / (1 + exp(b * (log(concentration) - log(e)))),
            data = .(subdata),
            start = .(start_vals),
            lower = c(d = 0, b = 0.01, e = min(.(subdata)$concentration)),
            upper = c(d = 2, b = 50, e = max(.(subdata)$concentration) * 5),
            control = nls.lm.control(maxiter = 500)
          )
        )),
        warning = function(w) {
          fit_warnings <<- c(fit_warnings, conditionMessage(w))
          invokeRestart("muffleWarning")  # suppress console output
        }
      ),
      error = function(e) {
        error_log[[pop]] <<- data.frame(
          pop_rep = pop,
          error_message = e$message
        )
        return(NULL)
      }
    )
    
    # After fitting, store warnings if any
    if (length(fit_warnings) > 0) {
      warning_log[[pop]] <- data.frame(
        pop_rep = pop,
        warning_message = fit_warnings,
        stringsAsFactors = FALSE
      )
    }
    
    if (is.null(fit)) next
    
    # Store point-estimate parameters
    fit_param_list[[pop]] <- data.frame(
      pop_rep = pop,
      t(coef(fit))
    )
    
    # Bootstrap
    boot_fit <- Boot(fit, f = coef, method = "residual", R = R)
    boot_params <- boot_fit$t
    boot_params <- boot_params[complete.cases(boot_params), ]
    
    # IC50 summary
    ic50_vals <- boot_params[, "e"]
    ic50_summary <- data.frame(
      pop_rep = pop,
      IC50 = coef(fit)["e"],
      IC50_lower = quantile(ic50_vals, 0.025, na.rm = TRUE),
      IC50_upper = quantile(ic50_vals, 0.975, na.rm = TRUE),
      n_boot = nrow(boot_params)
    )
    ic50_list[[pop]] <- ic50_summary
    
    # Bootstrapped parameter values
    boot_df <- as.data.frame(boot_params)
    boot_df$bootstrap_id <- seq_len(nrow(boot_df))
    boot_df[[group_var]] <- pop
    boot_param_list[[pop]] <- boot_df
    
    # Prediction grid
    grid <- data.frame(
      concentration = exp(seq(
        log(min(subdata$concentration)),
        log(max(subdata$concentration)),
        length.out = 100
      ))
    )
    
    pred_matrix <- apply(boot_params, 1, function(pars) {
      d <- pars["d"]
      b <- pars["b"]
      e <- pars["e"]
      d / (1 + exp(b * (log(grid$concentration) - log(e))))
    })
    
    grid$fit <- predict(fit, newdata = grid)
    grid$lower <- apply(pred_matrix, 1, quantile, probs = 0.025, na.rm = TRUE)
    grid$upper <- apply(pred_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)
    grid[[group_var]] <- pop
    
    pred_list[[pop]] <- grid
    raw_list[[pop]] <- subdata
  }
  
  # Combine error messages into a dataframe (if any)
  error_df <- if (length(error_log) > 0) bind_rows(error_log) else data.frame()
  fit_param_df <- if (length(fit_param_list) > 0) bind_rows(fit_param_list) else data.frame()
  warning_df <- if (length(warning_log) > 0) bind_rows(warning_log) else data.frame()
  
  list(
    warning_log = warning_df,
    ic50_table = bind_rows(ic50_list),
    fit_data = bind_rows(pred_list),
    raw_data = bind_rows(raw_list),
    boot_params = bind_rows(boot_param_list),
    fit_params = fit_param_df,         
    error_log = error_df
  )
}


results <- fit_bootstrap_ic50(a3) ### come back here to make sure none of these are hitting against the bounds

View(results$fit_data)
View(results$warning_log)


boot_params1 <- results$boot_params %>% 
  filter(b < 50) ### filtering out the bound hitting ones



b2 <- boot_params1 %>%
  group_by(pop_rep) %>% 
  summarise(mean_b = mean(b),
            upper_b = quantile(b, 0.975),
            lower_b = quantile(b, 0.025)) %>% 
  mutate(evolution_history = case_when(grepl(40, pop_rep) ~ "evolved 40",
                                       grepl(35, pop_rep) ~ "evolved 35",
                                       grepl("fR", pop_rep) ~ "fRS585")) %>% 
  mutate(drug = case_when(grepl("amph", pop_rep) ~ "amphotericine",
                          grepl("fluc", pop_rep) ~ "fluconazole",
                          grepl("casp", pop_rep) ~ "caspofungin"))

ggplot() +
  geom_pointrange(aes(x = pop_rep, y = mean_b, ymin = lower_b, ymax = upper_b, color = evolution_history), data = b2) +
  facet_wrap( ~ drug)
ggsave("figures/mics-pointrange-all-drugs.png", width = 12, height = 5)


e2 <- boot_params1 %>%
  mutate(drug = case_when(grepl("amph", pop_rep) ~ "amphotericine",
                          grepl("fluc", pop_rep) ~ "fluconazole",
                          grepl("casp", pop_rep) ~ "caspofungin")) %>% 
  group_by(pop_rep, drug) %>% 
  summarise(mean_e = mean(e),
            upper_e = quantile(e, 0.975),
            lower_e = quantile(e, 0.025)) %>% 
  mutate(evolution_history = case_when(grepl(40, pop_rep) ~ "evolved 40",
                                       grepl(35, pop_rep) ~ "evolved 35",
                                       grepl("fR", pop_rep) ~ "fRS585")) 


ggplot() +
  geom_pointrange(aes(x = pop_rep, y = mean_e, ymin = lower_e, ymax = upper_e, color = evolution_history), data = e2) +
  facet_wrap( ~ drug, scales = "free")
ggsave("figures/mics-pointrange-e-all-drugs.png", width = 12, height = 5)



### note that this boot file is missing a bunch of fits, presumably because they failed at the fitting step


tolerances <- results$ic50_table

write_csv(tolerances, "data-processed/tolerances.csv")


t2 <- tolerances %>% 
  mutate(evolution_history = case_when(grepl(40, pop_rep) ~ "evolved 40",
                                       grepl(35, pop_rep) ~ "evolved 35",
                                       grepl("fR", pop_rep) ~ "fRS585",
                                       grepl("WT Casp-Ev", pop_rep) ~ "caspofungin evolved")) %>% 
  mutate(drug = case_when(grepl("amph", pop_rep) ~ "amphotericine",
                          grepl("fluc", pop_rep) ~ "fluconazole",
                          grepl("casp", pop_rep) ~ "caspofungin"))

t2 %>% 
  ggplot(aes(x = pop_rep, y = IC50, color = evolution_history)) + geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(legend.position = "none") +
  facet_wrap( ~ drug, scales = "free")
ggsave("figures/mics-fluc-wide-all-drugs.png", width = 20, height = 6)


library(plotrix)




t3 <- t2 %>% 
  # filter(IC50 < 100) %>% 
  # separate(pop_rep, into = c("evolution_temp", "well", "rep", "set"), sep = "_", remove = FALSE) %>% 
  separate(pop_rep, into = c("evolution_temp", "everything_else"), sep = "_", remove = FALSE) %>% 
  mutate(drug = case_when(grepl("amph", pop_rep) ~ "amphotericine",
                          grepl("fluc", pop_rep) ~ "fluconazole",
                          grepl("casp", pop_rep) ~ "caspofungin")) %>% 
  
  # unite("hist_well", evolution_temp, well, remove = FALSE) %>%
  # mutate(hist_well = ifelse(grepl("fR", hist_well), "fRS585", hist_well)) %>%
  # group_by(drug, evolution_history) %>% 
  # summarise(mean_ic50 = mean(IC50)) %>% View
  group_by(evolution_history, drug) %>% 
  summarise(mean_ic502 = mean(IC50),
            se_ic50 = std.error(IC50))


### this takes an average at the population level
t2b <- t2 %>% 
  # filter(IC50 < 100) %>% 
  separate(pop_rep, into = c("evolution_temp", "well", "rep", "set"), sep = "_", remove = FALSE) %>%
  # separate(pop_rep, into = c("evolution_temp", "everything_else"), sep = "_", remove = FALSE) %>% 
  mutate(drug = case_when(grepl("amph", pop_rep) ~ "amphotericine",
                          grepl("fluc", pop_rep) ~ "fluconazole",
                          grepl("casp", pop_rep) ~ "caspofungin")) %>% 
  
  # unite("hist_well", evolution_temp, well, remove = FALSE) %>%
  # mutate(hist_well = ifelse(grepl("fR", hist_well), "fRS585", hist_well)) %>%
  # group_by(drug, evolution_history) %>% 
  # summarise(mean_ic50 = mean(IC50)) %>% View
  group_by(evolution_history, drug, well) %>% 
  summarise(mean_ic503 = mean(IC50),
            se_ic503 = std.error(IC50))




  ggplot() +
  geom_pointrange(aes(x = evolution_history, y = mean_ic502, ymin = mean_ic502 - se_ic50, ymax = mean_ic502 + se_ic50), data = t3, color = "blue") + ylab("IC50") +
    facet_wrap( ~ drug, scales = "free")
  ggsave("figures/ic50s-evolution-history-all-drugs-casp.png", width = 12, height = 4)
  
 
  
  #### now make this plot with all the populations, so we can see the spread
  
  ggplot() +
    geom_point(aes(x = evolution_history, y = mean_ic503), data = t2b, alpha = 0.5) +
    geom_pointrange(aes(x = evolution_history, y = mean_ic502, ymin = mean_ic502 - se_ic50, ymax = mean_ic502 + se_ic50), data = t3, color = "blue") +
    ylab("IC50") +
    facet_wrap( ~ drug, scales = "free") +
    xlab("Evolution history")
  ggsave("figures/ic50s-evolution-history-all-drugs-all-pops-casp.png", width = 12, height = 4)
  
  
   fit_data2 <- results$fit_data %>% 
    mutate(evolution_history = case_when(grepl(40, pop_rep) ~ "evolved 40",
                                         grepl(35, pop_rep) ~ "evolved 35",
                                         grepl("fR", pop_rep) ~ "fRS585",
           grepl("WT Casp-Ev", pop_rep) ~ "caspofungin evolved")) %>% 
     mutate(drug = case_when(grepl("amph", pop_rep) ~ "amphotericine",
                             grepl("fluc", pop_rep) ~ "fluconazole",
                             grepl("casp", pop_rep) ~ "caspofungin"))
  
  
  raw_data2 <- results$raw_data %>% 
    mutate(evolution_history = case_when(grepl(40, pop_rep) ~ "evolved 40",
                                         grepl(35, pop_rep) ~ "evolved 35",
                                         grepl("WT Casp-Ev", pop_rep) ~ "caspofungin evolved",
                                   grepl("fR", pop_rep) ~ "fRS585")) %>% 
    mutate(drug = case_when(grepl("amph", pop_rep) ~ "amphotericine",
                            grepl("fluc", pop_rep) ~ "fluconazole",
                            grepl("casp", pop_rep) ~ "caspofungin"))

ggplot() +
  geom_ribbon(data =  fit_data2, aes(x = concentration, ymin = lower, ymax = upper, group = pop_rep), 
              fill = "skyblue", alpha = 0.3) +
  geom_line(data =  fit_data2, aes(y = fit, x = concentration, group = pop_rep), color = "blue", size = .5) +
  geom_point(size = 1.5, alpha = 0.6, data = raw_data2, aes(x = concentration, y = OD)) +
  scale_x_log10() +
  facet_wrap(drug ~ evolution_history) +
  labs(title = "Dose-Response Curves with 95% Bootstrapped CI",
       x = "Drug concentration (log scale)",
       y = "OD") +
  theme(legend.position = "none")
# ggsave("figures/mic-logistic-boot.png", width = 25, height = 25)
ggsave("figures/mic-logistic-boot-facet-all-drugs.png", width = 15, height = 10)



raw_data2 %>% 
  filter(drug == "caspofungin") %>% 
  distinct(concentration) %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  arrange()
  
  


problem_reps <- c(
# "40_D4_Rep3_Plate2",
# "40_C7_Rep1_Plate1",
# "40_C5_Rep1_Plate1",
# "40_F2_Rep2_Plate3",
# "40_B6_Rep2_Plate1",
# "35_F4_Rep2_Plate3",
# "35_F4_Rep3_Set3",
# "40_E9_Rep3_Plate2",
# "40_E9_Rep1_Plate2",
"40_G9_Rep3_Set4",
"40_G9_Rep1_Set4",
"40_G9_Rep3_Plate3",
"40_G11_Rep3_Plate3",
"40_G11_Rep1_Plate3",
"40_G11_Rep3_Set4",
"40_E11_Rep1_Plate2")


#### come back here Nov 1 2025 (see how many of the bootstraps are hitting against the bounds etc.)

ggplot() +
  geom_point(size = 1.5, alpha = 0.6, data = subset(raw_data2, pop_rep %in% problem_reps), aes(x = concentration, y = OD)) +
  geom_ribbon(data =  subset(fit_data2, pop_rep %in% problem_reps), aes(x = concentration, ymin = lower, ymax = upper, group = pop_rep), 
              fill = "skyblue", alpha = 0.3) +
  geom_line(data =  subset(fit_data2, pop_rep %in% problem_reps), aes(y = fit, x = concentration, group = pop_rep), color = "blue", size = .5) +
  scale_x_log10() +
  facet_wrap(~ pop_rep) +
  labs(title = "Dose-Response Curves with 95% Bootstrapped CI",
       x = "Drug concentration (log scale)",
       y = "OD") +
  theme(legend.position = "none")
# ggsave("figures/mic-logistic-boot.png", width = 25, height = 25)
ggsave("figures/mic-logistic-boot-facet.png", width = 15, height = 10)


### look at the extremes

low_ic50 <- t2 %>% 
  top_n(wt = IC50, n = -10)

high_ic50 <- t2 %>% 
  top_n(wt = IC50, n = 10)
fit_data3 <- fit_data2 %>% 
  mutate(low_high_ic50 = ifelse(pop_rep %in% low_ic50$pop_rep, "low_ic50", NA))

ggplot() +
  geom_point(size = 1.5, alpha = 0.6, data = subset(raw_data2, pop_rep %in% low_ic50$pop_rep | pop_rep %in% high_ic50$pop_rep), aes(x = concentration, y = OD)) +
  geom_ribbon(data =  subset(fit_data3, pop_rep %in% low_ic50$pop_rep | pop_rep %in% high_ic50$pop_rep), aes(x = concentration, ymin = lower, ymax = upper, group = pop_rep, fill = low_high_ic50), 
               alpha = 0.3) +
  geom_line(data =  subset(fit_data3, pop_rep %in% low_ic50$pop_rep | pop_rep %in% high_ic50$pop_rep), aes(y = fit, x = concentration, group = pop_rep, color = low_high_ic50), size = .5) +
  scale_x_log10() +
  facet_wrap(~ pop_rep) +
  labs(title = "Dose-Response Curves with 95% Bootstrapped CI",
       x = "Drug concentration (log scale)",
       y = "OD") +
  theme(legend.position = "none")
ggsave("figures/low-ic-50.png", width = 10, height = 10)

ggplot() +
  geom_point(size = 1.5, alpha = 0.6, data = subset(raw_data2, pop_rep %in% low_ic50$pop_rep | pop_rep %in% high_ic50$pop_rep), aes(x = concentration, y = OD)) +
  geom_ribbon(data =  subset(fit_data3, pop_rep %in% low_ic50$pop_rep | pop_rep %in% high_ic50$pop_rep), aes(x = concentration, ymin = lower, ymax = upper, group = pop_rep, fill = low_high_ic50), 
              alpha = 0.3) +
  geom_line(data =  subset(fit_data3, pop_rep %in% low_ic50$pop_rep | pop_rep %in% high_ic50$pop_rep), aes(y = fit, x = concentration, group = pop_rep, color = low_high_ic50), size = .5) +
  scale_x_log10() +
  # facet_wrap(~ pop_rep) +
  labs(title = "Dose-Response Curves with 95% Bootstrapped CI",
       x = "Drug concentration (log scale)",
       y = "OD") +
  theme(legend.position = "none")
ggsave("figures/low-high-ic-50.png", width = 10, height = 10)





ggplot() +
  geom_point(size = 1.5, alpha = 0.6, data = raw_data2, aes(x = concentration, y = OD)) +
  geom_ribbon(data =  fit_data2, aes(x = concentration, ymin = lower, ymax = upper, group = pop_rep), 
              fill = "grey", alpha = 0.3) +
  geom_line(data =  fit_data2, aes(y = fit, x = concentration, group = pop_rep, color = evolution_history), size = .5) +
  scale_x_log10() +
  facet_wrap(~ pop_rep) +
  labs(title = "Dose-Response Curves with 95% Bootstrapped CI",
       x = "Drug concentration (log scale)",
       y = "OD") +
  theme(legend.position = "none")
ggsave("figures/mic-logistic-boot-color-history.png", width = 25, height = 25)

ggplot() +
  geom_point(size = 1.5, alpha = 0.6, data = raw_all, aes(x = concentration, y = OD)) +
  geom_ribbon(data = pred_all, aes(x = concentration, ymin = lower, ymax = upper), 
              fill = "skyblue", alpha = 0.3) +
  geom_line(data = pred_all, aes(y = fit, x = concentration), color = "blue", size = 1) +
  scale_x_log10() +
  facet_wrap(~ evolution_history) +
  labs(title = "Dose-Response Curves with 95% Bootstrapped CI",
       x = "Drug concentration (log scale)",
       y = "OD") +
  theme(legend.position = "none")
ggsave("figures/mic-logistic-boot.png", width = 25, height = 25)



### let's compare the boot results with the means to just the raw fitted parameters

fp <- results$fit_params

fpb <- fp %>% 
  mutate(evolution_history = case_when(grepl(40, pop_rep) ~ "evolved 40",
                                       grepl(35, pop_rep) ~ "evolved 35",
                                       grepl("fR", pop_rep) ~ "fRS585")) 


fp2 <- fp %>% 
  mutate(evolution_history = case_when(grepl(40, pop_rep) ~ "evolved 40",
                                       grepl(35, pop_rep) ~ "evolved 35",
                                       grepl("fR", pop_rep) ~ "fRS585")) %>% 
  separate(pop_rep, into = c("evolution_temp", "well", "rep", "set"), sep = "_", remove = FALSE) %>% 
  unite("hist_well", evolution_temp, well, remove = FALSE) %>%
  group_by(hist_well, evolution_history) %>% 
  summarise(mean_ic50 = mean(e)) %>% 
  group_by(evolution_history) %>% 
  summarise(mean_ic502 = mean(mean_ic50),
            se_ic50 = std.error(mean_ic50))

ggplot() +
  geom_jitter(aes(x = evolution_history, y = e), data = fpb, alpha = .5, width = .3) +
  geom_pointrange(aes(x = evolution_history, y = mean_ic502, ymin = mean_ic502 - se_ic50, ymax = mean_ic502 + se_ic50), data = fp2, color = "blue") + ylab("IC50 (fluconazole)") 
  
ggsave("figures/ic50s-evolution-history-params-all-data.png", width = 6, height = 4)
