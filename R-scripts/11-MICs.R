

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


fluc1 <- all_data_fluc %>% 
  # filter(grepl("FLZ", sheet_name)) %>%
  rename("population" = "<>") %>% 
  dplyr::select(population, sheet_name, everything()) %>% 
  gather(key = concentration, value = OD, 3:ncol(all_data_fluc)) %>% 
  filter(!is.na(OD)) %>% 
  mutate(concentration = as.numeric(concentration)) 

fluc1 %>% 
  ggplot(aes(x = concentration, y = OD, color = population)) + geom_point() 

auc_df_fluc1 <- fluc1 %>%
  arrange(population, concentration, sheet_name) %>%
  group_by(population, sheet_name) %>%
  summarise(AUC = trapz(concentration, OD))

auc_df_fluc1 %>% 
  ggplot(aes(x = population, y = AUC)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("figures/fluc-1-auc.png", width = 8, height = 6)
