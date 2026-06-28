
# =============================================================================
# MIC Analysis
# =============================================================================

library(tidyverse)
library(readxl)
library(drc)
library(pracma)
library(cowplot)
library(minpack.lm)
library(car)
library(conflicted)

# Prefer dplyr's select function
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
theme_set(theme_cowplot())


# =============================================================================
# 1. Data import
# =============================================================================

# Focal populations — excludes Casp-evolved controls but retains Blank wells
FOCAL_POPS <- c("^35_", "^40_", "^fRS585$", "^Blank$")

march_data    <- "data-raw/MICs/FINAL/Mar6.25(35C-ev_ALL3+40C-ev_Res)/24h.xlsx"
january_data  <- "data-raw/MICs/FINAL/Jan31.25(40C-ev_CASP_AMPB)/24h.xlsx"
february_data <- "data-raw/MICs/FINAL/Feb26.25(40C-ev_FLZ)/24h.xlsx"
# november_data excluded: contains only Casp-evolved controls, not used

read_mic_file <- function(path, range = "A24:M32", default_drug = NA_character_) {
  # Extract month from file path
  month <- case_when(
    str_detect(path, "(?i)Jan") ~ "January",
    str_detect(path, "(?i)Feb") ~ "February",
    str_detect(path, "(?i)Mar") ~ "March",
    str_detect(path, "(?i)Nov") ~ "November",
    TRUE ~ NA_character_
  )
  
  excel_sheets(path) |>
    map(\(sh) {
      d <- tryCatch(read_excel(path, sheet = sh, range = range), error = function(e) NULL)
      if (is.null(d) || ncol(d) == 0 || nrow(d) == 0) return(NULL)
      d |>
        rename(population = 1) |>
        pivot_longer(-population, names_to = "concentration", values_to = "OD") |>
        filter(!is.na(OD)) |>
        mutate(
          concentration = as.numeric(concentration),
          sheet_name    = sh,
          set           = str_extract(sh, "(?i)(Set|Plate)\\d+"),
          rep           = str_extract(sh, "(?i)Rep\\d+"),
          month         = month,
          drug          = case_when(
            str_detect(sh, "CASP") ~ "caspofungin",
            str_detect(sh, "AMPB") ~ "amphotericin",
            str_detect(sh, "FLZ")  ~ "fluconazole",
            TRUE                   ~ default_drug
          )
        )
    }) |>
    bind_rows()
}

all_mic_data <- bind_rows(
  read_mic_file(february_data, default_drug = "fluconazole"),
  read_mic_file(march_data),
  read_mic_file(january_data)
) |>
  filter(str_detect(population, paste(FOCAL_POPS, collapse = "|")))

write_csv(all_mic_data, "data-processed/all_mic_data-new.csv")


# =============================================================================
# 2. IC50 fitting
# =============================================================================

# Prepare data: add evolution_history, drop Blank, replace zero concentration
# with min_conc / 10 per strain × drug (more sensible than a hardcoded tiny value)
mic_data <- all_mic_data |>
  filter(population != "Blank") |>
  mutate(
    evolution_history = case_when(
      str_detect(population, "^35_") ~ "35 evolved",
      str_detect(population, "^40_") ~ "40 evolved",
      population == "fRS585"          ~ "fRS585"
    )
  ) |>
  group_by(population, drug) |>
  mutate(
    min_conc    = min(concentration[concentration > 0], na.rm = TRUE),
    concentration = if_else(concentration == 0, min_conc / 10, concentration)
  ) |>
  ungroup() |>
  select(-min_conc)


# 3-parameter logistic: OD = d / (1 + exp(b * (log(conc) - log(IC50))))
# Fit per strain × drug, pooling all replicates and sets
fit_ic50 <- function(data, R = 1000) {
  strains <- data |> distinct(population, evolution_history, drug, month)
  ic50_list  <- vector("list", nrow(strains))
  pred_list  <- vector("list", nrow(strains))
  error_log  <- vector("list", nrow(strains))

  for (i in seq_len(nrow(strains))) {
    sid  <- strains$population[i]
    evo  <- strains$evolution_history[i]
    drg  <- strains$drug[i]
    mth  <- strains$month[i]

    dose_response <- data |> filter(population == sid, drug == drg) |> as.data.frame()
    if (nrow(dose_response) < 5) { error_log[[i]] <- tibble(population = sid, drug = drg, error = "< 5 obs"); next }

    start_vals <- list(d = max(dose_response$OD), b = 10, e = median(dose_response$concentration))
    bounds_lo  <- c(d = 0,    b = 0.01, e = min(dose_response$concentration))
    bounds_hi  <- c(d = 2,    b = 50,   e = max(dose_response$concentration) * 5)

    # Use drm package which handles this better
    fit <- tryCatch(
      suppressWarnings(drm(
        OD ~ concentration, 
        data = dose_response,
        fct = LL.4(names = c("Slope", "Lower", "Upper", "EC50"))
      )),
      error = function(e) {
        error_log[[i]] <<- tibble(population = sid, drug = drg, error = e$message)
        NULL
      }
    )
    if (is.null(fit)) next

    # Extract parameters from drm fit
    # LL.4: Lower, Upper, Slope, EC50
    parms <- coef(fit)
    names(parms) <- c("Lower", "Upper", "Slope", "EC50")
    
    # Parametric bootstrap: resample residuals and refit
    residuals_orig <- residuals(fit)
    boot_params_list <- vector("list", R)
    
    for (r in seq_len(R)) {
      # Resample residuals
      residuals_boot <- sample(residuals_orig, replace = TRUE)
      dose_response_boot <- dose_response
      dose_response_boot$OD <- predict(fit) + residuals_boot
      
      # Refit
      fit_boot <- tryCatch(
        suppressWarnings(drm(OD ~ concentration, data = dose_response_boot, fct = LL.4(names = c("Slope", "Lower", "Upper", "EC50")))),
        error = function(e) NULL
      )
      
      if (!is.null(fit_boot)) {
        boot_params_list[[r]] <- coef(fit_boot)
      }
    }
    
    boot_params <- do.call(rbind, boot_params_list)
    colnames(boot_params) <- c("Slope", "Lower", "Upper", "EC50")
    ic50_vals   <- boot_params[, "EC50"]

    ic50_list[[i]] <- tibble(
      population        = sid,
      evolution_history = evo,
      drug              = drg,
      month             = mth,
      IC50              = parms[["EC50"]],
      IC50_lower        = quantile(ic50_vals, 0.025, na.rm = TRUE),
      IC50_upper        = quantile(ic50_vals, 0.975, na.rm = TRUE),
      d                 = parms[["Upper"]],
      b                 = parms[["Slope"]],
      b_hit_upper       = parms[["Slope"]] >= 49,
      n_boot            = nrow(boot_params)
    )

    grid <- tibble(concentration = exp(seq(log(min(dose_response$concentration)),
                                           log(max(dose_response$concentration)),
                                           length.out = 100)))
    # Use drm predict
    grid$fit     <- predict(fit, newdata = grid)[, 1]
    
    # Bootstrap predictions using LL.4 formula: d + (c-d) / (1 + exp(b * (log(conc) - log(e))))
    # Parameters order: Slope, Lower, Upper, EC50
    pred_matrix  <- apply(boot_params, 1, \(p) {
      p["Upper"] + (p["Lower"] - p["Upper"]) / (1 + exp(p["Slope"] * (log(grid$concentration) - log(p["EC50"]))))
    })
    grid$lower   <- apply(pred_matrix, 1, quantile, probs = 0.025, na.rm = TRUE)
    grid$upper   <- apply(pred_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)
    grid$population        <- sid
    grid$evolution_history <- evo
    grid$drug              <- drg
    grid$month             <- mth
    pred_list[[i]] <- grid
  }

  list(
    ic50_table = bind_rows(ic50_list),
    pred_curves = bind_rows(pred_list),
    errors      = bind_rows(error_log)
  )
}

set.seed(8214)
ic50_results <- fit_ic50(mic_data, R = 1000)

# Check for errors and fits hitting the upper bound on b (slope)
cat("Errors:\n");   print(ic50_results$errors)
cat("\nFits hitting b upper bound (b ≥ 49):\n")
ic50_results$ic50_table |> filter(b_hit_upper) |> select(population, drug, b, IC50)

write_csv(ic50_results$ic50_table,  "data-processed/ic50-table-june.csv")
write_csv(ic50_results$pred_curves, "data-processed/ic50-pred-curves-june.csv")



ic50s <- ic50_results$ic50_table







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



### note that this boot file is missing a bunch of fits, presumably because they failed at the fitting step (November 25 come back to this)


tolerances <- results$ic50_table

write_csv(tolerances, "data-processed/tolerances.csv")

tolerances <- read_csv("data-processed/tolerances.csv")

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
