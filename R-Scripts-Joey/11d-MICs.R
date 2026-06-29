
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



a2 <- mic_data %>%
  # separate(sheet_name, into = c("set", "rep", "drug_type"), sep = "_", remove = FALSE) %>% 
  unite("pop_rep", population, rep, set, drug, month, sep = "_", remove = FALSE)

# 3-parameter logistic: OD = d / (1 + exp(b * (log(conc) - log(IC50))))
# Fit per strain × drug, pooling all replicates and sets


# boostrapping function ---------------------------------------------------



fit_bootstrap_ic50 <- function(data, group_var = "pop_rep", R = 1000, seed = 123) {
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


results <- fit_bootstrap_ic50(a2) ### come back here to make sure none of these are hitting against the bounds

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
  mutate(month = case_when(grepl("February", pop_rep) ~ "February",
                                       grepl("March", pop_rep) ~ "March",
                                       grepl("January", pop_rep) ~ "January")) %>% 
  mutate(drug = case_when(grepl("amph", pop_rep) ~ "amphotericin",
                          grepl("fluc", pop_rep) ~ "fluconazole",
                          grepl("casp", pop_rep) ~ "caspofungin"))

ggplot() +
  geom_pointrange(aes(x = pop_rep, y = mean_b, ymin = lower_b, ymax = upper_b, color = evolution_history), data = b2) +
  facet_wrap( ~ drug)
ggsave("figures/mics-pointrange-all-drugs.png", width = 12, height = 5)


e2 <- boot_params1 %>%
  mutate(drug = case_when(grepl("amph", pop_rep) ~ "amphotericin",
                          grepl("fluc", pop_rep) ~ "fluconazole",
                          grepl("casp", pop_rep) ~ "caspofungin")) %>% 
  group_by(pop_rep, drug) %>% 
  summarise(mean_e = mean(e),
            upper_e = quantile(e, 0.975),
            lower_e = quantile(e, 0.025)) %>% 
  mutate(evolution_history = case_when(grepl(40, pop_rep) ~ "evolved 40",
                                       grepl(35, pop_rep) ~ "evolved 35",
                                       grepl("fR", pop_rep) ~ "fRS585")) |> 
  mutate(month = case_when(grepl("February", pop_rep) ~ "February",
                           grepl("March", pop_rep) ~ "March",
                           grepl("January", pop_rep) ~ "January")) 
  


ggplot() +
  geom_pointrange(aes(x = pop_rep, y = mean_e, ymin = lower_e, ymax = upper_e, color = evolution_history), data = e2) +
  facet_wrap( ~ drug, scales = "free")
ggsave("figures/mics-pointrange-e-all-drugs.png", width = 12, height = 5)



# Check for errors and fits hitting the upper bound on b (slope)
cat("Errors:\n");   print(results$errors)
cat("\nFits hitting b upper bound (b ≥ 49):\n")
results$ic50_table |> filter(b_hit_upper) |> select(population, drug, b, IC50)

write_csv(results$ic50_table,  "data-processed/ic50-table-june.csv")
# write_csv(results$pred_curves, "data-processed/ic50-pred-curves-june.csv")


a4 <- a2 |> 
  select(pop_rep, population) |> 
  distinct()
e3 <- e2 |> 
  left_join(a4)


amph <- e3 |> 
  filter(drug == "amphotericin") 


amph |> 
  ggplot(aes( x = evolution_history, y = mean_e, color = month)) + geom_point()

mod1 <- lm(mean_e ~ evolution_history + month, data = amph)
summary(mod1)
library(visreg)
visreg(mod1)



library(nlme)
model1 <- lme(mean_e ~  evolution_history,
              random = ~ 1 | month,
              data = amph)
summary(model1)
