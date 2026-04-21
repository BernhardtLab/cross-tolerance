### Growth rate estimation and TPC fitting (excluding lagsat model)
###
### Description: Estimates per-well growth rates using get.growth.rate() from growthTools,
###              excluding the lag.sat model. Fits Sharpe-Schoolfield thermal performance curves
###              to each strain and extracts Topt and Ctmax parameters.
###
### Input: "data-processed/all-blocks-tpc-experiment.csv"
### Output: "data-processed/tpc-parameters-by-strain-no-lagsat.csv"
###         figures/growth-tools-figures-no-lagsat/
###         figures/topt-ctmax-*-no-lagsat.png

library(tidyverse)
library(growthTools)
library(glue)
library(rTPC)
library(nls.multstart)
library(broom)
library(car)

# Load and prepare data -------------------------------------------------------

edata <- read_csv("data-processed/all-blocks-tpc-experiment.csv") |> 
  filter(evolution_history %in% c("35 evolved", "40 evolved", "fRS585")) |>
  mutate(rep.id = paste(well, block, test_temperature, strain, evolution_history, sep = "_"))

# Create output directory structure for figures 
growth_results_list <- list()

# Group by block and temperature, then fit within each group
unique_combos <- edata |>
  distinct(block, test_temperature) |>
  arrange(block, test_temperature)

cat("Estimating growth rates (excluding lagsat model)...\n\n")

for (i in seq_len(nrow(unique_combos))) {
  block_i <- unique_combos$block[i]
  temp_i <- unique_combos$test_temperature[i]
  
  # Create subdirectory for this block/temperature combination
  grow_fig_path <- glue("figures/growth-tools-figures-no-lagsat/block{block_i}_{temp_i}C")
  dir.create(grow_fig_path, recursive = TRUE, showWarnings = FALSE)
  
  # Filter to this block/temp combination
  data_subset <- edata |>
    filter(block == block_i, test_temperature == temp_i)
  
  # Fit growth rates for this subset
  # Note: methods = c("sat", "flr") excludes lagsat
  growth_results_list[[i]] <- data_subset |>
    mutate(ln_abundance = log(od)) |>
    group_by(rep.id) |>
    do(grs = get.growth.rate(
      x           = .$days,
      y           = .$ln_abundance,
      id          = unique(.$well),
      plot.best.Q = TRUE,
      methods     = c("sat", "flr"),
      fpath       = grow_fig_path
    ))
  
  cat(sprintf("Completed block %d, %dC (%d/%d)\n", 
              block_i, temp_i, i, nrow(unique_combos)))
}

# Combine all results
growth_results <- bind_rows(growth_results_list)

# Extract and parse growth rate summaries -------------------------------------

growth_summary_fixed <- growth_results |>
  summarise(
    rep.id,
    mu         = grs$best.slope,
    best_model = grs$best.model,
    se         = grs$best.se,
    R2         = grs$best.model.rsqr,
    n_obs      = grs$best.model.slope.n
  ) |>
  separate(rep.id, 
           into = c("well", "block", "test_temperature", "lineage", "strain_id", "evolution_history"),
           sep = "_", 
           extra = "merge",
           remove = FALSE) |>
  mutate(
    strain = paste(lineage, strain_id, sep = "_")
  ) |>
  select(rep.id, well, block, test_temperature, lineage, strain_id, strain, 
         evolution_history, mu, best_model, se, R2, n_obs)

cat("\nGrowth rates estimated for", nrow(growth_summary_fixed), "replicates\n\n")

# Fit Sharpe-Schoolfield thermal performance curves ---------------------------

cat("Fitting Sharpe-Schoolfield TPC models...\n\n")

# Prepare data for TPC fitting
growth_for_tpc <- growth_summary_fixed |>
  mutate(
    test_temperature = as.numeric(test_temperature),
    temp = test_temperature,
    rate = mu
  ) |>
  filter(!is.na(mu), !is.na(temp)) |>
  select(strain, temp, rate)

# Function to fit TPC model to a single strain
fit_tpc_strain <- function(strain_data, strain_name) {
  
  if (nrow(strain_data) < 4) {
    warning(sprintf("Strain %s has only %d observations", strain_name, nrow(strain_data)))
    return(NULL)
  }
  
  # Get start values and limits
  start_vals <- tryCatch(
    get_start_vals(strain_data$temp, strain_data$rate, model_name = 'sharpeschoolhigh_1981'),
    error = function(e) return(NULL)
  )
  
  if (is.null(start_vals)) {
    warning(sprintf("Could not get start values for strain %s", strain_name))
    return(NULL)
  }
  
  low_lims <- get_lower_lims(strain_data$temp, strain_data$rate, model_name = 'sharpeschoolhigh_1981')
  upper_lims <- get_upper_lims(strain_data$temp, strain_data$rate, model_name = 'sharpeschoolhigh_1981')
  
  # Fit the model
  fit <- tryCatch(
    nls_multstart(
      rate ~ sharpeschoolhigh_1981(temp = temp, r_tref, e, eh, th, tref = 15),
      data = strain_data,
      iter = 500,
      start_lower = start_vals - 10,
      start_upper = start_vals + 10,
      lower = low_lims,
      upper = upper_lims,
      supp_errors = 'Y'
    ),
    error = function(e) {
      warning(sprintf("Model fitting failed for strain %s: %s", strain_name, e$message))
      return(NULL)
    }
  )
  
  if (is.null(fit)) return(NULL)
  
  # Extract parameters
  params <- calc_params(fit)
  
  return(list(
    fit = fit,
    params = params,
    strain = strain_name,
    n_obs = nrow(strain_data)
  ))
}

# Fit models for each strain
unique_strains <- growth_for_tpc |> distinct(strain) |> pull(strain)

tpc_fits <- list()
for (strain in unique_strains) {
  strain_data <- growth_for_tpc |> filter(strain == !!strain)
  tpc_fits[[strain]] <- fit_tpc_strain(strain_data, strain)
  
  if (!is.null(tpc_fits[[strain]])) {
    cat(sprintf("✓ %s (n=%d obs)\n", strain, tpc_fits[[strain]]$n_obs))
  } else {
    cat(sprintf("✗ %s - fitting failed\n", strain))
  }
}

cat("\nSuccessfully fitted", sum(!sapply(tpc_fits, is.null)), "out of", length(unique_strains), "strains\n\n")

# Extract thermal performance parameters ------------------------------------

tpc_results <- tibble()

for (strain in names(tpc_fits)) {
  if (!is.null(tpc_fits[[strain]])) {
    params <- tpc_fits[[strain]]$params
    
    tpc_results <- tpc_results |>
      bind_rows(
        params |>
          mutate(strain = strain) |>
          select(strain, rmax, topt, ctmax)
      )
  }
}

# Join back with strain metadata
tpc_results_full <- tpc_results |>
  left_join(
    growth_summary_fixed |>
      distinct(strain, lineage, strain_id, evolution_history) |>
      mutate(lineage_numeric = as.numeric(lineage)),
    by = "strain"
  ) |>
  arrange(lineage_numeric, strain_id)

cat("=== Thermal Performance Parameters (Sharpe-Schoolfield Model) ===\n\n")
print(tpc_results_full, n = Inf)

# Save results
write_csv(tpc_results_full, "data-processed/tpc-parameters-by-strain-no-lagsat.csv")
cat("\n\nFull TPC parameters saved to: data-processed/tpc-parameters-by-strain-no-lagsat.csv\n")

# Statistical comparison: 35°C vs 40°C evolved strains ------------------------

cat("\n\n=== Statistical Comparison: 35°C vs 40°C Evolved Strains ===\n\n")

tpc_comparison <- tpc_results_full |>
  filter(lineage %in% c("35", "40")) |>
  mutate(lineage = factor(lineage, levels = c("35", "40")))

# Descriptive statistics
cat("Descriptive Statistics:\n\n")
tpc_comparison |>
  group_by(lineage) |>
  summarise(
    n = n(),
    topt_mean = mean(topt),
    topt_sd = sd(topt),
    topt_se = sd(topt) / sqrt(n()),
    ctmax_mean = mean(ctmax),
    ctmax_sd = sd(ctmax),
    ctmax_se = sd(ctmax) / sqrt(n()),
    .groups = 'drop'
  ) |>
  print()

# Extract vectors for testing
topt_35 <- tpc_comparison |> filter(lineage == "35") |> pull(topt)
topt_40 <- tpc_comparison |> filter(lineage == "40") |> pull(topt)
ctmax_35 <- tpc_comparison |> filter(lineage == "35") |> pull(ctmax)
ctmax_40 <- tpc_comparison |> filter(lineage == "40") |> pull(ctmax)

# T-tests
cat("\n\n=== T-tests ===\n\n")

cat("Topt difference (35°C vs 40°C evolved):\n")
ttest_topt <- t.test(topt_35, topt_40, var.equal = TRUE)
print(ttest_topt)
cat("Effect size (Cohen's d):", 
    (mean(topt_40) - mean(topt_35)) / sqrt(((length(topt_35)-1)*sd(topt_35)^2 + 
                                             (length(topt_40)-1)*sd(topt_40)^2) / 
                                            (length(topt_35) + length(topt_40) - 2)), "\n\n")

cat("Ctmax difference (35°C vs 40°C evolved):\n")
ttest_ctmax <- t.test(ctmax_35, ctmax_40, var.equal = TRUE)
print(ttest_ctmax)
cat("Effect size (Cohen's d):", 
    (mean(ctmax_40) - mean(ctmax_35)) / sqrt(((length(ctmax_35)-1)*sd(ctmax_35)^2 + 
                                              (length(ctmax_40)-1)*sd(ctmax_40)^2) / 
                                             (length(ctmax_35) + length(ctmax_40) - 2)), "\n\n")

# Mann-Whitney U tests
cat("\n=== Mann-Whitney U Tests ===\n\n")

cat("Topt difference (35°C vs 40°C evolved):\n")
mw_topt <- wilcox.test(topt_35, topt_40)
print(mw_topt)

cat("\nCtmax difference (35°C vs 40°C evolved):\n")
mw_ctmax <- wilcox.test(ctmax_35, ctmax_40)
print(mw_ctmax)

# Visualizations -------------------------------------------------------

cat("\n\nCreating visualizations...\n")

# Plot 1: Topt by evolution history
ggplot(tpc_results_full, aes(x = lineage, y = topt, fill = lineage)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  theme_minimal(base_size = 12) +
  labs(title = "Thermal Optimum (Topt) by Evolution History",
       x = "Evolution History",
       y = "Topt (°C)",
       fill = "Evolution History",
       subtitle = "Growth rates estimated without lagsat model") +
  theme(legend.position = "none")

ggsave("figures/topt-by-evolution-history-no-lagsat.png", width = 6, height = 5)

# Plot 2: Ctmax by evolution history
ggplot(tpc_results_full, aes(x = lineage, y = ctmax, fill = lineage)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  theme_minimal(base_size = 12) +
  labs(title = "Critical Thermal Maximum (Ctmax) by Evolution History",
       x = "Evolution History",
       y = "Ctmax (°C)",
       fill = "Evolution History",
       subtitle = "Growth rates estimated without lagsat model") +
  theme(legend.position = "none")

ggsave("figures/ctmax-by-evolution-history-no-lagsat.png", width = 6, height = 5)

# Plot 3: Topt vs Ctmax scatter
ggplot(tpc_results_full, aes(x = topt, y = ctmax, color = lineage, shape = lineage)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal(base_size = 12) +
  labs(title = "Relationship between Topt and Ctmax",
       x = "Topt (°C)",
       y = "Ctmax (°C)",
       color = "Evolution History",
       shape = "Evolution History",
       subtitle = "Growth rates estimated without lagsat model")

ggsave("figures/topt-vs-ctmax-no-lagsat.png", width = 7, height = 5)

# Plot 4: Statistical comparison
plot_data <- tpc_comparison |>
  pivot_longer(cols = c(topt, ctmax), names_to = "parameter", values_to = "value") |>
  mutate(parameter = case_when(
    parameter == "topt" ~ "Topt (°C)",
    parameter == "ctmax" ~ "Ctmax (°C)"
  ))

ggplot(plot_data, aes(x = lineage, y = value, fill = lineage)) +
  geom_boxplot(alpha = 0.6, width = 0.5) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.5, color = "black") +
  facet_wrap(~parameter, scales = "free_y", nrow = 1) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 13, face = "bold")
  ) +
  labs(
    title = "Thermal Performance Parameters: 35°C vs 40°C Evolved Strains",
    x = "Evolution History",
    y = "Parameter Value",
    fill = "Lineage",
    subtitle = "Growth rates estimated without lagsat model"
  )

ggsave("figures/topt-ctmax-statistical-comparison-no-lagsat.png", width = 9, height = 5)

cat("Visualizations saved!\n")

cat("\n\n=== ANALYSIS COMPLETE ===\n")
