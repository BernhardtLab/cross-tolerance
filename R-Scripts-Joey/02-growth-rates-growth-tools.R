library(tidyverse)
library(growthTools)

# Load and prepare data -------------------------------------------------------

edata <- read_csv("data-processed/all-blocks-tpc-experiment.csv") |> 
  filter(evolution_history %in% c("35 evolved", "40 evolved", "fRS585")) |>
  mutate(rep.id = paste(well, block, test_temperature, strain, evolution_history, sep = "_"))

# Create output directory structure for figures 
# Organize by block and temperature for easier navigation

growth_results_list <- list()

# Group by block and temperature, then fit within each group
unique_combos <- edata |>
  distinct(block, test_temperature)

for (i in seq_len(nrow(unique_combos))) {
  block_i <- unique_combos$block[i]
  temp_i <- unique_combos$test_temperature[i]
  
  # Create subdirectory for this block/temperature combination
  grow_fig_path <- glue::glue("figures/growth-tools-figures/block{block_i}_{temp_i}C")
  dir.create(grow_fig_path, recursive = TRUE, showWarnings = FALSE)
  
  # Filter to this block/temp combination
  data_subset <- edata |>
    filter(block == block_i, test_temperature == temp_i)
  
  # Fit growth rates for this subset
  growth_results_list[[i]] <- data_subset |>
    mutate(ln_abundance = log(od)) |>
    group_by(rep.id) |>
    do(grs = get.growth.rate(
      x           = .$days,
      y           = .$ln_abundance,
      id          = unique(.$well),
      plot.best.Q = TRUE,
      methods     = c("sat", "flr", "lagsat"),
      fpath       = grow_fig_path
    ))
  
  cat(sprintf("Completed block %d, %dC (%d/%d)\n", 
              block_i, temp_i, i, nrow(unique_combos)))
}

# Combine all results
growth_results <- bind_rows(growth_results_list)

# Extract growth rate summaries -----------------------------------------------

growth_summary <- growth_results |>
  summarise(
    rep.id,
    mu         = grs$best.slope,
    best_model = grs$best.model,
    se         = grs$best.se,
    R2         = grs$best.model.rsqr,
    n_obs      = grs$best.model.slope.n
  ) |>
  separate(rep.id, 
           into = c("well", "block", "test_temperature", "strain", "evolution_history"),
           sep = "_", remove = FALSE)

# Summary output
cat("\n=== Growth Rate Summary ===\n")
print(growth_summary)

# Check how many figures were saved
total_figs <- length(list.files("figures/growth-tools-figures", recursive = TRUE))
cat("\nTotal figures saved to figures/growth-tools-figures:", total_figs, "\n")

write_csv(growth_summary, "data-processed/growth-rates-growth-tools.csv")
