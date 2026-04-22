# =============================================================================
# Compare Growth Rate Methods and Fit Sharpe-Schoolfield TPC Models
#
# This script:
# 1. Compares growth rates from growthTools vs 3-parameter logistic
# 2. Fits Sharpe-Schoolfield High model to both datasets
# 3. Extracts Topt and CTmax for each method
# 4. Tests if evolution history effects differ by estimation method
# =============================================================================

library(tidyverse)
library(minpack.lm)
library(plotrix)
library(broom)

# Configuration ---------------------------------------------------------------

K_B <- 8.617333e-5      # Boltzmann constant (eV K⁻¹)
TREF_K <- 288.15        # Reference temperature (15°C in Kelvin)
CTMAX_CAP <- 50.0       # Cap for ctmax if estimation fails
N_STARTS <- 500         # Number of random starting points for multistart fitting

set.seed(42)

OUT <- "data-processed/tpc-comparison"
dir.create(OUT, showWarnings = FALSE)

EVO_COLORS <- c(
  "40 evolved" = "#FA3208",
  "35 evolved" = "#0E63FF",
  "fRS585"     = "#000000"
)

# =============================================================================
# 1. Load both growth rate datasets
# =============================================================================

# Growth rates from growthTools package
gr_growthtools <- read_csv("data-processed/growth-rates-growth-tools.csv") |>
  select(rep.id, well, block, test_temperature, strain, evolution_history, mu, R2) |>
  rename(mu_gt = mu, r2_gt = R2)

# Growth rates from 3-parameter logistic
gr_logistic <- read_csv("data-processed/growth-rates-logistic.csv") |>
  select(rep.id, well, block, test_temperature, strain, evolution_history, mu, r2) |>
  rename(mu_log = mu, r2_log = r2)

# Merge by well/block/temperature/strain/evolution_history
gr_comparison <- full_join(
  gr_growthtools |> select(well, block, test_temperature, strain, evolution_history, mu_gt, r2_gt),
  gr_logistic |> select(well, block, test_temperature, strain, evolution_history, mu_log, r2_log),
  by = c("well", "block", "test_temperature", "strain", "evolution_history")
)

cat(sprintf("Growth rate comparison:\n"))
cat(sprintf("Total rows: %d\n", nrow(gr_comparison)))
cat(sprintf("Rows with both methods: %d\n", sum(!is.na(gr_comparison$mu_gt) & !is.na(gr_comparison$mu_log))))

# =============================================================================
# 2. Compare methods visually
# =============================================================================

# Scatterplot of growth rates from both methods
p_compare <- gr_comparison |>
  filter(!is.na(mu_gt), !is.na(mu_log)) |>
  ggplot(aes(x = mu_gt, y = mu_log, color = evolution_history)) +
  geom_point(size = 2.5, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50", linewidth = 1) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x = "Growth Rate from growthTools (day⁻¹)",
    y = "Growth Rate from 3-parameter Logistic (day⁻¹)",
    title = "Comparison of Growth Rate Estimation Methods"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(OUT, "01-method-comparison.png"), p_compare,
       width = 7, height = 6, dpi = 300)
print(p_compare)

# Calculate correlation by temperature
corr_by_temp <- gr_comparison |>
  filter(!is.na(mu_gt), !is.na(mu_log)) |>
  group_by(test_temperature) |>
  summarise(
    n = n(),
    correlation = cor(mu_gt, mu_log, method = "pearson"),
    rmse = sqrt(mean((mu_gt - mu_log)^2)),
    .groups = "drop"
  )

cat("\nCorrelation by temperature:\n")
print(corr_by_temp)

# =============================================================================
# 3. Helper functions for TPC fitting
# =============================================================================

sharpeschoolhigh <- function(T_c, r_tref, e, eh, th) {
  T_k         <- T_c + 273.15
  boltz_act   <- (e  / K_B) * (1 / TREF_K - 1 / T_k)
  boltz_deact <- (eh / K_B) * (1 / th     - 1 / T_k)
  r_tref * exp(boltz_act) / (1 + exp(boltz_deact))
}

calc_topt_ctmax <- function(params, T_range = NULL) {
  if (is.null(T_range)) T_range <- seq(5, 55, length.out = 5000)
  preds <- sharpeschoolhigh(T_range, params[1], params[2], params[3], params[4])
  valid <- is.finite(preds)
  if (!any(valid)) return(c(topt = NA, ctmax = NA, rmax = NA))
  
  topt_idx <- which.max(preds[valid])
  topt     <- T_range[valid][topt_idx]
  rmax     <- preds[valid][topt_idx]
  
  # Try zero-crossing above Topt
  pa          <- preds
  pa[T_range <= topt] <- rmax
  sign_changes <- which(diff(sign(pa)) != 0)
  
  if (length(sign_changes) > 0) {
    i  <- sign_changes[1]
    t1 <- T_range[i];     t2 <- T_range[i + 1]
    p1 <- pa[i];          p2 <- pa[i + 1]
    ctmax <- if (p2 != p1) t1 - p1 * (t2 - t1) / (p2 - p1) else t1
  } else {
    # Operational CT_max: first T above Topt where predicted rate < 5% of rmax
    below <- which(T_range > topt & preds < 0.05 * rmax)
    ctmax <- if (length(below) > 0) T_range[below[1]] else CTMAX_CAP
  }
  
  c(topt = topt, ctmax = ctmax, rmax = rmax)
}

fit_tpc_multistart <- function(temps, rates) {
  # Multistart bounds
  BOUNDS_LO <- c(r_tref = 0.01, e = 0.01, eh =  0.5, th = 303.0)
  BOUNDS_HI <- c(r_tref = 50.0, e =  3.0, eh = 50.0, th = 335.0)
  START_LO  <- c(r_tref =  0.5, e =  0.1, eh =  0.5, th = 305.0)
  START_HI  <- c(r_tref = 20.0, e =  2.0, eh = 20.0, th = 325.0)
  
  best_popt <- NULL
  best_rss  <- Inf
  
  # Generate random starts
  starts <- mapply(function(...) c(...),
                   runif(N_STARTS, START_LO["r_tref"], START_HI["r_tref"]),
                   runif(N_STARTS, START_LO["e"],      START_HI["e"]),
                   runif(N_STARTS, START_LO["eh"],     START_HI["eh"]),
                   runif(N_STARTS, START_LO["th"],     START_HI["th"]),
                   SIMPLIFY = FALSE)
  
  for (p0 in starts) {
    tryCatch({
      fit <- suppressWarnings(nlsLM(
        rates ~ sharpeschoolhigh(temps, r_tref, e, eh, th),
        start  = list(r_tref = p0[1], e = p0[2], eh = p0[3], th = p0[4]),
        lower  = BOUNDS_LO,
        upper  = BOUNDS_HI,
        control = nls.lm.control(maxiter = 200, maxfev = 5000)
      ))
      rss <- sum(residuals(fit)^2)
      if (rss < best_rss) {
        best_rss  <- rss
        best_popt <- coef(fit)
      }
    }, error = function(e) NULL)
  }
  
  if (is.null(best_popt)) return(NULL)
  
  ss_tot <- sum((rates - mean(rates))^2)
  r2     <- if (ss_tot > 0) 1 - best_rss / ss_tot else NA_real_
  
  derived <- calc_topt_ctmax(best_popt)
  
  list(
    params = best_popt,
    topt = derived["topt"],
    ctmax = derived["ctmax"],
    rmax = derived["rmax"],
    r2 = r2
  )
}

# =============================================================================
# 4. Fit TPC to growthTools data
# =============================================================================

cat("\n=== Fitting TPC to growthTools data ===\n")

gr_gt_by_strain <- gr_comparison |>
  select(strain, evolution_history, test_temperature, mu_gt) |>
  filter(!is.na(mu_gt)) |>
  group_by(strain, evolution_history) |>
  summarise(
    temps = list(as.numeric(test_temperature)),
    rates = list(as.numeric(mu_gt)),
    n_temps = n(),
    .groups = "drop"
  ) |>
  filter(n_temps >= 3)

cat(sprintf("Strains with ≥3 temperature points: %d\n", nrow(gr_gt_by_strain)))

tpc_gt_list <- list()
for (i in seq_len(nrow(gr_gt_by_strain))) {
  row <- gr_gt_by_strain[i, ]
  fit_result <- fit_tpc_multistart(unlist(row$temps), unlist(row$rates))
  
  if (!is.null(fit_result)) {
    tpc_gt_list[[i]] <- data.frame(
      strain = row$strain,
      evo_history = row$evolution_history,
      method = "growthTools",
      r_tref = fit_result$params[1],
      e = fit_result$params[2],
      eh = fit_result$params[3],
      th = fit_result$params[4],
      topt = fit_result$topt,
      ctmax = fit_result$ctmax,
      rmax = fit_result$rmax,
      r2 = fit_result$r2,
      row.names = NULL
    )
  }
  
  if (i %% 10 == 0) cat(sprintf("  Completed %d/%d\n", i, nrow(gr_gt_by_strain)))
}

tpc_gt <- bind_rows(tpc_gt_list)
cat(sprintf("Successfully fit %d strains\n", nrow(tpc_gt)))

# =============================================================================
# 5. Fit TPC to logistic data
# =============================================================================

cat("\n=== Fitting TPC to logistic data ===\n")

gr_log_by_strain <- gr_comparison |>
  select(strain, evolution_history, test_temperature, mu_log) |>
  filter(!is.na(mu_log)) |>
  group_by(strain, evolution_history) |>
  summarise(
    temps = list(as.numeric(test_temperature)),
    rates = list(as.numeric(mu_log)),
    n_temps = n(),
    .groups = "drop"
  ) |>
  filter(n_temps >= 3)

cat(sprintf("Strains with ≥3 temperature points: %d\n", nrow(gr_log_by_strain)))

tpc_log_list <- list()
for (i in seq_len(nrow(gr_log_by_strain))) {
  row <- gr_log_by_strain[i, ]
  fit_result <- fit_tpc_multistart(unlist(row$temps), unlist(row$rates))
  
  if (!is.null(fit_result)) {
    tpc_log_list[[i]] <- data.frame(
      strain = row$strain,
      evo_history = row$evolution_history,
      method = "logistic",
      r_tref = fit_result$params[1],
      e = fit_result$params[2],
      eh = fit_result$params[3],
      th = fit_result$params[4],
      topt = fit_result$topt,
      ctmax = fit_result$ctmax,
      rmax = fit_result$rmax,
      r2 = fit_result$r2,
      row.names = NULL
    )
  }
  
  if (i %% 10 == 0) cat(sprintf("  Completed %d/%d\n", i, nrow(gr_log_by_strain)))
}

tpc_log <- bind_rows(tpc_log_list)
cat(sprintf("Successfully fit %d strains\n", nrow(tpc_log)))

# =============================================================================
# 6. Clean and standardize TPC results
# =============================================================================

tpc_gt_clean <- tpc_gt |>
  mutate(
    strain_temp = evo_history,
    evolution_history = case_when(
      strain == "35" ~ "35 evolved",
      strain == "40" ~ "40 evolved",
      strain == "fRS585" ~ "fRS585",
      TRUE ~ strain
    ),
    strain = strain_temp
  ) |>
  select(strain, evolution_history, method, r_tref, e, eh, th, topt, ctmax, rmax, r2)

tpc_log_clean <- tpc_log |>
  mutate(
    strain = sub("^[0-9]+_", "", strain),
    evolution_history = evo_history
  ) |>
  select(strain, evolution_history, method, r_tref, e, eh, th, topt, ctmax, rmax, r2)

tpc_all_clean <- bind_rows(tpc_gt_clean, tpc_log_clean)

# =============================================================================
# 7. Summary statistics
# =============================================================================

cat("\n=== Thermal Traits by Method and Evolution History ===\n")
summary_traits <- tpc_all_clean |>
  group_by(method, evolution_history) |>
  summarise(
    n = n(),
    mean_topt = round(mean(topt, na.rm = TRUE), 2),
    sd_topt = round(sd(topt, na.rm = TRUE), 2),
    mean_ctmax = round(mean(ctmax, na.rm = TRUE), 2),
    sd_ctmax = round(sd(ctmax, na.rm = TRUE), 2),
    mean_r2 = round(mean(r2, na.rm = TRUE), 3),
    .groups = "drop"
  ) |>
  arrange(evolution_history, method)

print(summary_traits)

# =============================================================================
# 8. Statistical analysis: Evolution history effect on Topt
# =============================================================================

cat("\n=== EFFECT OF EVOLUTION HISTORY ON TOPT ===\n")

cat("\nGrowthTools method:\n")
lm_topt_gt <- lm(topt ~ evolution_history, data = tpc_gt_clean)
print(summary(lm_topt_gt))

cat("\nLogistic method:\n")
lm_topt_log <- lm(topt ~ evolution_history, data = tpc_log_clean)
print(summary(lm_topt_log))

# =============================================================================
# 9. Statistical analysis: Evolution history effect on CTmax
# =============================================================================

cat("\n=== EFFECT OF EVOLUTION HISTORY ON CTMAX ===\n")

cat("\nGrowthTools method:\n")
lm_ctmax_gt <- lm(ctmax ~ evolution_history, data = tpc_gt_clean)
print(summary(lm_ctmax_gt))

cat("\nLogistic method:\n")
lm_ctmax_log <- lm(ctmax ~ evolution_history, data = tpc_log_clean)
print(summary(lm_ctmax_log))

# =============================================================================
# 10. Visualizations: Topt comparison
# =============================================================================

p_topt_compare <- tpc_all_clean |>
  ggplot(aes(x = evolution_history, y = topt, color = method, fill = method)) +
  geom_point(position = position_dodge(width = 0.3), size = 2.5, alpha = 0.7) +
  geom_point(
    data = summary_traits |> select(method, evolution_history, mean_topt) |> rename(topt = mean_topt),
    position = position_dodge(width = 0.3),
    size = 4,
    shape = 5,
    stroke = 1.5,
    aes(color = method),
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("growthTools" = "#1f77b4", "logistic" = "#ff7f0e"), name = NULL) +
  labs(
    x = NULL,
    y = "Optimal Temperature Topt (°C)",
    title = "Thermal Optimum by Estimation Method"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(OUT, "02-topt-comparison.png"), p_topt_compare,
       width = 7, height = 6, dpi = 300)
print(p_topt_compare)

# =============================================================================
# 11. Visualizations: CTmax comparison
# =============================================================================

p_ctmax_compare <- tpc_all_clean |>
  ggplot(aes(x = evolution_history, y = ctmax, color = method, fill = method)) +
  geom_point(position = position_dodge(width = 0.3), size = 2.5, alpha = 0.7) +
  geom_point(
    data = summary_traits |> select(method, evolution_history, mean_ctmax) |> rename(ctmax = mean_ctmax),
    position = position_dodge(width = 0.3),
    size = 4,
    shape = 5,
    stroke = 1.5,
    aes(color = method),
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("growthTools" = "#1f77b4", "logistic" = "#ff7f0e"), name = NULL) +
  labs(
    x = NULL,
    y = "Critical Thermal Maximum CTmax (°C)",
    title = "Thermal Tolerance by Estimation Method"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(OUT, "03-ctmax-comparison.png"), p_ctmax_compare,
       width = 7, height = 6, dpi = 300)
print(p_ctmax_compare)

# =============================================================================
# 12. Export results
# =============================================================================

write_csv(gr_comparison, file.path(OUT, "growth-rates-comparison.csv"))
write_csv(tpc_all_clean, file.path(OUT, "thermal-traits-all-methods.csv"))
write_csv(tpc_gt_clean, file.path(OUT, "thermal-traits-growthtools.csv"))
write_csv(tpc_log_clean, file.path(OUT, "thermal-traits-logistic.csv"))
write_csv(summary_traits, file.path(OUT, "summary-thermal-traits.csv"))

cat(sprintf("\nExported to %s/\n", OUT))
for (f in list.files(OUT, full.names = FALSE)) {
  cat(sprintf("  %s\n", f))
}
