
# =============================================================================
# MIC / IC50 analysis — point estimates only (no bootstrapping)
# =============================================================================
# Goal: fit one dose-response curve per population x drug x month (pooling
# all replicate wells -- and, for fRS585, all sets -- as multiple OD points
# at each concentration), then ask whether evolution history (35 evolved /
# 40 evolved / fRS585 ancestor) predicts IC50, fit separately per drug, with
# month (block) as a fixed effect where needed.
#
# Design decisions locked in with Joey before writing this script:
#   1. Comparison models are fit SEPARATELY per drug (not pooled across drugs).
#   2. Month (block) enters as a FIXED effect, not a random intercept — there
#      are only 2-3 month levels per drug, too few to estimate a random-effect
#      variance reliably.
#   3. Curves whose fitted IC50 falls outside the actually-tested concentration
#      range are FLAGGED but KEPT (not dropped) — see `extrapolated` column.
#   4. Blanks are corrected PER CONCENTRATION, PER PLATE (sheet), not with a
#      single scalar per plate.
#   5. Each curve is fit through ALL replicate wells at once (3 reps per
#      population per month; for fRS585, 3 reps x every set run that month),
#      rather than fitting one curve per well and averaging IC50s afterward.

library(tidyverse)
library(readxl)
library(minpack.lm)
library(broom)
library(cowplot)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
theme_set(theme_cowplot())


# =============================================================================
# 1. Data import — identical to 11c-MICs.R
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

write_csv(all_mic_data, "data-processed/all_mic_data-new-e.csv")

# NOTE on block design: the March file ("ALL3") contributes data for ALL THREE
# drugs, not just AMPB/CASP as the older scripts' comments imply. So every
# drug actually spans two of the three months:
#   amphotericin: January + March
#   caspofungin:  January + March
#   fluconazole:  February + March
# No drug has three full months and none is confined to a single month, so
# the same per-drug model structure (evolution_history + month) applies
# uniformly to all three drugs in section 7 below.


# =============================================================================
# 2. Blank correction — per plate (sheet), per concentration
# =============================================================================
# Each plate's "Blank" row has its own OD at every one of the 12 concentration
# columns (media + drug, no cells). Background is therefore subtracted per
# sheet AND per concentration, rather than with one scalar per plate. Blank
# rows are dropped afterwards since they aren't populations to fit a curve to.
#
# IMPORTANT: sheet_name alone is NOT a unique plate identifier — the same
# name (e.g. "Set1_Rep1_CASP") is reused across different monthly files
# (January and March both have a "Set1_Rep1_CASP" sheet). Joining on
# sheet_name + concentration alone therefore matches each row to the blank
# from BOTH months (a many-to-many join), corrupting the row count. month
# must be part of the join key to disambiguate.

blank_lookup <- all_mic_data |>
  filter(population == "Blank") |>
  select(sheet_name, month, concentration, blank_OD = OD)

mic_data <- all_mic_data |>
  filter(population != "Blank") |>
  left_join(blank_lookup, by = c("sheet_name", "month", "concentration"), relationship = "many-to-one") |>
  mutate(
    OD_raw = OD,
    OD     = pmax(OD - blank_OD, 0)   # clip any negative noise at/near zero
  )

# Every well should have found a matching blank reading on its own plate
stopifnot(all(!is.na(mic_data$blank_OD)))


# =============================================================================
# 3. Prepare data for curve fitting
# =============================================================================

mic_data2 <- mic_data |>
  mutate(
    evolution_history = case_when(
      str_detect(population, "^35_") ~ "35 evolved",
      str_detect(population, "^40_") ~ "40 evolved",
      population == "fRS585"          ~ "fRS585"
    )
  ) |>
  # Replace the nominal 0 concentration with half the lowest tested non-zero
  # concentration *for that drug*, so every curve can be fit on a log
  # concentration scale (can't take log(0)). This is the next rung down the
  # 2-fold dilution series, rather than the old min_conc/10 placeholder.
  group_by(drug) |>
  mutate(
    min_nonzero_conc = min(concentration[concentration > 0], na.rm = TRUE),
    concentration     = if_else(concentration == 0, min_nonzero_conc / 2, concentration)
  ) |>
  ungroup() |>
  select(-min_nonzero_conc) |>
  # one unique ID per dose-response curve: population x drug x month. All
  # replicate wells for that population in that month (3 reps; for fRS585,
  # 3 reps x every set run that month) are pooled into the SAME curve, so
  # the curve is fit through multiple OD points at each concentration rather
  # than one curve per well.
  unite("curve_id", population, drug, month, sep = "_", remove = FALSE)


# =============================================================================
# 4. Fit a 3-parameter logistic dose-response curve per population x drug x
#    month (pooling replicate wells), no bootstrap
# =============================================================================
# OD = d / (1 + exp(b * (log(concentration) - log(IC50))))
#   d    = upper asymptote (OD with no drug)
#   b    = slope (steepness of inhibition)
#   IC50 = concentration at half-maximal OD ("e" in the model formula below)
#
# QC flags recorded per curve, per Joey's "flag but keep" decision:
#   b_hit_upper  — slope pinned at its upper bound (b >= 49.5), a sign the
#                  optimizer couldn't identify a clean inflection
#   extrapolated — fitted IC50 falls outside the concentration range actually
#                  tested for that curve

fit_ic50_curves <- function(data, group_var = "curve_id") {

  pops      <- unique(data[[group_var]])
  fit_list  <- list()
  pred_list <- list()

  for (pop in pops) {

    subdata <- data |> filter(.data[[group_var]] == pop, concentration > 0)

    if (nrow(subdata) < 5) {
      fit_list[[pop]] <- tibble(
        curve_id = pop, IC50 = NA_real_, b = NA_real_, d = NA_real_,
        converged = FALSE, b_hit_upper = NA, extrapolated = NA,
        pseudo_r2 = NA_real_, error_message = "Insufficient data points"
      )
      next
    }

    conc_min <- min(subdata$concentration)
    conc_max <- max(subdata$concentration)

    start_vals <- list(d = max(subdata$OD), b = 10, e = median(subdata$concentration))

    fit <- tryCatch(
      nlsLM(
        OD ~ d / (1 + exp(b * (log(concentration) - log(e)))),
        data    = subdata,
        start   = start_vals,
        lower   = c(d = 0, b = 0.01, e = conc_min / 10),
        upper   = c(d = 2, b = 50,   e = conc_max * 10),
        control = nls.lm.control(maxiter = 500)
      ),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      fit_list[[pop]] <- tibble(
        curve_id = pop, IC50 = NA_real_, b = NA_real_, d = NA_real_,
        converged = FALSE, b_hit_upper = NA, extrapolated = NA,
        pseudo_r2 = NA_real_, error_message = "Fit failed to converge"
      )
      next
    }

    co     <- coef(fit)
    pred   <- predict(fit)
    ss_res <- sum((subdata$OD - pred)^2)
    ss_tot <- sum((subdata$OD - mean(subdata$OD))^2)

    fit_list[[pop]] <- tibble(
      curve_id      = pop,
      IC50          = unname(co["e"]),
      b             = unname(co["b"]),
      d             = unname(co["d"]),
      converged     = TRUE,
      b_hit_upper   = unname(co["b"]) >= 49.5,
      extrapolated  = unname(co["e"]) < conc_min | unname(co["e"]) > conc_max,
      pseudo_r2     = 1 - ss_res / ss_tot,
      error_message = NA_character_
    )

    # smooth prediction curve for diagnostic plotting
    grid       <- tibble(concentration = exp(seq(log(conc_min), log(conc_max), length.out = 100)))
    grid$fit   <- predict(fit, newdata = grid)
    grid[[group_var]] <- pop
    pred_list[[pop]] <- grid
  }

  list(
    ic50_table  = bind_rows(fit_list),
    pred_curves = bind_rows(pred_list)
  )
}

results    <- fit_ic50_curves(mic_data2)
ic50_table <- results$ic50_table


# =============================================================================
# 5. Attach metadata back onto the per-curve IC50 table
# =============================================================================

curve_metadata <- mic_data2 |>
  select(curve_id, population, drug, month, evolution_history) |>
  distinct()

# set/rep are deliberately dropped from curve_metadata: they're pooled INTO
# each curve now rather than distinguishing curves, so they're no longer
# 1:1 with curve_id. Guard against the same many-to-many join bug class as
# the blank-correction step above.
stopifnot(nrow(curve_metadata) == n_distinct(mic_data2$curve_id))

ic50_table2 <- ic50_table |>
  left_join(curve_metadata, by = "curve_id")

write_csv(ic50_table2, "data-processed/ic50-table-11e.csv")

# QC summary: how many curves converged / hit bounds / extrapolated, by drug
qc_summary <- ic50_table2 |>
  group_by(drug) |>
  summarise(
    n_curves       = n(),
    n_converged    = sum(converged, na.rm = TRUE),
    n_extrapolated = sum(extrapolated, na.rm = TRUE),
    n_b_hit_upper  = sum(b_hit_upper, na.rm = TRUE),
    .groups = "drop"
  )
print(qc_summary)

# Drop curves that failed to fit entirely (insufficient data or nlsLM never
# converged) -- there's no usable IC50 for these, so unlike the extrapolated/
# b_hit_upper flags (which are kept), these rows are removed from every
# downstream step (comparison models, plots).
cat(sum(!ic50_table2$converged), "of", nrow(ic50_table2),
    "curves did not converge and are excluded from downstream analysis\n")

ic50_table3 <- ic50_table2 |>
  filter(converged) |> 
  filter(b_hit_upper == "FALSE")


# =============================================================================
# 6. Diagnostic plots — raw data + fitted curve, one figure per drug
# =============================================================================

pred_curves <- results$pred_curves |> left_join(curve_metadata, by = "curve_id")

for (d in unique(mic_data$drug)) {

  p <- ggplot() +
    geom_point(data = filter(mic_data, drug == d), aes(x = concentration, y = OD),
               alpha = 0.5, size = 1) +
    geom_line(data = filter(pred_curves, drug == d),
              aes(x = concentration, y = fit, group = curve_id),
              color = "blue", linewidth = 0.4) +
    scale_x_log10() +
    facet_wrap(~ population, scales = "free_y") +
    labs(title = paste("Dose-response curves -", d),
         x = "Concentration (log scale)", y = "OD (blank-corrected)") +
    theme(legend.position = "none")

  ggsave(paste0("figures/11e-dose-response-", d, ".png"), p, width = 16, height = 14)
}


# =============================================================================
# 7. Compare IC50 across evolution history, separately for each drug
# =============================================================================
# Fixed effect for month (block): only 2 month levels per drug, too few to
# estimate a random-effect variance reliably, so block is a fixed covariate
# (ANCOVA-style) rather than a mixed-model random intercept. IC50 is modeled
# on the log scale since 2-fold serial dilution data are naturally
# log-distributed. Extrapolated/bound-hitting curves are kept (flagged, not
# dropped) per the QC decision above.

# No within-month averaging needed here: each curve already pools its 3
# replicate wells (and, for fRS585, every set run that month) at the curve-
# fitting stage in section 4, so curve_id is already exactly one row per
# population x drug x month -- the unit the comparison model should treat
# as one independent data point. fRS585 therefore contributes one IC50 per
# month, same as every evolved population, rather than one per set.

comparison_data <- ic50_table3 |>
  filter(!is.na(IC50), IC50 > 0) |>  # non-converged curves already dropped above
  mutate(
    log_IC50          = log(IC50),
    evolution_history = factor(evolution_history, levels = c("fRS585", "35 evolved", "40 evolved")),
    month              = factor(month)
  )

write_csv(comparison_data, "data-processed/ic50-comparison-data-11e.csv")

drug_models <- comparison_data |>
  split(comparison_data$drug) |>
  map(\(d) lm(log_IC50 ~ evolution_history + month, data = d))

iwalk(drug_models, \(mod, drug_name) {
  cat("\n==============================\n", drug_name, "\n==============================\n")
  print(summary(mod))
})

# Tidy coefficient table across all three drugs, for a combined look
model_results <- imap_dfr(drug_models, \(mod, drug_name) {
  tidy(mod, conf.int = TRUE) |> mutate(drug = drug_name)
})

write_csv(model_results, "data-processed/ic50-model-results-11e.csv")


# =============================================================================
# 8. Visualize the evolution-history comparison, per drug
# =============================================================================

summary_stats <- comparison_data |>
  group_by(drug, evolution_history) |>
  summarise(
    mean_log_ic50 = mean(log_IC50),
    se_log_ic50   = sd(log_IC50) / sqrt(n()),
    n             = n(),
    .groups = "drop"
  )

ggplot() +
  geom_jitter(data = comparison_data, aes(x = evolution_history, y = log_IC50),
              width = 0.15, alpha = 0.3) +
  geom_pointrange(data = summary_stats,
                   aes(x = evolution_history, y = mean_log_ic50,
                       ymin = mean_log_ic50 - se_log_ic50, ymax = mean_log_ic50 + se_log_ic50),
                   color = "blue") +
  facet_wrap(~ drug, scales = "free_y") +
  labs(x = "Evolution history", y = "log(IC50)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/11e-ic50-evolution-history.png", width = 12, height = 5)
