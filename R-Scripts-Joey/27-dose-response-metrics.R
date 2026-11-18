# =============================================================================
# 27-dose-response-metrics.R
# Additional dose-response metrics beyond IC50, per plate curve, normalised to
# the same-sheet ancestor (fRS585), then averaged per strain.
#
# Metrics (all from 24 h endpoint OD; no 48 h reads exist, so true SMG is not
# available):
#   IC90       absolute 90% inhibition: concentration where fitted OD = 0.1*top.
#              Undefined (NA) when bot >= 0.1*top (trailing growth).
#              Flagged when beyond the highest concentration tested.
#   SMG24      24 h supra-MIC growth: mean OD at concentrations above the
#              curve's own IC50, divided by OD at zero drug (tolerance).
#   resid      fitted bot / top (model-based residual growth; check on SMG24).
#   AUC        mean relative growth (OD / OD at zero drug) across the log2
#              concentration range (trapezoid; model-free).
#
# Ancestor normalisation (same drug x month x set x rep sheet), log2 fold
# change (primary; matches Script 30):
#   IC50, IC90, AUC: log2(strain / ancestor)
#   SMG24, resid:    log2((strain + EPS) / (ancestor + EPS)), EPS = 0.05,
#                    because ancestor values are often ~0 for caspofungin and
#                    amphotericin B.
# Differences (strain - ancestor) are also kept as d_* columns.
#
# Inputs:  data-processed/all_mic_data-new.csv   (from Script 11c)
#          data-processed/normalised-ic50-per-plate.csv (11d; IC50 check)
# Outputs: data-processed/dr-metrics-per-plate-27.csv
#          data-processed/dr-metrics-per-strain-27.csv
#          data-processed/dr-relative-growth-27.csv  (well-level, for FVT)
# =============================================================================


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)
library(minpack.lm)

OUT  <- "data-processed"
EPS  <- 0.05   # pseudocount for fold changes of metrics that can be ~0
KEYS <- c("population", "evolution_history", "drug", "month", "set", "rep")
SHEET_KEYS <- c("drug", "month", "set", "rep")


# =============================================================================
# 2. Blank-corrected well-level data (as in 11d)
# =============================================================================

raw <- read_csv(file.path(OUT, "all_mic_data-new.csv"), show_col_types = FALSE)

blank <- raw |>
  filter(population == "Blank") |>
  select(sheet_name, month, concentration, blank_OD = OD)

od <- raw |>
  filter(population != "Blank") |>
  left_join(blank, by = c("sheet_name", "month", "concentration"),
            relationship = "many-to-one") |>
  mutate(
    OD = pmax(OD - blank_OD, 0),
    evolution_history = case_when(
      str_detect(population, "^35_") ~ "35 evolved",
      str_detect(population, "^40_") ~ "40 evolved",
      population == "fRS585"          ~ "fRS585"
    )
  ) |>
  select(all_of(KEYS), concentration, OD)

stopifnot(all(count(od, across(all_of(KEYS)))$n == 12))

# Relative growth (OD / zero-drug OD in the same row); kept for FVT scripts
rel <- od |>
  group_by(across(all_of(KEYS))) |>
  mutate(od0 = OD[concentration == 0], rel_growth = OD / od0) |>
  ungroup()

write_csv(rel, file.path(OUT, "dr-relative-growth-27.csv"))


# =============================================================================
# 3. 4PL fits (identical to 11d: zero conc -> min/10, 20-start grid, min RSS)
# =============================================================================

fit_4pl <- function(df) {
  na_fit <- tibble(ic50 = NA_real_, bot = NA_real_, top = NA_real_, slope = NA_real_)
  start_grid <- expand_grid(
    e0     = unname(quantile(df$concentration, c(0.1, 0.25, 0.5, 0.75, 0.9))),
    slope0 = c(1, 5, 10, 30)
  )
  fits <- map2(start_grid$e0, start_grid$slope0, \(e0, s0) tryCatch(
    nlsLM(
      OD ~ bot + (top - bot) / (1 + exp(slope * (log(concentration) - log(e)))),
      data    = df,
      start   = list(bot = 0, top = max(df$OD), slope = s0, e = e0),
      lower   = c(bot = 0,   top = 0,   slope = 0.01, e = min(df$concentration)),
      upper   = c(bot = 0.5, top = 2,   slope = 200,  e = max(df$concentration) * 5),
      control = nls.lm.control(maxiter = 500)
    ),
    error = function(e) NULL
  )) |> compact()
  if (length(fits) == 0) return(na_fit)
  co <- coef(fits[[which.min(map_dbl(fits, deviance))]])
  tibble(ic50 = co[["e"]], bot = co[["bot"]], top = co[["top"]], slope = co[["slope"]])
}

fit_input <- od |>
  group_by(population, drug) |>
  mutate(concentration = if_else(concentration == 0,
                                 min(concentration[concentration > 0]) / 10,
                                 concentration)) |>
  ungroup()

cat("Fitting 4PL curves...\n")
fits <- fit_input |>
  group_by(across(all_of(KEYS))) |>
  group_modify(\(d, k) fit_4pl(d)) |>
  ungroup()

# Check against 11d IC50s
ic50_11d <- read_csv(file.path(OUT, "normalised-ic50-per-plate.csv"), show_col_types = FALSE)
ic50_check <- ic50_11d |>
  select(all_of(KEYS), ic50_11d = ic50) |>
  inner_join(fits, by = KEYS) |>
  summarise(n = n(), max_rel_diff = max(abs(ic50 - ic50_11d) / ic50_11d))
print(ic50_check)


# =============================================================================
# 4. Metrics per curve
# =============================================================================

max_conc <- od |> group_by(drug) |> summarise(max_conc = max(concentration))

auc_log <- function(conc, y) {
  x <- log2(conc[conc > 0]); y <- y[conc > 0]
  o <- order(x); x <- x[o]; y <- y[o]
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2) / (max(x) - min(x))
}

model_free <- rel |>
  left_join(fits |> select(all_of(KEYS), ic50), by = KEYS) |>
  group_by(across(all_of(KEYS))) |>
  summarise(
    smg24 = if (any(concentration > first(ic50))) mean(rel_growth[concentration > first(ic50)]) else NA_real_,
    auc   = auc_log(concentration, rel_growth),
    .groups = "drop"
  )

metrics <- fits |>
  left_join(max_conc, by = "drug") |>
  mutate(
    resid = bot / top,
    # absolute IC90: bot + (top - bot) / (1 + exp(s * (log c - log e))) = 0.1 * top
    ic90 = if_else(bot < 0.1 * top,
                   ic50 * ((top - bot) / (0.1 * top - bot) - 1)^(1 / slope),
                   NA_real_),
    ic90_extrapolated = !is.na(ic90) & ic90 > max_conc
  ) |>
  left_join(model_free, by = KEYS) |>
  select(-max_conc)


# =============================================================================
# 5. Ancestor normalisation
# =============================================================================

anc_metrics <- metrics |>
  filter(population == "fRS585") |>
  select(all_of(SHEET_KEYS), ic50_anc = ic50, ic90_anc = ic90,
         resid_anc = resid, smg24_anc = smg24, auc_anc = auc)

per_plate <- metrics |>
  filter(population != "fRS585") |>
  left_join(anc_metrics, by = SHEET_KEYS) |>
  mutate(
    log2_ic50 = log2(ic50 / ic50_anc),
    log2_ic90 = log2(ic90 / ic90_anc),
    # fold change (primary; consistent with Script 30 GAMs)
    lfc_resid = log2((resid + EPS) / (resid_anc + EPS)),
    lfc_smg24 = log2((smg24 + EPS) / (smg24_anc + EPS)),
    lfc_auc   = log2(auc / auc_anc),
    # differences (kept for reference)
    d_resid   = resid - resid_anc,
    d_smg24   = smg24 - smg24_anc,
    d_auc     = auc - auc_anc
  )

per_strain <- per_plate |>
  group_by(population, evolution_history, drug) |>
  summarise(
    n_plates   = n(),
    n_ic90     = sum(!is.na(log2_ic90)),
    n_ic90_extrapolated = sum(ic90_extrapolated, na.rm = TRUE),
    across(c(log2_ic50, log2_ic90, lfc_resid, lfc_smg24, lfc_auc,
             d_resid, d_smg24, d_auc), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(across(where(is.double), \(x) if_else(is.nan(x), NA_real_, x)))

write_csv(per_plate,  file.path(OUT, "dr-metrics-per-plate-27.csv"))
write_csv(per_strain, file.path(OUT, "dr-metrics-per-strain-27.csv"))
