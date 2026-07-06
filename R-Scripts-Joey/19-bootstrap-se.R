# =============================================================================
# 19-bootstrap-se.R
#
# Residual bootstrap standard errors for:
#   1. SSH TPC traits (Topt, Tmax, Rmax, B80) — 1000 bootstraps per strain,
#      fitting the SSH model to all 3 replicate wells × 5 temperatures (~15 pts)
#   2. IC50s (fluconazole, caspofungin, amphotericin) — 1000 bootstraps per
#      strain × drug, pooling all replicates and months before fitting
#
# Bootstrap method: residual (resample residuals from point-estimate fit
#   with replacement, add back to fitted values, refit)
#
# Inputs:
#   data-processed/gcplyr/gcplyr-metrics-per-well-17.csv  — per-well AUC
#   data-raw/MICs/FINAL/*/24h.xlsx                        — raw MIC plates
#
# Outputs (CSVs):
#   data-processed/gcplyr/tpc-boot-se-19.csv
#   data-processed/ic50-boot-pooled-19.csv
#
# Outputs (figures, suffix -19):
#   figures/thermal-traits-dotplot-boot-19.png
#   figures/fluconazole-ic50-dotplot-boot-19.png
#   figures/caspofungin-ic50-dotplot-boot-19.png
#   figures/amphotericin-ic50-dotplot-boot-19.png
#   figures/tpc-ci-ribbon-per-strain-19.png
#   figures/fluconazole-ci-ribbon-per-strain-19.png
#   figures/caspofungin-ci-ribbon-per-strain-19.png
#   figures/amphotericin-ci-ribbon-per-strain-19.png
#
# Sections:
#   1. Setup
#   2. SSH model functions + bootstrap helpers (params_at_bound, boot_residual)
#   3. TPC residual bootstrap
#   4. IC50 data preparation (pool reps + months per strain × drug)
#   5. IC50 residual bootstrap
#   6. Plots
#   7. Export
# =============================================================================


# =============================================================================
# 1. Setup
# =============================================================================

library(tidyverse)
library(minpack.lm)
library(car)
library(readxl)
library(cowplot)
theme_set(theme_cowplot())
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

set.seed(4782)

AUC_PATH <- "data-processed/gcplyr/gcplyr-metrics-per-well-17.csv"
OUT_DIR  <- "data-processed/gcplyr"
FIGS     <- "figures"

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")
TARGET_EVO <- c("35 evolved", "40 evolved", "fRS585")

# SSH constants
K_B    <- 8.617333e-5   # Boltzmann constant (eV K⁻¹)
TREF_K <- 288.15        # Reference temperature: 15°C in Kelvin
TMAX_CAP <- 50.0

# SSH parameter bounds (identical to script 17)
BOUNDS_LO <- c(r_tref = 1e-6, e = 0.01, eh =  0.5, th = 303.0)
BOUNDS_HI <- c(r_tref = 10.0, e =  3.0, eh = 50.0, th = 335.0)
START_LO  <- c(r_tref = 0.01, e =  0.1, eh =  0.5, th = 305.0)
START_HI  <- c(r_tref =  2.0, e =  2.0, eh = 20.0, th = 325.0)

# Bootstrap settings
N_BOOT        <- 1000   # bootstrap iterations per curve
N_STARTS      <- 500    # multi-start count for initial point-estimate fit
N_STARTS_BOOT <- 20     # fallback random starts per bootstrap refit


# =============================================================================
# 2. SSH model functions
# =============================================================================

sharpeschoolhigh <- function(T_c, r_tref, e, eh, th) {
  T_k <- T_c + 273.15
  r_tref *
    exp((e  / K_B) * (1 / TREF_K - 1 / T_k)) /
    (1 + exp((eh / K_B) * (1 / th - 1 / T_k)))
}

calc_tpc_traits <- function(r_tref, e, eh, th,
                             T_range = seq(0, 55, length.out = 10000)) {
  na_result <- c(topt = NA_real_, tmax = NA_real_, rmax = NA_real_, b80 = NA_real_)
  preds <- sharpeschoolhigh(T_range, r_tref, e, eh, th)
  if (!any(is.finite(preds))) return(na_result)

  opt <- optim(
    par    = T_range[which.max(preds)],
    fn     = function(T) -sharpeschoolhigh(T, r_tref, e, eh, th),
    method = "Brent",
    lower  = min(T_range),
    upper  = max(T_range)
  )
  topt <- opt$par
  rmax <- -opt$value

  thresh_fn <- function(T) sharpeschoolhigh(T, r_tref, e, eh, th) - 0.05 * rmax
  tmax <- tryCatch(
    uniroot(thresh_fn, lower = topt, upper = max(T_range), tol = 1e-8)$root,
    error = function(e) TMAX_CAP
  )

  above_80 <- T_range[preds >= 0.8 * rmax]
  b80 <- if (length(above_80) >= 2) max(above_80) - min(above_80) else NA_real_

  c(topt = topt, tmax = tmax, rmax = rmax, b80 = b80)
}

# Multi-start SSH fit for the point estimate (same as script 17)
fit_ssh_multistart <- function(temps, rates) {
  starts <- lapply(seq_len(N_STARTS), function(i)
    c(
      runif(1, START_LO["r_tref"], START_HI["r_tref"]),
      runif(1, START_LO["e"],      START_HI["e"]),
      runif(1, START_LO["eh"],     START_HI["eh"]),
      runif(1, START_LO["th"],     START_HI["th"])
    )
  )

  best_popt <- NULL
  best_rss  <- Inf

  for (p0 in starts) {
    tryCatch({
      fit <- suppressWarnings(nlsLM(
        rates ~ sharpeschoolhigh(temps, r_tref, e, eh, th),
        start   = list(r_tref = p0[1], e = p0[2], eh = p0[3], th = p0[4]),
        lower   = BOUNDS_LO,
        upper   = BOUNDS_HI,
        control = nls.lm.control(maxiter = 200, maxfev = 5000)
      ))
      rss <- sum(residuals(fit)^2)
      if (rss < best_rss) { best_rss <- rss; best_popt <- coef(fit) }
    }, error = function(e) NULL)
  }

  if (is.null(best_popt)) return(NULL)

  ss_tot <- sum((rates - mean(rates))^2)
  r2     <- if (ss_tot > 0) 1 - best_rss / ss_tot else NA_real_
  traits <- calc_tpc_traits(
    best_popt["r_tref"], best_popt["e"], best_popt["eh"], best_popt["th"]
  )

  list(params = best_popt, r2 = r2,
       topt = traits["topt"], tmax = traits["tmax"],
       rmax = traits["rmax"], b80  = traits["b80"])
}

# Fast refit for each bootstrap sample: try the point-estimate params first,
# then fall back to N_STARTS_BOOT random starts if that fails.
fit_ssh_boot_refit <- function(temps, rates, init_params) {
  all_starts <- c(
    list(init_params),
    lapply(seq_len(N_STARTS_BOOT), function(i)
      c(
        runif(1, START_LO["r_tref"], START_HI["r_tref"]),
        runif(1, START_LO["e"],      START_HI["e"]),
        runif(1, START_LO["eh"],     START_HI["eh"]),
        runif(1, START_LO["th"],     START_HI["th"])
      )
    )
  )

  best_popt <- NULL
  best_rss  <- Inf

  for (p0 in all_starts) {
    tryCatch({
      fit <- suppressWarnings(nlsLM(
        rates ~ sharpeschoolhigh(temps, r_tref, e, eh, th),
        start   = list(r_tref = p0[1], e = p0[2], eh = p0[3], th = p0[4]),
        lower   = BOUNDS_LO,
        upper   = BOUNDS_HI,
        control = nls.lm.control(maxiter = 200, maxfev = 5000)
      ))
      rss <- sum(residuals(fit)^2)
      if (rss < best_rss) { best_rss <- rss; best_popt <- coef(fit) }
    }, error = function(e) NULL)
    # If init_params converged well, skip random starts
    if (!is.null(best_popt) && identical(p0, init_params) && best_rss < 1e-8) break
  }

  best_popt
}


# ── 2d. Bootstrap helpers ─────────────────────────────────────────────────────

# Returns TRUE if any parameter falls within rel_tol of its bound
# (relative to the bound range), indicating the optimizer hit a constraint.
params_at_bound <- function(params, lo, hi, rel_tol = 1e-3) {
  if (any(!is.finite(params))) return(TRUE)
  range_w <- hi - lo
  any(
    (params - lo) / range_w < rel_tol |
    (hi - params) / range_w < rel_tol
  )
}

# Generic residual bootstrap.
#
# fitted_vals  numeric vector of fitted values from the point-estimate model
# resids       numeric vector of residuals
# refit_fn     function(boot_y) -> named numeric vector of statistics, or NULL
#              if the refit failed or any parameter touched a bound
# R            number of VALID samples to accumulate (default: N_BOOT)
# max_iter     hard ceiling on total iterations (default: 10 × R)
#
# Returns a list:
#   $samples      - R × n_stats matrix (NA rows if max_iter reached before R)
#   $n_valid      - valid samples actually accumulated
#   $n_total_iter - total resampling iterations run
boot_residual <- function(fitted_vals, resids, refit_fn,
                          R = N_BOOT, max_iter = 10L * R) {
  first_result <- NULL
  samples      <- vector("list", R)
  n_valid      <- 0L
  n_iter       <- 0L

  while (n_valid < R && n_iter < max_iter) {
    n_iter <- n_iter + 1L
    boot_y <- fitted_vals + sample(resids, size = length(resids), replace = TRUE)
    result <- refit_fn(boot_y)

    if (!is.null(result) && all(is.finite(result))) {
      n_valid            <- n_valid + 1L
      samples[[n_valid]] <- result
      if (is.null(first_result)) first_result <- result
    }
  }

  n_stats <- if (!is.null(first_result)) length(first_result) else 0L
  stat_nm <- if (!is.null(first_result)) names(first_result)  else character(0L)

  mat <- matrix(NA_real_, nrow = R, ncol = n_stats,
                dimnames = list(NULL, stat_nm))
  if (n_valid > 0)
    mat[seq_len(n_valid), ] <- matrix(
      unlist(samples[seq_len(n_valid)]),
      nrow = n_valid, ncol = n_stats, byrow = TRUE
    )

  list(samples = mat, n_valid = n_valid, n_total_iter = n_iter)
}


# =============================================================================
# 3. TPC residual bootstrap
# =============================================================================

# ── 3a. Load per-well AUC (output of script 17) ───────────────────────────────

auc_data <- read_csv(AUC_PATH, show_col_types = FALSE) |>
  filter(evolution_history %in% TARGET_EVO,
         !is.na(auc_gc), is.finite(auc_gc))

strains <- auc_data |> distinct(strain, evolution_history)
cat(sprintf(
  "Bootstrapping TPC traits: %d strains × %d bootstraps\n",
  nrow(strains), N_BOOT
))

# ── 3b-3c. Point estimate + residual bootstrap per strain ─────────────────────

tpc_boot_list   <- vector("list", nrow(strains))
tpc_ribbon_list <- vector("list", nrow(strains))

for (i in seq_len(nrow(strains))) {
  sid <- strains$strain[i]
  evo <- strains$evolution_history[i]
  d   <- auc_data |> filter(strain == sid)

  if (nrow(d) < 5) {
    cat(sprintf("  SKIP (n = %d): %s\n", nrow(d), sid))
    next
  }

  # Point-estimate SSH fit (multi-start, same as script 17)
  res <- fit_ssh_multistart(d$test_temperature, d$auc_gc)

  if (is.null(res)) {
    cat(sprintf("  FAILED (point estimate): %s\n", sid))
    next
  }

  p      <- res$params
  fitted <- sharpeschoolhigh(
    d$test_temperature, p["r_tref"], p["e"], p["eh"], p["th"]
  )
  resids <- d$auc_gc - fitted

  # boot_residual(): accumulate exactly N_BOOT valid samples.
  # Valid = converged AND no parameter within rel_tol of any bound.
  # IIFE freezes per-iteration values to avoid for-loop late-binding.
  # refit_fn also returns all 4 raw SSH params for CI ribbon computation.
  tpc_refit_fn <- (function(temps_, p_, blo_, bhi_) {
    function(boot_y) {
      popt <- fit_ssh_boot_refit(temps_, boot_y, p_)
      if (is.null(popt)) return(NULL)
      if (params_at_bound(popt[names(blo_)], blo_, bhi_)) return(NULL)
      tr <- calc_tpc_traits(popt["r_tref"], popt["e"], popt["eh"], popt["th"])
      if (any(is.na(tr))) return(NULL)
      c(tr,
        r_tref = unname(popt["r_tref"]),
        e      = unname(popt["e"]),
        eh     = unname(popt["eh"]),
        th     = unname(popt["th"]))
    }
  })(d$test_temperature, p, BOUNDS_LO, BOUNDS_HI)

  boot_res <- boot_residual(fitted, resids, tpc_refit_fn)
  n_ok     <- boot_res$n_valid

  if (boot_res$n_total_iter >= 10L * N_BOOT)
    message(sprintf(
      "  WARNING: %s hit max_iter before accumulating %d valid TPC bootstraps (got %d)",
      sid, N_BOOT, n_ok
    ))

  # Always initialise boot_traits with the expected named column structure.
  # Copy from boot_res$samples by column name — robust to NULL or mismatched
  # colnames that can arise when names(first_result) is NULL in boot_residual.
  tpc_cols    <- c("topt", "tmax", "rmax", "b80", "r_tref", "e", "eh", "th")
  boot_traits <- matrix(NA_real_, nrow = N_BOOT, ncol = length(tpc_cols),
                        dimnames = list(NULL, tpc_cols))
  shared_cols <- intersect(tpc_cols, colnames(boot_res$samples))
  if (n_ok > 0 && length(shared_cols) > 0)
    boot_traits[seq_len(n_ok), shared_cols] <-
      boot_res$samples[seq_len(n_ok), shared_cols, drop = FALSE]

  # CI ribbon: evaluate SSH model over T range for each bootstrap param set
  T_RIBBON   <- seq(15, 50, length.out = 200)
  fitted_crv <- sharpeschoolhigh(T_RIBBON, p["r_tref"], p["e"], p["eh"], p["th"])
  if (n_ok > 0) {
    boot_ssh <- boot_traits[seq_len(n_ok), c("r_tref", "e", "eh", "th"), drop = FALSE]
    crv_mat  <- vapply(
      seq_len(n_ok),
      function(j) sharpeschoolhigh(T_RIBBON,
                                   boot_ssh[j, "r_tref"], boot_ssh[j, "e"],
                                   boot_ssh[j, "eh"],     boot_ssh[j, "th"]),
      numeric(length(T_RIBBON))
    )  # length(T_RIBBON) × n_ok; columns = bootstrap replicates
    tpc_ribbon_list[[i]] <- tibble(
      strain            = sid,
      evolution_history = evo,
      temperature       = T_RIBBON,
      fitted            = fitted_crv,
      ci_lo = apply(crv_mat, 1, quantile, 0.025, na.rm = TRUE),
      ci_hi = apply(crv_mat, 1, quantile, 0.975, na.rm = TRUE)
    )
  }

  tpc_boot_list[[i]] <- tibble(
    strain            = sid,
    evolution_history = evo,
    # Point estimates
    topt  = res$topt,
    tmax  = res$tmax,
    rmax  = res$rmax,
    b80   = res$b80,
    th    = p["th"],
    th_c  = p["th"] - 273.15,
    r2    = res$r2,
    # Bootstrap SEs (SD of bootstrap distribution)
    se_topt = sd(boot_traits[, "topt"], na.rm = TRUE),
    se_tmax = sd(boot_traits[, "tmax"], na.rm = TRUE),
    se_rmax = sd(boot_traits[, "rmax"], na.rm = TRUE),
    se_b80  = sd(boot_traits[, "b80"],  na.rm = TRUE),
    se_th   = sd(boot_traits[, "th"],   na.rm = TRUE),
    n_obs        = nrow(d),
    n_boot_ok    = n_ok,
    n_total_iter = boot_res$n_total_iter
  )

  if (i %% 5 == 0 || i == nrow(strains))
    cat(sprintf("  %d / %d strains done\n", i, nrow(strains)))
}

tpc_boot_se <- bind_rows(tpc_boot_list)
cat(sprintf("\nTPC bootstrap complete. %d / %d strains fitted.\n",
            nrow(tpc_boot_se), nrow(strains)))
print(
  tpc_boot_se |>
    group_by(evolution_history) |>
    summarise(
      n         = n(),
      mean_topt = round(mean(topt,  na.rm = TRUE), 2),
      se_topt   = round(mean(se_topt, na.rm = TRUE), 3),
      mean_tmax = round(mean(tmax,  na.rm = TRUE), 2),
      se_tmax   = round(mean(se_tmax, na.rm = TRUE), 3),
      .groups   = "drop"
    )
)


# =============================================================================
# 4. IC50 data preparation (pool all reps + months per strain × drug)
# =============================================================================

march_data    <- "data-raw/MICs/FINAL/Mar6.25(35C-ev_ALL3+40C-ev_Res)/24h.xlsx"
january_data  <- "data-raw/MICs/FINAL/Jan31.25(40C-ev_CASP_AMPB)/24h.xlsx"
february_data <- "data-raw/MICs/FINAL/Feb26.25(40C-ev_FLZ)/24h.xlsx"

FOCAL_POPS <- c("^35_", "^40_", "^fRS585$", "^Blank$")

read_mic_file <- function(path, range = "A24:M32", default_drug = NA_character_) {
  month <- case_when(
    str_detect(path, "(?i)Jan") ~ "January",
    str_detect(path, "(?i)Feb") ~ "February",
    str_detect(path, "(?i)Mar") ~ "March",
    TRUE ~ NA_character_
  )
  excel_sheets(path) |>
    map(\(sh) {
      d <- tryCatch(
        read_excel(path, sheet = sh, range = range),
        error = function(e) NULL
      )
      if (is.null(d) || ncol(d) == 0 || nrow(d) == 0) return(NULL)
      d |>
        rename(population = 1) |>
        pivot_longer(-population, names_to = "concentration", values_to = "OD") |>
        filter(!is.na(OD)) |>
        mutate(
          concentration = as.numeric(concentration),
          sheet_name    = sh,
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

# Per-plate, per-concentration blank correction (same as script 11c)
blank_lookup <- all_mic_data |>
  filter(population == "Blank") |>
  select(sheet_name, month, concentration, blank_OD = OD)

mic_data <- all_mic_data |>
  filter(population != "Blank") |>
  left_join(
    blank_lookup,
    by = c("sheet_name", "month", "concentration"),
    relationship = "many-to-one"
  ) |>
  mutate(OD = pmax(OD - blank_OD, 0))

stopifnot(all(!is.na(mic_data$blank_OD)))

# Assign evolution history; replace zero concentrations per strain × drug
mic_data <- mic_data |>
  mutate(
    evolution_history = case_when(
      str_detect(population, "^35_") ~ "35 evolved",
      str_detect(population, "^40_") ~ "40 evolved",
      population == "fRS585"          ~ "fRS585"
    )
  ) |>
  filter(!is.na(evolution_history)) |>
  group_by(population, drug) |>
  mutate(
    min_conc      = min(concentration[concentration > 0], na.rm = TRUE),
    concentration = if_else(concentration == 0, min_conc / 10, concentration)
  ) |>
  ungroup() |>
  select(-min_conc)

# All data are now pooled by population × drug — no sub-setting by rep or month.
# Each row is one (population, drug, concentration, OD) observation across all plates.

cat(sprintf("\nIC50 data: %d rows covering %d populations × drugs\n",
            nrow(mic_data),
            n_distinct(paste(mic_data$population, mic_data$drug))))


# =============================================================================
# 5. IC50 residual bootstrap (pooled per strain × drug)
# =============================================================================

strain_drug_combos <- mic_data |>
  distinct(population, evolution_history, drug)

cat(sprintf(
  "Bootstrapping IC50: %d strain × drug combos × %d bootstraps\n",
  nrow(strain_drug_combos), N_BOOT
))

ic50_boot_list   <- vector("list", nrow(strain_drug_combos))
ic50_ribbon_list <- vector("list", nrow(strain_drug_combos))
ic50_error_log   <- list()

for (i in seq_len(nrow(strain_drug_combos))) {
  pop  <- strain_drug_combos$population[i]
  evo  <- strain_drug_combos$evolution_history[i]
  drg  <- strain_drug_combos$drug[i]

  sub <- mic_data |>
    filter(population == pop, drug == drg, concentration > 0)

  if (nrow(sub) < 5) {
    ic50_error_log[[length(ic50_error_log) + 1]] <- tibble(
      population = pop, drug = drg, reason = "too few data points"
    )
    next
  }

  # Point-estimate fit: 3-parameter logistic
  # OD = d / (1 + exp(b * (log(conc) - log(IC50))))
  start_vals <- list(
    d = max(sub$OD, na.rm = TRUE),
    b = 10,
    e = median(sub$concentration)
  )

  # Parameter names: use 'hill' for the slope to avoid collision with loop index
  fit <- tryCatch(
    suppressWarnings(nlsLM(
      OD ~ d / (1 + exp(hill * (log(concentration) - log(e)))),
      data    = sub,
      start   = list(d = start_vals$d, hill = start_vals$b, e = start_vals$e),
      lower   = c(d = 0, hill = 0.01, e = min(sub$concentration)),
      upper   = c(d = 2, hill = 50,   e = max(sub$concentration) * 5),
      control = nls.lm.control(maxiter = 500)
    )),
    error = function(err) {
      ic50_error_log[[length(ic50_error_log) + 1]] <<- tibble(
        population = pop, drug = drg, reason = err$message
      )
      NULL
    }
  )

  if (is.null(fit)) next

  fit_vals   <- fitted(fit)
  fit_resids <- residuals(fit)
  pt_params  <- coef(fit)   # d, hill, e (IC50)

  ic50_lo <- c(d = 0,   hill = 0.01, e = min(sub$concentration))
  ic50_hi <- c(d = 2,   hill = 50,   e = max(sub$concentration) * 5)

  # boot_residual(): accumulate N_BOOT valid samples; valid = converged AND
  # no parameter within rel_tol of any bound (d, hill, and e = IC50).
  # IIFE freezes per-iteration values to avoid for-loop late-binding.
  # refit_fn returns all 3 params so CI ribbons can be computed downstream.
  ic50_refit_fn <- (function(sub_, pt_, lo_, hi_) {
    function(boot_y) {
      sub_b    <- sub_
      sub_b$OD <- boot_y
      fit_b <- tryCatch(
        suppressWarnings(nlsLM(
          OD ~ d / (1 + exp(hill * (log(concentration) - log(e)))),
          data    = sub_b,
          start   = as.list(pt_),
          lower   = lo_,
          upper   = hi_,
          control = nls.lm.control(maxiter = 500)
        )),
        error = function(e) NULL
      )
      if (is.null(fit_b)) return(NULL)
      bp <- coef(fit_b)
      if (params_at_bound(bp, lo_, hi_)) return(NULL)
      c(ic50 = unname(bp["e"]), d = unname(bp["d"]), hill = unname(bp["hill"]))
    }
  })(sub, pt_params, ic50_lo, ic50_hi)

  boot_ic50 <- boot_residual(fit_vals, fit_resids, ic50_refit_fn)

  if (boot_ic50$n_total_iter >= 10L * N_BOOT)
    message(sprintf(
      "  WARNING: %s / %s hit max_iter before accumulating %d valid IC50 bootstraps (got %d)",
      pop, drg, N_BOOT, boot_ic50$n_valid
    ))

  # Always initialise with the expected named column structure; copy by name.
  ic50_cols         <- c("ic50", "d", "hill")
  boot_ic50_samples <- matrix(NA_real_, nrow = N_BOOT, ncol = length(ic50_cols),
                              dimnames = list(NULL, ic50_cols))
  shared_ic50 <- intersect(ic50_cols, colnames(boot_ic50$samples))
  if (boot_ic50$n_valid > 0 && length(shared_ic50) > 0)
    boot_ic50_samples[seq_len(boot_ic50$n_valid), shared_ic50] <-
      boot_ic50$samples[seq_len(boot_ic50$n_valid), shared_ic50, drop = FALSE]

  ic50_vals <- boot_ic50_samples[seq_len(boot_ic50$n_valid), "ic50"]
  ic50_pt   <- pt_params["e"]

  # CI ribbon: evaluate logistic model over concentration range for each
  # bootstrap param set
  conc_range <- exp(seq(
    log(min(sub$concentration) * 0.3),
    log(max(sub$concentration) * 3),
    length.out = 200
  ))
  fitted_crv_ic50 <- with(as.list(pt_params),
    d / (1 + exp(hill * (log(conc_range) - log(e))))
  )
  n_ok_ic50 <- boot_ic50$n_valid
  if (n_ok_ic50 > 0) {
    boot_ic50_p  <- boot_ic50_samples[seq_len(n_ok_ic50), , drop = FALSE]
    ic50_crv_mat <- vapply(
      seq_len(n_ok_ic50),
      function(j) {
        boot_ic50_p[j, "d"] /
          (1 + exp(boot_ic50_p[j, "hill"] *
                     (log(conc_range) - log(boot_ic50_p[j, "ic50"]))))
      },
      numeric(length(conc_range))
    )  # length(conc_range) × n_ok_ic50; columns = bootstrap replicates
    ic50_ribbon_list[[i]] <- tibble(
      population        = pop,
      drug              = drg,
      evolution_history = evo,
      concentration     = conc_range,
      fitted            = fitted_crv_ic50,
      ci_lo = apply(ic50_crv_mat, 1, quantile, 0.025, na.rm = TRUE),
      ci_hi = apply(ic50_crv_mat, 1, quantile, 0.975, na.rm = TRUE)
    )
  }

  ic50_boot_list[[i]] <- tibble(
    population        = pop,
    evolution_history = evo,
    drug              = drg,
    ic50              = ic50_pt,
    se_ic50           = sd(ic50_vals, na.rm = TRUE),
    se_log_ic50       = sd(log(ic50_vals[ic50_vals > 0]), na.rm = TRUE),
    ic50_lower        = suppressWarnings(quantile(ic50_vals, 0.025, na.rm = TRUE)),
    ic50_upper        = suppressWarnings(quantile(ic50_vals, 0.975, na.rm = TRUE)),
    n_data            = nrow(sub),
    n_boot_ok         = boot_ic50$n_valid,
    n_total_iter      = boot_ic50$n_total_iter
  )

  if (i %% 10 == 0 || i == nrow(strain_drug_combos))
    cat(sprintf("  %d / %d done\n", i, nrow(strain_drug_combos)))
}

ic50_boot_se <- bind_rows(ic50_boot_list)
cat(sprintf("\nIC50 bootstrap complete. %d / %d combos fitted.\n",
            nrow(ic50_boot_se), nrow(strain_drug_combos)))

if (length(ic50_error_log) > 0) {
  cat("Errors:\n")
  print(bind_rows(ic50_error_log))
}


# =============================================================================
# 6. Plots
# =============================================================================

# ── 6a. TPC traits dot plot — per-strain bootstrap SE ─────────────────────────

TRAIT_LEVELS <- c("topt", "tmax", "th_c", "b80")
TRAIT_LABELS <- c(
  topt = "Topt (\u00b0C)",
  tmax = "Tmax (\u00b0C)",
  th_c = "Th (\u00b0C)",
  b80  = "B80 (\u00b0C)"
)

# Ancestor reference lines
anc_refs_tpc <- tpc_boot_se |>
  filter(evolution_history == "fRS585") |>
  select(topt, tmax, th_c, b80) |>
  pivot_longer(everything(), names_to = "trait", values_to = "anc_val") |>
  mutate(trait = factor(trait, levels = TRAIT_LEVELS))

# Per-strain values with bootstrap SE, evolved groups only
plot_tpc <- tpc_boot_se |>
  filter(evolution_history != "fRS585") |>
  select(strain, evolution_history, topt, tmax, th_c, b80,
         se_topt, se_tmax, se_th, se_b80) |>
  pivot_longer(
    cols      = c(topt, tmax, th_c, b80),
    names_to  = "trait",
    values_to = "estimate"
  ) |>
  mutate(
    se = case_when(
      trait == "topt" ~ se_topt,
      trait == "tmax" ~ se_tmax,
      trait == "th_c" ~ se_th,
      trait == "b80"  ~ se_b80
    ),
    trait             = factor(trait, levels = TRAIT_LEVELS),
    evolution_history = factor(evolution_history, levels = c("35 evolved", "40 evolved"))
  )

# Group mean ± SE (across strains) for the large summary point
group_means_tpc <- plot_tpc |>
  group_by(evolution_history, trait) |>
  summarise(
    mean_val = mean(estimate, na.rm = TRUE),
    se_mean  = sd(estimate,   na.rm = TRUE) / sqrt(sum(!is.na(estimate))),
    .groups  = "drop"
  )

write_csv(group_means_tpc, "data-processed/group-means-tpc-traits.csv") ### these are the results in the paper


group_means_tpc |> 
  ggplot() + geom_pointrange(aes(x = evolution_history, y = mean_val, ymin = mean_val - se_mean, ymax = mean_val + se_mean)) +
  facet_wrap( ~ trait, scales = "free")



# ── Statistical tests ─────────────────────────────────────────────────────────
# Welch two-sample t-test (35 vs 40) + one-sample t-test (each group vs ancestor)
# Holm-corrected within each trait across the 3 comparisons.
# Uses point estimates from the SSH fits (topt, tmax, th_c, b80).

p_stars <- function(p) case_when(
  p < 0.001 ~ "***",
  p < 0.01  ~ "**",
  p < 0.05  ~ "*",
  TRUE      ~ "ns"
)

run_tests_tpc <- function(x, y = NULL, mu = NULL, label_x, label_y = "ancestor", trait) {
  if (!is.null(y)) {
    t_res <- tryCatch(t.test(x, y),                           error = function(e) NULL)
    w_res <- tryCatch(wilcox.test(x, y, exact = FALSE),       error = function(e) NULL)
    ref   <- mean(y)
  } else {
    t_res <- tryCatch(t.test(x, mu = mu),                     error = function(e) NULL)
    w_res <- tryCatch(wilcox.test(x, mu = mu, exact = FALSE), error = function(e) NULL)
    ref   <- mu
  }
  tibble(
    trait      = trait,
    comparison = paste(label_x, "vs", label_y),
    mean_x     = mean(x),
    ref        = ref,
    diff       = mean(x) - ref,
    ci_lo      = if (!is.null(t_res)) t_res$conf.int[1] else NA_real_,
    ci_hi      = if (!is.null(t_res)) t_res$conf.int[2] else NA_real_,
    p_welch    = if (!is.null(t_res)) t_res$p.value     else NA_real_,
    p_wilcox   = if (!is.null(w_res)) w_res$p.value     else NA_real_
  )
}

g35_tpc  <- tpc_boot_se |> filter(evolution_history == "35 evolved")
g40_tpc  <- tpc_boot_se |> filter(evolution_history == "40 evolved")
anc_vals <- tpc_boot_se |> filter(evolution_history == "fRS585")

tpc_stats <- bind_rows(
  run_tests_tpc(g35_tpc$topt, g40_tpc$topt,       label_x = "35 evolved", label_y = "40 evolved", trait = "topt"),
  run_tests_tpc(g35_tpc$topt, mu = anc_vals$topt,  label_x = "35 evolved",                        trait = "topt"),
  run_tests_tpc(g40_tpc$topt, mu = anc_vals$topt,  label_x = "40 evolved",                        trait = "topt"),
  run_tests_tpc(g35_tpc$tmax, g40_tpc$tmax,       label_x = "35 evolved", label_y = "40 evolved", trait = "tmax"),
  run_tests_tpc(g35_tpc$tmax, mu = anc_vals$tmax,  label_x = "35 evolved",                        trait = "tmax"),
  run_tests_tpc(g40_tpc$tmax, mu = anc_vals$tmax,  label_x = "40 evolved",                        trait = "tmax"),
  run_tests_tpc(g35_tpc$th_c, g40_tpc$th_c,       label_x = "35 evolved", label_y = "40 evolved", trait = "th_c"),
  run_tests_tpc(g35_tpc$th_c, mu = anc_vals$th_c,  label_x = "35 evolved",                        trait = "th_c"),
  run_tests_tpc(g40_tpc$th_c, mu = anc_vals$th_c,  label_x = "40 evolved",                        trait = "th_c"),
  run_tests_tpc(g35_tpc$b80,  g40_tpc$b80,        label_x = "35 evolved", label_y = "40 evolved", trait = "b80"),
  run_tests_tpc(g35_tpc$b80,  mu = anc_vals$b80,   label_x = "35 evolved",                        trait = "b80"),
  run_tests_tpc(g40_tpc$b80,  mu = anc_vals$b80,   label_x = "40 evolved",                        trait = "b80")
) |>
  group_by(trait) |>
  mutate(
    p_welch_holm  = p.adjust(p_welch,  method = "holm"),
    p_wilcox_holm = p.adjust(p_wilcox, method = "holm")
  ) |>
  ungroup()

cat("\n=== TPC trait statistical results (Holm-corrected) ===\n")
print(tpc_stats |> select(trait, comparison, diff, ci_lo, ci_hi, p_welch_holm))

# Bracket positions (group vs group) — above the highest point + SE bar
y_range <- plot_tpc |>
  group_by(trait) |>
  summarise(
    y_max  = max(estimate + se, na.rm = TRUE),
    y_span = diff(range(estimate, na.rm = TRUE)),
    .groups = "drop"
  )

bracket_df <- tpc_stats |>
  filter(comparison == "35 evolved vs 40 evolved") |>
  mutate(
    trait       = factor(trait, levels = TRAIT_LEVELS),
    xmin        = "35 evolved",
    xmax        = "40 evolved",
    annotations = p_stars(p_welch_holm)
  ) |>
  left_join(y_range, by = "trait") |>
  mutate(y_position = y_max + y_span * 0.06)

# Stars for each group vs ancestor — just above the group mean ± SE bar
anc_star_df <- tpc_stats |>
  filter(str_detect(comparison, "vs ancestor")) |>
  mutate(
    trait             = factor(trait, levels = TRAIT_LEVELS),
    evolution_history = factor(str_remove(comparison, " vs ancestor"),
                               levels = c("35 evolved", "40 evolved")),
    label             = p_stars(p_welch_holm)
  ) |>
  left_join(group_means_tpc, by = c("trait", "evolution_history")) |>
  left_join(y_range, by = "trait") |>
  mutate(y_pos = mean_val + se_mean + y_span * 0.04)

ggplot(plot_tpc,
       aes(x = evolution_history, y = estimate, color = evolution_history)) +
  geom_hline(
    data     = anc_refs_tpc,
    aes(yintercept = anc_val),
    linetype = "dashed", color = "#000000", linewidth = 0.6
  ) +
  # Per-strain estimate ± 1 bootstrap SE (small points, jittered)
  geom_pointrange(
    aes(ymin = estimate - se, ymax = estimate + se),
    position  = position_jitter(width = 0.15, seed = 42),
    size      = 0.3,
    linewidth = 0.5,
    alpha     = 0.65
  ) +
  # Group mean ± SE across strains (large summary point)
  geom_pointrange(
    data      = group_means_tpc,
    aes(y = mean_val, ymin = mean_val - se_mean, ymax = mean_val + se_mean),
    size      = 0.9,
    linewidth = 1.3
  ) +
  suppressWarnings(ggsignif::geom_signif(
    data       = bracket_df,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual     = TRUE, tip_length = 0.02, textsize = 4.5, color = "black"
  )) +
  geom_text(
    data     = anc_star_df,
    aes(x = evolution_history, y = y_pos, label = label),
    color    = "black", size = 4, fontface = "bold", nudge_x = 0.3
  ) +
  facet_wrap(~ trait, scales = "free_y", labeller = as_labeller(TRAIT_LABELS)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  scale_color_manual(values = EVO_COLORS) +
  labs(
    x       = NULL,
    y       = "Temperature (\u00b0C)",
    caption = paste(
      "Small points \u00b1 bar: per-strain estimate \u00b1 1 bootstrap SE",
      " |  Large points \u00b1 bar: group mean \u00b1 SE (across strains)",
      " |  Dashed: ancestor (fRS585)\n",
      "Bracket: Welch t-test (Holm-corrected)  |  Stars right of mean: vs. ancestor (one-sample t-test)"
    )
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text      = element_text(size = 13),
    plot.caption    = element_text(size = 8, color = "grey40")
  )

ggsave(
  file.path(FIGS, "thermal-traits-dotplot-boot-19.png"),
  width = 15, height = 7, dpi = 300
)

# ── 6b. IC50 dot plots — per-strain bootstrap SE (log scale) ──────────────────
# Error bars are symmetric on the log scale:
#   ymin = exp(log(ic50) - se_log_ic50), ymax = exp(log(ic50) + se_log_ic50)

y_labels <- c(
  fluconazole  = "Fluconazole IC50 (\u03bcg/mL, log scale)",
  caspofungin  = "Caspofungin IC50 (\u03bcg/mL, log scale)",
  amphotericin = "Amphotericin B IC50 (\u03bcg/mL, log scale)"
)

# ── IC50 statistical tests ─────────────────────────────────────────────────────
# Two-sample Welch t-test (35 vs 40) + one-sample t-test (each vs ancestor),
# both on log(IC50). Holm-corrected within each drug across the 3 comparisons.

ic50_stats <- bind_rows(lapply(
  c("fluconazole", "caspofungin", "amphotericin"),
  function(drg) {
    dat <- ic50_boot_se |> filter(drug == drg)
    g35 <- log(dat |> filter(evolution_history == "35 evolved") |> pull(ic50))
    g40 <- log(dat |> filter(evolution_history == "40 evolved") |> pull(ic50))
    anc <- log(dat |> filter(evolution_history == "fRS585")    |> pull(ic50))
    if (length(g35) < 2 || length(g40) < 2 || length(anc) == 0) return(tibble())

    ttest <- function(x, y_or_mu) {
      if (length(y_or_mu) == 1)
        tryCatch(t.test(x, mu = y_or_mu), error = function(e) NULL)
      else
        tryCatch(t.test(x, y_or_mu),      error = function(e) NULL)
    }
    make_row <- function(comparison, x, y_or_mu) {
      res <- ttest(x, y_or_mu)
      ref <- if (length(y_or_mu) == 1) y_or_mu else mean(y_or_mu)
      tibble(
        drug       = drg,
        comparison = comparison,
        diff       = mean(x) - ref,
        ci_lo      = if (!is.null(res)) res$conf.int[1] else NA_real_,
        ci_hi      = if (!is.null(res)) res$conf.int[2] else NA_real_,
        p_welch    = if (!is.null(res)) res$p.value     else NA_real_
      )
    }
    bind_rows(
      make_row("35 vs 40 evolved",       g35, g40),
      make_row("35 evolved vs ancestor", g35, anc),
      make_row("40 evolved vs ancestor", g40, anc)
    )
  }
)) |>
  group_by(drug) |>
  mutate(p_holm = p.adjust(p_welch, method = "holm")) |>
  ungroup()

cat("\n=== IC50 statistical results (Holm-corrected, log scale) ===\n")
print(ic50_stats |> select(drug, comparison, diff, ci_lo, ci_hi, p_holm))

for (drg in c("fluconazole", "caspofungin", "amphotericin")) {
  drug_data <- ic50_boot_se |>
    filter(drug == drg) |>
    mutate(
      ic50_lo_log = exp(log(ic50) - se_log_ic50),
      ic50_hi_log = exp(log(ic50) + se_log_ic50)
    )

  if (nrow(drug_data) == 0) next

  anc_val <- drug_data |>
    filter(evolution_history == "fRS585") |>
    pull(ic50)
  if (length(anc_val) == 0) anc_val <- NA_real_

  plot_df <- drug_data |>
    filter(evolution_history != "fRS585") |>
    mutate(evolution_history = factor(
      evolution_history, levels = c("35 evolved", "40 evolved")
    ))

  # Group geometric mean ± SE (on log scale, back-transformed)
  gmeans <- plot_df |>
    group_by(evolution_history) |>
    summarise(
      mean_log = mean(log(ic50), na.rm = TRUE),
      se_log   = sd(log(ic50),  na.rm = TRUE) / sqrt(sum(!is.na(ic50))),
      mean_val = exp(mean_log),
      ymin     = exp(mean_log - se_log),
      ymax     = exp(mean_log + se_log),
      .groups  = "drop"
    )

  # Bracket: 35 vs 40 evolved.
  # ggsignif with scale_y_log10 interprets y_position in log10 units
  # (transformed space), so we pass log10(natural_scale_target).
  p_bracket   <- ic50_stats |>
    filter(drug == drg, comparison == "35 vs 40 evolved") |>
    pull(p_holm)
  bracket_y   <- log10(max(plot_df$ic50, na.rm = TRUE) * 2.5)
  bracket_ann <- tibble(
    xmin        = "35 evolved",
    xmax        = "40 evolved",
    annotations = p_stars(p_bracket),
    y_position  = bracket_y
  )

  # Stars for each group vs ancestor — just above the group geometric mean SE bar.
  # geom_text respects the scale transformation; y_pos is in natural scale.
  star_ann <- ic50_stats |>
    filter(drug == drg, str_detect(comparison, "vs ancestor")) |>
    mutate(
      evolution_history = factor(str_remove(comparison, " vs ancestor"),
                                 levels = c("35 evolved", "40 evolved")),
      label = p_stars(p_holm)
    ) |>
    left_join(gmeans, by = "evolution_history") |>
    mutate(y_pos = ymax * 1.15)

  ggplot(plot_df,
         aes(x = evolution_history, y = ic50, color = evolution_history)) +
    geom_hline(
      yintercept = anc_val, linetype = "dashed",
      color = "#000000", linewidth = 0.6
    ) +
    geom_pointrange(
      aes(ymin = ic50_lo_log, ymax = ic50_hi_log),
      position  = position_jitter(width = 0.15, seed = 42),
      size      = 0.3, linewidth = 0.5, alpha = 0.65
    ) +
    geom_pointrange(
      data      = gmeans,
      aes(y = mean_val, ymin = ymin, ymax = ymax),
      size      = 0.9, linewidth = 1.3
    ) +
    suppressWarnings(ggsignif::geom_signif(
      data        = bracket_ann,
      aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
      manual      = TRUE, tip_length = 0.02, textsize = 4.5, color = "black"
    )) +
    geom_text(
      data        = star_ann,
      aes(x = evolution_history, y = y_pos, label = label),
      color       = "black", size = 4, fontface = "bold", nudge_x = 0.3
    ) +
    scale_y_log10(expand = expansion(mult = c(0.05, 0.25))) +
    scale_color_manual(values = EVO_COLORS) +
    labs(
      x       = NULL,
      y       = y_labels[[drg]],
      caption = paste(
        "Small points \u00b1 bar: per-strain pooled IC50 \u00b1 1 bootstrap SE (log-symmetric)",
        " |  Large points \u00b1 bar: geometric mean \u00b1 SE (across strains)",
        " |  Dashed: ancestor (fRS585)\n",
        "Bracket: Welch t-test (Holm-corrected)  |  Stars right of mean: vs. ancestor (one-sample t-test)"
      )
    ) +
    theme_bw(base_size = 13) +
    theme(
      legend.position = "none",
      plot.caption    = element_text(size = 8, color = "grey40")
    )

  ggsave(
    file.path(FIGS, sprintf("%s-ic50-dotplot-boot-19.png", drg)),
    width = 5, height = 6, dpi = 300
  )
}


# ── 6c. TPC per-strain CI ribbon figure ───────────────────────────────────────

tpc_ribbon_df <- bind_rows(tpc_ribbon_list)
tpc_pt_data   <- auc_data |> filter(strain %in% tpc_ribbon_df$strain)

# Order panels: ancestor first, then 35 evolved, then 40 evolved (alpha within)
strain_order <- tpc_ribbon_df |>
  distinct(strain, evolution_history) |>
  arrange(
    factor(evolution_history, levels = c("fRS585", "35 evolved", "40 evolved")),
    strain
  ) |>
  pull(strain)

tpc_ribbon_df <- tpc_ribbon_df |> mutate(strain = factor(strain, levels = strain_order))
tpc_pt_data   <- tpc_pt_data   |> mutate(strain = factor(strain, levels = strain_order))

ggplot() +
  geom_ribbon(
    data  = tpc_ribbon_df,
    aes(x = temperature, ymin = ci_lo, ymax = ci_hi, fill = evolution_history),
    alpha = 0.25
  ) +
  geom_line(
    data      = tpc_ribbon_df,
    aes(x = temperature, y = fitted, color = evolution_history),
    linewidth = 0.7
  ) +
  geom_point(
    data  = tpc_pt_data,
    aes(x = test_temperature, y = auc_gc, color = evolution_history),
    size  = 1.0, alpha = 0.7
  ) +
  facet_wrap(~ strain, scales = "free_y", ncol = 6) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  scale_fill_manual(values  = EVO_COLORS, name = NULL) +
  labs(
    x       = "Temperature (\u00b0C)",
    y       = "AUC (blank-corrected)",
    caption = "Ribbon: 95% bootstrap CI (1,000 residual resamples)  |  Points: per-well AUC"
  ) +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "none",
    strip.text      = element_text(size = 7),
    plot.caption    = element_text(size = 7, color = "grey40")
  )

ggsave(
  file.path(FIGS, "tpc-ci-ribbon-per-strain-19.png"),
  width = 18, height = 14, dpi = 200
)


# ── 6d. IC50 per-strain CI ribbon figures (one per drug) ──────────────────────

ic50_ribbon_df <- bind_rows(ic50_ribbon_list)

drug_xlabels <- c(
  fluconazole  = "Fluconazole (\u03bcg/mL, log scale)",
  caspofungin  = "Caspofungin (\u03bcg/mL, log scale)",
  amphotericin = "Amphotericin B (\u03bcg/mL, log scale)"
)

for (drg in c("fluconazole", "caspofungin", "amphotericin")) {
  ribbon_drg <- ic50_ribbon_df |> filter(drug == drg)
  if (nrow(ribbon_drg) == 0) next

  # Order panels: ancestor, then 35 evolved, then 40 evolved (alpha within)
  pop_order <- ribbon_drg |>
    distinct(population, evolution_history) |>
    arrange(
      factor(evolution_history, levels = c("fRS585", "35 evolved", "40 evolved")),
      population
    ) |>
    pull(population)

  ribbon_drg <- ribbon_drg |>
    mutate(population = factor(population, levels = pop_order))
  data_drg <- mic_data |>
    filter(drug == drg, population %in% pop_order) |>
    mutate(population = factor(population, levels = pop_order))

  ggplot() +
    geom_ribbon(
      data  = ribbon_drg,
      aes(x = concentration, ymin = ci_lo, ymax = ci_hi, fill = evolution_history),
      alpha = 0.25
    ) +
    geom_line(
      data      = ribbon_drg,
      aes(x = concentration, y = fitted, color = evolution_history),
      linewidth = 0.7
    ) +
    geom_point(
      data  = data_drg,
      aes(x = concentration, y = OD, color = evolution_history),
      size  = 0.9, alpha = 0.6
    ) +
    facet_wrap(~ population, scales = "free_y", ncol = 6) +
    scale_x_log10() +
    scale_color_manual(values = EVO_COLORS, name = NULL) +
    scale_fill_manual(values  = EVO_COLORS, name = NULL) +
    labs(
      x       = drug_xlabels[[drg]],
      y       = "OD (blank-corrected)",
      caption = "Ribbon: 95% bootstrap CI (1,000 residual resamples)  |  Points: per-well OD (all reps pooled)"
    ) +
    theme_bw(base_size = 9) +
    theme(
      legend.position = "none",
      strip.text      = element_text(size = 7),
      plot.caption    = element_text(size = 7, color = "grey40")
    )

  ggsave(
    file.path(FIGS, sprintf("%s-ci-ribbon-per-strain-19.png", drg)),
    width = 18, height = 14, dpi = 200
  )
}


# =============================================================================
# 7. Export
# =============================================================================

write_csv(tpc_boot_se,    file.path(OUT_DIR, "tpc-boot-se-19.csv"))
write_csv(ic50_boot_se,   "data-processed/ic50-boot-pooled-19.csv")
write_csv(tpc_ribbon_df,  file.path(OUT_DIR, "tpc-ci-ribbon-19.csv"))
write_csv(ic50_ribbon_df, "data-processed/ic50-ci-ribbon-19.csv")

cat("\nExported:\n",
    file.path(OUT_DIR, "tpc-boot-se-19.csv"),    "\n",
    "data-processed/ic50-boot-pooled-19.csv",     "\n",
    file.path(OUT_DIR, "tpc-ci-ribbon-19.csv"),   "\n",
    "data-processed/ic50-ci-ribbon-19.csv",        "\n")


# =============================================================================
# 8. Deming regression: th_c vs log(IC50) per drug
#
# Deming regression accounts for measurement error in both axes:
#   x = th_c (°C) — bootstrap SE from SSH TPC fit
#   y = log(IC50)  — bootstrap SE on log scale from dose-response fit
#
# Per-observation error variances (xvar, yvar) are passed directly to
# deming(), which implements orthogonal distance regression weighted by
# the error variance ratio at each point.
# =============================================================================

library(deming)

# Join th_c SE (TPC) with log-scale IC50 SE, one row per strain × drug
deming_dat <- tpc_boot_se |>
  select(strain, evolution_history, th_c, se_th) |>
  inner_join(
    ic50_boot_se |>
      select(population, drug, ic50, se_log_ic50),
    by = c("strain" = "population")
  ) |> 
  mutate(log_ic50 = log(ic50))

cat(sprintf(
  "\nDeming regression data: %d strain × drug combinations across %d drugs\n",
  nrow(deming_dat),
  n_distinct(deming_dat$drug)
))


# ── 8a. Fit Deming regression per drug (all evolution histories pooled) ────────

deming_results <- deming_dat |>
  group_by(drug) |>
  group_modify(function(d, key) {
    if (nrow(d) < 5) {
      message("  SKIP (n = ", nrow(d), "): ", key$drug)
      return(tibble())
    }
    fit <- tryCatch(
      deming(
        log_ic50 ~ th_c,
        data = d,
        xstd = se_th,       # per-strain SD of th (bootstrap SE, Kelvin = °C)
        ystd = se_log_ic50  # per-strain SD of log(IC50) (bootstrap SE)
      ),
      error = function(e) { message("  FAILED: ", key$drug, " — ", e$message); NULL }
    )
    if (is.null(fit)) return(tibble())

    cf <- coef(fit)   # named: "(Intercept)", "th_c"
    se <- fit$se      # unnamed length-2: intercept SE, slope SE
    t_slope <- cf["th_c"] / se[2]
    tibble(
      n            = nrow(d),
      intercept    = cf["(Intercept)"],
      se_intercept = se[1],
      slope        = cf["th_c"],
      se_slope     = se[2],
      t_slope      = t_slope,
      p_slope      = 2 * pt(abs(t_slope), df = nrow(d) - 2, lower.tail = FALSE)
    )
  }) |>
  ungroup()

cat("\n=== Deming regression: th_c vs log(IC50), pooled across evolution histories ===\n")
print(deming_results)


# ── 8b. Per-evolution-history Deming fits (for slope/elevation comparison) ────

deming_by_group <- deming_dat |>
  group_by(drug, evolution_history) |>
  group_modify(function(d, key) {
    if (nrow(d) < 4) return(tibble())
    fit <- tryCatch(
      deming(
        log_ic50 ~ th_c,
        data = d,
        xstd = se_th,
        ystd = se_log_ic50
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) return(tibble())
    cf <- coef(fit)
    se <- fit$se
    t_slope <- cf["th_c"] / se[2]
    tibble(
      n         = nrow(d),
      intercept = cf["(Intercept)"],
      slope     = cf["th_c"],
      se_slope  = se[2],
      t_slope   = t_slope,
      p_slope   = 2 * pt(abs(t_slope), df = nrow(d) - 2, lower.tail = FALSE)
    )
  }) |>
  ungroup()

cat("\n=== Deming regression by evolution history ===\n")
print(deming_by_group)


# ── 8c. Scatterplot with Deming fit per drug ───────────────────────────────────

# Prediction grid for pooled Deming line per drug
deming_lines <- deming_results |>
  inner_join(
    deming_dat |>
      group_by(drug) |>
      summarise(th_min = min(th_c), th_max = max(th_c), .groups = "drop"),
    by = "drug"
  ) |>
  group_by(drug) |>
  reframe(
    th_c         = seq(unique(th_min), unique(th_max), length.out = 200),
    log_ic50_hat = unique(intercept) + unique(slope) * th_c
  )

# Clip lines to data y-range (including error bars) to prevent axis blowout
y_data_range_pooled <- deming_dat |>
  group_by(drug) |>
  summarise(
    ylo = min(log_ic50 - se_log_ic50, na.rm = TRUE),
    yhi = max(log_ic50 + se_log_ic50, na.rm = TRUE),
    .groups = "drop"
  )

deming_lines_clipped <- deming_lines |>
  left_join(y_data_range_pooled, by = "drug") |>
  filter(log_ic50_hat >= ylo, log_ic50_hat <= yhi)

# Slope + 95% CI annotation, positioned top-left of each facet
pooled_label_df <- deming_results |>
  mutate(
    ci_lo = slope - 1.96 * se_slope,
    ci_hi = slope + 1.96 * se_slope,
    label = sprintf("slope = %.2f\n95%% CI [%.2f, %.2f]", slope, ci_lo, ci_hi)
  ) |>
  left_join(
    deming_dat |>
      group_by(drug) |>
      summarise(
        x_pos = min(th_c - se_th) + 0.02 * diff(range(th_c)),
        y_pos = max(log_ic50 + se_log_ic50),
        .groups = "drop"
      ),
    by = "drug"
  )

ggplot(deming_dat,
       aes(x = th_c, y = log_ic50, color = evolution_history)) +
  geom_errorbar(
    aes(ymin = log_ic50 - se_log_ic50, ymax = log_ic50 + se_log_ic50),
    width = 0, alpha = 0.5, linewidth = 0.4
  ) +
  geom_errorbarh(
    aes(xmin = th_c - se_th, xmax = th_c + se_th),
    height = 0, alpha = 0.5, linewidth = 0.4
  ) +
  geom_point(size = 2, alpha = 0.85) +
  geom_line(
    data        = deming_lines_clipped,
    aes(x = th_c, y = log_ic50_hat),
    inherit.aes = FALSE,
    color = "black", linewidth = 0.9
  ) +
  geom_text(
    data        = pooled_label_df,
    aes(x = x_pos, y = y_pos, label = label),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1, size = 3.2, color = "black", lineheight = 1.3
  ) +
  facet_wrap(~ drug, scales = "free") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x       = "Deactivation temperature Th (\u00b0C)",
    y       = "log(IC50)",
    caption = "Error bars: \u00b11 bootstrap SE on each axis  |  Line: Deming regression (pooled across evolution histories, per drug)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 12, face = "bold"),
    plot.caption    = element_text(size = 8, color = "grey40")
  )

ggsave(
  file.path(FIGS, "th-ic50-deming-19.png"),
  width = 13, height = 5, dpi = 300
)


# ── 8d. Within-group (centered) Deming regression ─────────────────────────────
#
# Group-mean center both th_c and log(IC50) within each drug × evolution_history
# combination. This removes the between-group signal so the regression asks:
# "among strains that evolved under the same regime, do those with higher Th
# also have higher IC50?" — i.e., the within-group relationship.

deming_centered <- deming_dat |>
  group_by(drug, evolution_history) |>
  mutate(
    th_c_c     = th_c    - mean(th_c),
    log_ic50_c = log_ic50 - mean(log_ic50)
  ) |>
  ungroup()

deming_within <- deming_centered |>
  group_by(drug) |>
  group_modify(function(d, key) {
    fit <- tryCatch(
      deming(log_ic50_c ~ th_c_c, data = d,
             xstd = se_th, ystd = se_log_ic50),
      error = function(e) NULL
    )
    if (is.null(fit)) return(tibble())
    cf <- coef(fit)
    se <- fit$se
    t_slope <- cf["th_c_c"] / se[2]
    tibble(
      n        = nrow(d),
      slope    = cf["th_c_c"],
      se_slope = se[2],
      t_slope  = t_slope,
      p_slope  = 2 * pt(abs(t_slope), df = nrow(d) - 2, lower.tail = FALSE)
    )
  }) |>
  ungroup()

cat("\n=== Within-group Deming (evolution history removed by centering) ===\n")
print(deming_within)

# Centered scatterplot with within-group Deming lines
within_lines <- deming_within |>
  inner_join(
    deming_centered |>
      group_by(drug) |>
      summarise(th_min = min(th_c_c), th_max = max(th_c_c), .groups = "drop"),
    by = "drug"
  ) |>
  group_by(drug) |>
  reframe(
    th_c_c       = seq(unique(th_min), unique(th_max), length.out = 200),
    log_ic50_hat = unique(slope) * th_c_c   # intercept ≈ 0 for centered data
  )

# Clip lines to the y-range of the data (including error bars) so the
# regression line doesn't blow out the axis on steep-slope facets
y_data_range <- deming_centered |>
  group_by(drug) |>
  summarise(
    ylo = min(log_ic50_c - se_log_ic50, na.rm = TRUE),
    yhi = max(log_ic50_c + se_log_ic50, na.rm = TRUE),
    .groups = "drop"
  )

within_lines_clipped <- within_lines |>
  left_join(y_data_range, by = "drug") |>
  filter(log_ic50_hat >= ylo, log_ic50_hat <= yhi)

# Slope + 95% CI text annotation, positioned in the top-left of each facet
label_df <- deming_within |>
  mutate(
    ci_lo = slope - 1.96 * se_slope,
    ci_hi = slope + 1.96 * se_slope,
    label = sprintf("slope = %.2f\n95%% CI [%.2f, %.2f]", slope, ci_lo, ci_hi)
  ) |>
  left_join(
    deming_centered |>
      group_by(drug) |>
      summarise(
        x_pos = min(th_c_c - se_th) + 0.02 * diff(range(th_c_c)),
        y_pos = max(log_ic50_c + se_log_ic50),
        .groups = "drop"
      ),
    by = "drug"
  )

ggplot(deming_centered,
       aes(x = th_c_c, y = log_ic50_c, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_errorbar(
    aes(ymin = log_ic50_c - se_log_ic50, ymax = log_ic50_c + se_log_ic50),
    width = 0, alpha = 0.4, linewidth = 0.4
  ) +
  geom_errorbarh(
    aes(xmin = th_c_c - se_th, xmax = th_c_c + se_th),
    height = 0, alpha = 0.4, linewidth = 0.4
  ) +
  geom_point(size = 2, alpha = 0.85) +
  geom_line(
    data      = within_lines_clipped,
    aes(x = th_c_c, y = log_ic50_hat),
    color     = "black", linewidth = 1.0
  ) +
  geom_text(
    data        = label_df,
    aes(x = x_pos, y = y_pos, label = label),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1, size = 3.2, color = "black", lineheight = 1.3
  ) +
  facet_wrap(~ drug, scales = "free") +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(
    x       = "\u0394Th (\u00b0C, group-mean centered)",
    y       = "\u0394log(IC50) (group-mean centered)",
    caption = paste(
      "Variables centered within each evolution history group to remove between-group confounding",
      " |  Error bars: \u00b11 bootstrap SE  |  Line: within-group Deming regression (pooled across groups)"
    )
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 12, face = "bold"),
    plot.caption    = element_text(size = 8, color = "grey40")
  )

ggsave(
  file.path(FIGS, "th-ic50-deming-within-19.png"),
  width = 13, height = 5, dpi = 300
)


# ── 8e. Export Deming results ─────────────────────────────────────────────────

write_csv(deming_dat,      "data-processed/deming-input-19.csv")
write_csv(deming_results,  "data-processed/deming-results-pooled-19.csv")
write_csv(deming_by_group, "data-processed/deming-results-by-group-19.csv")
write_csv(deming_centered, "data-processed/deming-centered-input-19.csv")
write_csv(deming_within,   "data-processed/deming-results-within-19.csv")

cat("\nDeming exports complete.\n")
