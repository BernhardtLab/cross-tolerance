# =============================================================================
# MIC / IC50 Dose-Response Analysis
# Converted from Python/Jupyter notebook
#
# Fits three-parameter logistic dose-response curves to MIC data and
# compares IC50 values between evolved N. glabratus populations and the
# ancestral strain.
#
# Input:  Table_S2.xlsx  (sheets: FLZ, CASP, AMP-B)
# Output: processed_outputs/   figures/
# =============================================================================

# ── Libraries ─────────────────────────────────────────────────────────────────
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(minpack.lm)   # nlsLM for robust nonlinear fitting

# ── Configuration ─────────────────────────────────────────────────────────────
DATA_FILE  <- "Table_S2.xlsx"
OUT        <- "processed_outputs"
FIGS       <- "figures"
dir.create(OUT,  showWarnings = FALSE)
dir.create(FIGS, showWarnings = FALSE)

DRUG_SHEETS <- c(
  FLZ   = "fluconazole",
  CASP  = "caspofungin",
  `AMP-B` = "amphotericin"
)

EVOLUTION_CATEGORIES <- c(
  "40_"    = "evolved_40C",
  "35_"    = "evolved_35C",
  "fRS585" = "fRS585_control"
)

EVO_COLORS <- c(
  evolved_40C    = "#FA3208",
  evolved_35C    = "#0E63FF",
  fRS585_control = "#000000"
)

EVO_LABELS <- c(
  evolved_40C    = "Evolved 40°C",
  evolved_35C    = "Evolved 35°C",
  fRS585_control = "Ancestor"
)

set.seed(42)

# =============================================================================
# 1. Helper functions
# =============================================================================

categorize_strain <- function(strain_name) {
  for (pattern in names(EVOLUTION_CATEGORIES)) {
    if (grepl(pattern, strain_name, fixed = TRUE))
      return(EVOLUTION_CATEGORIES[[pattern]])
  }
  "unknown"
}

# =============================================================================
# 2. Parse drug sheets
#
# Each sheet contains several plates identified by a "Set#_Rep#" header.
# Below each plate header there is a "Drug Concentration" row followed by
# strain rows with OD600 readings.
# =============================================================================

parse_drug_sheet <- function(sheet_name, drug_name) {
  raw <- suppressMessages(
    read_excel(DATA_FILE, sheet = sheet_name,
               col_names = FALSE, col_types = "text")
  )

  nr <- nrow(raw); nc <- ncol(raw)

  # Find all plate-header cells matching Set#_Rep#
  plate_locs <- list()
  for (r in seq_len(nr)) {
    for (c in seq_len(nc)) {
      val <- raw[[c]][r]
      if (length(val) == 0 || is.na(val)) next
      val <- as.character(val)
      if (nchar(val) > 0 && grepl("^Set\\d+_Rep\\d+$", val))
        plate_locs[[length(plate_locs) + 1]] <- list(r = r, c = c, id = val)
    }
  }
  if (length(plate_locs) == 0) return(data.frame())

  records <- list()

  for (plate_idx in seq_along(plate_locs)) {
    row0     <- plate_locs[[plate_idx]]$r
    col0     <- plate_locs[[plate_idx]]$c
    plate_id <- plate_locs[[plate_idx]]$id
    end_row  <- if (plate_idx < length(plate_locs))
                  plate_locs[[plate_idx + 1]]$r - 1 else nr

    # Extract the plate region (up to 20 columns wide)
    col_end  <- min(col0 + 19, nc)
    region_r <- row0:end_row
    region_c <- col0:col_end

    # Find the "Drug Concentration" row within the region
    conc_row_local <- NA; conc_col_local <- NA
    for (ri in seq_along(region_r)) {
      for (ci in seq_along(region_c)) {
        val <- raw[[region_c[ci]]][region_r[ri]]
        # Guard against NULL, NA, or length-0 before grepl
        if (length(val) == 0 || is.na(val)) next
        val <- as.character(val)
        if (nchar(val) > 0 && grepl("Drug Concentration", val, fixed = TRUE)) {
          conc_row_local <- ri; conc_col_local <- ci
          break
        }
      }
      if (!is.na(conc_row_local)) break
    }
    if (is.na(conc_row_local)) next

    # Extract concentration values from that row
    conc_row_abs <- region_r[conc_row_local]
    concs <- list()
    for (ci in conc_col_local:length(region_c)) {
      val <- suppressWarnings(as.numeric(raw[[region_c[ci]]][conc_row_abs]))
      if (!is.na(val) && val >= 0)
        concs[[length(concs) + 1]] <- list(col_local = ci, conc = val)
    }
    if (length(concs) == 0) next

    # Extract strain rows below the concentration row
    for (ri in (conc_row_local + 1):length(region_r)) {
      strain_val <- raw[[region_c[conc_col_local]]][region_r[ri]]
      if (is.na(strain_val)) next
      strain <- trimws(as.character(strain_val))
      if (strain == "" || strain == "nan" || strain == "Blank" ||
          grepl("Drug Concentration", strain, fixed = TRUE) ||
          grepl("^Set\\d+_Rep\\d+$", strain)) next

      for (conc_info in concs) {
        od_raw <- suppressWarnings(
          as.numeric(raw[[region_c[conc_info$col_local]]][region_r[ri]])
        )
        if (!is.na(od_raw)) {
          records[[length(records) + 1]] <- data.frame(
            strain        = strain,
            concentration = conc_info$conc,
            OD            = od_raw,
            plate_id      = plate_id,
            drug          = drug_name,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(records) == 0) return(data.frame())
  result <- bind_rows(records)
  result$evolution_history <- sapply(result$strain, categorize_strain)
  result
}


replace_zero_concentrations <- function(df) {
  # Replace 0-concentration wells with 1/10 of the lowest non-zero conc per plate
  for (pid in unique(df$plate_id)) {
    mask     <- df$plate_id == pid
    non_zero <- df$concentration[mask & df$concentration > 0]
    if (length(non_zero) > 0) {
      replacement <- min(non_zero) / 10
      df$concentration[mask & df$concentration == 0] <- replacement
    }
  }
  df
}

# =============================================================================
# 3. Load all drug sheets
# =============================================================================

all_data <- list()
for (sheet in names(DRUG_SHEETS)) {
  drug_name <- DRUG_SHEETS[[sheet]]
  df_drug   <- parse_drug_sheet(sheet, drug_name)
  if (nrow(df_drug) > 0) {
    all_data[[drug_name]] <- df_drug
    cat(sprintf("%s: %s records, %d strains\n",
                drug_name,
                format(nrow(df_drug), big.mark = ","),
                length(unique(df_drug$strain))))
  }
}

combined_data <- replace_zero_concentrations(
  bind_rows(all_data)
)

# =============================================================================
# 4. Fit dose-response curves
#
# 3-parameter logistic:  OD = d / (1 + exp(b * (log(C) - log(IC50))))
# d    = upper asymptote (max OD)
# b    = slope
# IC50 = half-maximal inhibitory concentration
#
# Bootstrap CI for IC50: n = 500 resamples with replacement
# =============================================================================

logistic_3pl <- function(concentration, d, b, ic50) {
  d / (1 + exp(b * (log(concentration) - log(ic50))))
}


fit_strain <- function(data, n_bootstrap = 500) {
  # Average technical replicates at each concentration
  avg   <- data %>%
    group_by(concentration) %>%
    summarise(OD = mean(OD, na.rm = TRUE), .groups = "drop")
  x <- avg$concentration
  y <- avg$OD
  valid <- is.finite(x) & is.finite(y) & (x > 0)
  x <- x[valid]; y <- y[valid]
  if (length(x) < 3) return(NULL)

  d_init  <- max(y)
  lo      <- c(d = 0,         b = 0.01, ic50 = min(x))
  hi      <- c(d = 2*d_init,  b = 50,   ic50 = 5*max(x))
  p0      <- list(d = d_init, b = 1.0,  ic50 = median(x))

  fit <- tryCatch(
    suppressWarnings(
      nlsLM(y ~ logistic_3pl(x, d, b, ic50),
            start   = p0,
            lower   = lo,
            upper   = hi,
            control = nls.lm.control(maxiter = 200, maxfev = 5000))
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  popt   <- coef(fit)
  y_pred <- logistic_3pl(x, popt["d"], popt["b"], popt["ic50"])
  ss_res <- sum((y - y_pred)^2)
  ss_tot <- sum((y - mean(y))^2)
  r2     <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_

  # Bootstrap CI for IC50
  boot_ic50 <- numeric(0)
  for (i in seq_len(n_bootstrap)) {
    idx <- sample(length(x), length(x), replace = TRUE)
    bp  <- tryCatch(
      suppressWarnings(
        coef(nlsLM(y[idx] ~ logistic_3pl(x[idx], d, b, ic50),
                   start   = as.list(popt),
                   lower   = lo,
                   upper   = hi,
                   control = nls.lm.control(maxiter = 100, maxfev = 2000)))
      ),
      error = function(e) NULL
    )
    if (!is.null(bp)) boot_ic50 <- c(boot_ic50, bp["ic50"])
  }

  ci_lo <- if (length(boot_ic50) > 0) quantile(boot_ic50, 0.025) else NA_real_
  ci_hi <- if (length(boot_ic50) > 0) quantile(boot_ic50, 0.975) else NA_real_

  list(
    d          = popt["d"],
    b          = popt["b"],
    ic50       = popt["ic50"],
    ic50_ci_lo = as.numeric(ci_lo),
    ic50_ci_hi = as.numeric(ci_hi),
    r2         = r2,
    x_data     = x,
    y_data     = y
  )
}

# =============================================================================
# 5. Run fitting for every strain × drug combination
# =============================================================================

combos <- combined_data %>%
  distinct(strain, drug)

results_list <- list()
for (i in seq_len(nrow(combos))) {
  strain <- combos$strain[i]
  drug   <- combos$drug[i]
  sub    <- combined_data %>%
    filter(strain == !!strain, drug == !!drug)
  fit <- fit_strain(sub)
  if (!is.null(fit)) {
    evo <- sub$evolution_history[1]
    results_list[[length(results_list) + 1]] <- data.frame(
      strain            = strain,
      drug              = drug,
      evolution_history = evo,
      d                 = fit$d,
      b                 = fit$b,
      ic50              = fit$ic50,
      ic50_ci_lo        = fit$ic50_ci_lo,
      ic50_ci_hi        = fit$ic50_ci_hi,
      r2                = fit$r2,
      stringsAsFactors  = FALSE
    )
  }
}

results_df <- bind_rows(results_list)
cat(sprintf("\nFitted %d strain × drug combinations\n", nrow(results_df)))

# =============================================================================
# 6. Export results
# =============================================================================

# IC50 per strain (long format)
results_df %>%
  select(strain, drug, evolution_history,
         ic50, ic50_ci_lo, ic50_ci_hi, r2) %>%
  write.csv(file.path(OUT, "ic50_per_strain.csv"), row.names = FALSE)

# Wide format: one column per evolution history per drug (Prism-friendly)
for (drug in unique(results_df$drug)) {
  sub  <- results_df %>% filter(drug == !!drug)
  wide <- list()
  for (evo in unique(sub$evolution_history)) {
    vals <- sub %>% filter(evolution_history == evo) %>% pull(ic50)
    wide[[paste0(drug, "_", evo)]] <- vals
  }
  max_len <- max(sapply(wide, length))
  wide    <- lapply(wide, function(v) c(v, rep(NA, max_len - length(v))))
  as.data.frame(wide) %>%
    write.csv(file.path(OUT, paste0("ic50_prism_", drug, ".csv")),
              row.names = FALSE)
}

cat("Exported:\n")
for (f in list.files(OUT)) cat(sprintf("  %s\n", f))

# =============================================================================
# 7. Dose-response curve figure
# =============================================================================

drug_order  <- c("amphotericin", "caspofungin", "fluconazole")
drug_labels <- c(
  fluconazole  = "Fluconazole",
  caspofungin  = "Caspofungin",
  amphotericin = "Amphotericin-B"
)

# Build smooth fitted curves for each strain
smooth_curves <- list()
raw_avg_list  <- list()

for (i in seq_len(nrow(results_df))) {
  row    <- results_df[i, ]
  strain <- row$strain
  drug   <- row$drug

  raw <- combined_data %>%
    filter(strain == !!strain, drug == !!drug) %>%
    group_by(concentration) %>%
    summarise(OD = mean(OD, na.rm = TRUE), .groups = "drop")

  raw_avg_list[[length(raw_avg_list) + 1]] <- raw %>%
    mutate(strain = strain, drug = drug,
           evolution_history = row$evolution_history)

  x_min    <- min(raw$concentration)
  x_max    <- max(raw$concentration)
  x_smooth <- exp(seq(log(x_min), log(x_max), length.out = 100))
  y_smooth <- logistic_3pl(x_smooth, row$d, row$b, row$ic50)

  smooth_curves[[length(smooth_curves) + 1]] <- data.frame(
    strain            = strain,
    drug              = drug,
    evolution_history = row$evolution_history,
    concentration     = x_smooth,
    OD                = y_smooth
  )
}

raw_avg    <- bind_rows(raw_avg_list)
curve_data <- bind_rows(smooth_curves)

# Factor drug for facet order
raw_avg$drug    <- factor(raw_avg$drug,    levels = drug_order)
curve_data$drug <- factor(curve_data$drug, levels = drug_order)

p_dose <- ggplot() +
  # Individual strain fitted curves
  geom_line(data = curve_data,
            aes(x = concentration, y = OD,
                group = strain, color = evolution_history),
            linewidth = 0.9, alpha = 0.7) +
  # Raw averaged data points
  geom_point(data = raw_avg,
             aes(x = concentration, y = OD,
                 group = strain, color = evolution_history),
             size = 1.5, alpha = 0.6) +
  scale_x_log10() +
  scale_color_manual(values = EVO_COLORS, labels = EVO_LABELS, name = NULL) +
  facet_wrap(~ drug, nrow = 1,
             labeller = labeller(drug = drug_labels)) +
  labs(x = expression(bold("Drug Concentration (" * mu * "g/mL)")), y = "OD600") +
  theme_bw(base_size = 13) +
  theme(
    strip.text       = element_text(face = "bold", size = 14),
    axis.title       = element_text(face = "bold", size = 13),
    axis.text        = element_text(face = "bold", size = 11),
    legend.position  = "top",
    legend.text      = element_text(face = "bold", size = 11),
    panel.grid       = element_blank(),
    axis.line        = element_line(linewidth = 0.8),
    panel.border     = element_rect(linewidth = 1.5)
  )

ggsave(file.path(FIGS, "dose_response_curves.png"), p_dose,
       width = 15, height = 6, dpi = 300)

# PDF: use quartz on macOS (no XQuartz needed), cairo_pdf elsewhere
if (.Platform$OS.type == "unix" && Sys.info()[["sysname"]] == "Darwin") {
  quartz(type = "pdf", file = file.path(FIGS, "dose_response_curves.pdf"),
         width = 15, height = 6)
  print(p_dose)
  dev.off()
} else {
  ggsave(file.path(FIGS, "dose_response_curves.pdf"), p_dose,
         width = 15, height = 6, device = cairo_pdf)
}

