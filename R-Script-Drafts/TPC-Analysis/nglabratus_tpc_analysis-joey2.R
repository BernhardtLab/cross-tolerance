# =============================================================================
# N. glabratus Thermal Performance Curve Analysis
# Converted from Python/Jupyter notebook
#
# Fits thermal performance curves (TPCs) for N. glabratus populations
# experimentally evolved at 40°C or 35°C, plus the ancestral strain (fRS585),
# assayed at 25°C, 35°C, 38°C, 41°C, and 42°C.
#
# Input:  Table_S1.xlsx
# Output: Processed_Outputs/
# =============================================================================

# ── Libraries ─────────────────────────────────────────────────────────────────
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(lme4)
library(car)       # for Wald chi-sq tests
library(minpack.lm)  # nlsLM for robust nonlinear fitting

# ── Configuration ─────────────────────────────────────────────────────────────
DATA_PATH  <- "R-Script-Drafts/TPC-Analysis/Table_S1.xlsx"
OUT        <- "R-Script-Drafts/TPC-Analysis/Processed_Outputs"
dir.create(OUT, showWarnings = FALSE)

# Boltzmann constant (eV K⁻¹) and reference temperature (15°C in Kelvin)
K_B      <- 8.617333e-5
TREF_K   <- 288.15
CTMAX_CAP <- 50.0
N_STARTS  <- 500
set.seed(42)

EVO_COLORS <- c(
  "40°C Evolved" = "#FA3208",
  "35°C Evolved" = "#0E63FF",
  "Ancestor"     = "#000000"
)

# ── Helper: convert time values to decimal hours ───────────────────────────────
# Excel stores times as fractions of a day (e.g. 0.5 = 12:00).
# readxl returns them as numeric fractions when col_types = "numeric".
# Multiply by 24 to get hours.
time_to_hours <- function(x) {
  as.numeric(x) * 24
}

# =============================================================================
# 1. Parse strain key from 'Key' sheet
# =============================================================================
# The Key sheet has three side-by-side block tables starting at row 7.
# Columns: (loc, strain) repeated for blocks 1, 2, 3.
key_raw <- read_excel(DATA_PATH, sheet = "Key", col_names = FALSE,
                      skip = 6)

well_to_strain <- list()

for (i in seq_len(nrow(key_raw))) {
  row <- key_raw[i, ]
  # Block 1: cols 1-2, Block 2: cols 3-4, Block 3: cols 5-6
  for (block_idx in 1:3) {
    loc_col    <- (block_idx - 1) * 2 + 1
    strain_col <- (block_idx - 1) * 2 + 2
    loc    <- row[[loc_col]]
    strain <- row[[strain_col]]
    if (is.na(loc) || is.na(strain)) next
    if (strain == "Strain Dropped") next
    wells <- trimws(strsplit(as.character(loc), ",")[[1]])
    for (w in wells) {
      key <- paste0(block_idx, "_", w)
      well_to_strain[[key]] <- as.character(strain)
    }
  }
}

cat("Well→strain mappings:", length(well_to_strain), "\n")

# =============================================================================
# 2. Parse OD600 time-series from temperature sheets
# =============================================================================
parse_temp_sheet <- function(sheet_name, temp_label) {
  # Read as text first to find BLOCK markers, then re-read for values
  raw <- suppressMessages(
    read_excel(DATA_PATH, sheet = sheet_name, col_names = FALSE,
               col_types = "text")
  )

  n_rows <- nrow(raw)
  block_starts <- which(
    !is.na(raw[[1]]) & grepl("^BLOCK", raw[[1]], ignore.case = TRUE)
  )

  records <- list()

  for (bi in seq_along(block_starts)) {
    start_idx  <- block_starts[bi]
    block_num  <- as.integer(gsub("[^0-9]", "", raw[[1]][start_idx]))
    header_idx <- start_idx + 1
    end_idx    <- if (bi < length(block_starts)) block_starts[bi + 1] - 1 else n_rows

    header <- raw[header_idx, ]
    # Well columns start at col 3 (col 1 = time, col 2 = unused label)
    well_cols <- which(!is.na(header) & seq_along(header) >= 3)
    well_names <- as.character(header[well_cols])

    data_rows <- (header_idx + 1):end_idx
    if (length(data_rows) == 0) next

    for (ri in data_rows) {
      row <- raw[ri, ]
      if (is.na(row[[1]])) next
      # Times are stored as fractional days by Excel → convert to hours
      t_raw <- suppressWarnings(as.numeric(row[[1]]))
      if (is.na(t_raw)) next
      t_h <- t_raw * 24   # fraction of day → hours

      for (j in seq_along(well_cols)) {
        ci     <- well_cols[j]
        well   <- well_names[j]
        od_raw <- suppressWarnings(as.numeric(row[[ci]]))
        if (is.na(od_raw)) next
        key    <- paste0(block_num, "_", well)
        strain <- well_to_strain[[key]]
        if (is.null(strain)) next
        records[[length(records) + 1]] <- data.frame(
          block  = block_num,
          well   = well,
          strain = strain,
          time_h = t_h,
          OD     = od_raw,
          temp   = temp_label,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(records) == 0) return(NULL)
  df <- bind_rows(records)
  cat(sprintf("  %s: %s rows\n", sheet_name, format(nrow(df), big.mark = ",")))
  df
}

temp_sheets <- list(
  list(sheet = "25C",  label = 25),
  list(sheet = "35C",  label = 35),
  list(sheet = "38C",  label = 38),
  list(sheet = "41C",  label = 41),
  list(sheet = "42C",  label = 42)
)

temp_dfs <- map(temp_sheets, ~ parse_temp_sheet(.x$sheet, .x$label))
df_all   <- bind_rows(temp_dfs) %>%
  filter(!is.na(strain)) %>%
  mutate(OD = pmax(OD, 1e-4))

# Assign evolutionary history
assign_evo <- function(strain) {
  case_when(
    grepl("^40_", strain)   ~ "40°C Evolved",
    grepl("^35_", strain)   ~ "35°C Evolved",
    grepl("fRS585", strain) ~ "Ancestor",
    TRUE                    ~ "Other"
  )
}

df_all <- df_all %>%
  mutate(evo_history = assign_evo(strain))

cat(sprintf("Total rows: %s\n", format(nrow(df_all), big.mark = ",")))

# =============================================================================
# 3. Plot raw OD600 growth curves (QC)
# =============================================================================
PRIMARY <- c("40°C Evolved", "35°C Evolved", "Ancestor")

plot_df <- df_all %>%
  filter(evo_history %in% PRIMARY) %>%
  group_by(strain, evo_history, time_h, temp) %>%
  summarise(OD = mean(OD), .groups = "drop")

p_raw <- ggplot(plot_df, aes(x = time_h, y = OD,
                              group = strain, color = evo_history)) +
  geom_line(alpha = 0.55, linewidth = 0.8) +
  facet_wrap(~ temp, labeller = label_bquote(.(temp) * "°C")) +
  scale_color_manual(values = EVO_COLORS, name = NULL) +
  labs(x = "Time (h)", y = "OD600",
       title = "Raw OD600 Growth Curves") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

ggsave(file.path(OUT, "qc_raw_curves.png"), p_raw,
       width = 13, height = 8, dpi = 150)
print(p_raw)

# =============================================================================
# 4. Estimate growth rates (3-parameter logistic, fallback sliding window)
# =============================================================================

fit_growth_logistic <- function(time_h, OD, min_points = 6) {
  # 3-parameter logistic: K / (1 + exp(-r*(t - t_mid)))
  logistic3 <- function(t, K, r, t_mid) {
    K / (1 + exp(-r * (t - t_mid)))
  }

  n <- length(time_h)
  if (n < min_points) return(c(mu = NA_real_, r2 = NA_real_, lag_h = NA_real_))

  OD_max  <- max(OD);  OD_min <- min(OD)
  t_range <- tail(time_h, 1) - time_h[1]

  # Numerical gradient for initial guesses
  grad   <- diff(OD) / diff(time_h)
  grad_t <- (head(time_h, -1) + tail(time_h, -1)) / 2
  K0     <- OD_max * 1.05
  t_mid0 <- grad_t[which.max(grad)]
  r0     <- max(4 * max(grad) / K0, 0.05)

  result <- tryCatch({
    fit <- nlsLM(
      OD ~ logistic3(time_h, K, r, t_mid),
      start  = list(K = K0, r = r0, t_mid = t_mid0),
      lower  = c(K = OD_min, r = 0.005, t_mid = -t_range),
      upper  = c(K = OD_max * 3, r = 20.0, t_mid = tail(time_h, 1) + t_range),
      control = nls.lm.control(maxiter = 200, maxfev = 10000)
    )
    K_fit   <- coef(fit)[["K"]]
    r_fit   <- coef(fit)[["r"]]
    t_mid_fit <- coef(fit)[["t_mid"]]

    if (K_fit < OD_min || r_fit <= 0) stop("Implausible params")

    pred   <- predict(fit)
    ss_res <- sum((OD - pred)^2)
    ss_tot <- sum((OD - mean(OD))^2)
    r2     <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_

    # mu (day⁻¹), R², lag (h)
    c(mu    = r_fit * K_fit / 4 * 24,
      r2    = r2,
      lag_h = max(t_mid_fit - 2.0 / r_fit, 0.0))
  }, error = function(e) NULL)

  if (!is.null(result)) return(result)

  # ── Fallback: sliding-window max slope on ln(OD) ──────────────────────────
  ln_OD    <- log(pmax(OD, 1e-6))
  best_mu  <- NA_real_
  best_r2  <- NA_real_

  for (start in seq_len(n - 4)) {
    for (end in (start + 4):n) {
      x <- time_h[start:end]
      y <- ln_OD[start:end]
      if (diff(range(x)) == 0) next
      fit_lm <- lm(y ~ x)
      slope  <- coef(fit_lm)[["x"]]
      r2_sw  <- summary(fit_lm)$r.squared
      if (is.na(best_mu) || slope > best_mu) {
        best_mu <- slope * 24
        best_r2 <- r2_sw
      }
    }
  }

  c(mu = best_mu, r2 = best_r2, lag_h = NA_real_)
}

# Run on every well
gr_list <- df_all %>%
  group_by(strain, evo_history, block, well, temp) %>%
  arrange(time_h) %>%
  group_map(function(grp, keys) {
    res <- fit_growth_logistic(grp$time_h, grp$OD)
    bind_cols(keys, as.data.frame(t(res)))
  })

growth_rates <- bind_rows(gr_list)
cat(sprintf("Growth rates estimated: %s wells\n",
            format(nrow(growth_rates), big.mark = ",")))
print(
  growth_rates %>%
    group_by(temp) %>%
    summarise(
      n    = n(),
      mean = round(mean(mu, na.rm = TRUE), 3),
      sd   = round(sd(mu, na.rm = TRUE), 3),
      min  = round(min(mu, na.rm = TRUE), 3),
      max  = round(max(mu, na.rm = TRUE), 3)
    )
)

# Average across replicate wells → one value per strain × block × temperature
mu_mean <- growth_rates %>%
  group_by(strain, evo_history, block, temp) %>%
  summarise(mu      = mean(mu, na.rm = TRUE),
            n_wells = sum(!is.na(mu)),
            .groups = "drop")

cat(sprintf("mu_mean rows: %d\n", nrow(mu_mean)))

# =============================================================================
# 5. Sharpe–Schoolfield High model + Topt / CTmax extraction
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

  # Try zero-crossing above Topt (never happens for SSH, but included for parity)
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

# ── Multistart bounds ─────────────────────────────────────────────────────────
BOUNDS_LO <- c(r_tref = 0.01, e = 0.01, eh =  0.5, th = 303.0)
BOUNDS_HI <- c(r_tref = 50.0, e =  3.0, eh = 50.0, th = 335.0)
START_LO  <- c(r_tref =  0.5, e =  0.1, eh =  0.5, th = 305.0)
START_HI  <- c(r_tref = 20.0, e =  2.0, eh = 20.0, th = 325.0)

T_pred <- seq(15, 50, length.out = 300)

fit_params_ssh <- list()
fit_preds_ssh  <- list()

for (sid in unique(mu_mean$strain)) {
  d     <- mu_mean %>% filter(strain == sid)
  evo   <- d$evo_history[1]
  temps <- as.numeric(d$temp)
  rates <- as.numeric(d$mu)

  best_popt <- NULL
  best_rss  <- Inf

  # 500 random starts
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

  if (is.null(best_popt)) {
    cat(sprintf("  FAILED: %s\n", sid))
    next
  }

  ss_tot <- sum((rates - mean(rates))^2)
  r2     <- if (ss_tot > 0) 1 - best_rss / ss_tot else NA_real_

  derived <- calc_topt_ctmax(best_popt)
  topt    <- derived["topt"]
  ctmax   <- derived["ctmax"]
  rmax    <- derived["rmax"]
  if (is.na(ctmax)) ctmax <- CTMAX_CAP

  fit_params_ssh[[length(fit_params_ssh) + 1]] <- data.frame(
    strain      = sid,
    evo_history = evo,
    r_tref      = best_popt["r_tref"],
    e           = best_popt["e"],
    eh          = best_popt["eh"],
    th          = best_popt["th"],
    topt        = topt,
    ctmax       = ctmax,
    rmax        = rmax,
    r2          = r2,
    row.names   = NULL
  )

  pred_rows <- data.frame(
    strain      = sid,
    evo_history = evo,
    temp        = T_pred,
    pred        = sharpeschoolhigh(T_pred,
                                    best_popt["r_tref"], best_popt["e"],
                                    best_popt["eh"],     best_popt["th"])
  )
  fit_preds_ssh[[length(fit_preds_ssh) + 1]] <- pred_rows
}

tpc_params <- bind_rows(fit_params_ssh)
tpc_preds  <- bind_rows(fit_preds_ssh)


tpc_params |> 
  ggplot(aes(x = evo_history, y = ctmax)) + geom_point()

library(plotrix)
tpc_params_sum <- tpc_params |> 
  group_by(evo_history) |> 
  summarise(mean_tmax = mean(ctmax),
            se_tmax = std.error(ctmax))



tpc_params |> 
  ggplot(aes(x = evo_history, y = ctmax)) + geom_point() +
  geom_pointrange(aes(x = evo_history, y = mean_tmax, ymin = mean_tmax - se_tmax, ymax = mean_tmax + se_tmax), data = tpc_params_sum, color = "blue")
ggsave("R-Script-Drafts/TPC-Analysis/Processed_Outputs/tmax-means.png", width = 6, height = 4)



# Flag poor fits (R² < 0.9)
bad_strains      <- tpc_params %>% filter(r2 < 0.9) %>% pull(strain)
tpc_params_clean <- tpc_params %>% filter(!strain %in% bad_strains)

cat(sprintf("Fitted:              %d strains\n", nrow(tpc_params)))
cat(sprintf("Poor fits (R²<0.9): %s\n",
            if (length(bad_strains)) paste(bad_strains, collapse = ", ") else "none"))
cat(sprintf("Retained:           %d strains\n", nrow(tpc_params_clean)))
cat("\nMean Topt and CTmax by group:\n")
print(
  tpc_params_clean %>%
    group_by(evo_history) %>%
    summarise(mean_topt  = round(mean(topt,  na.rm = TRUE), 2),
              mean_ctmax = round(mean(ctmax, na.rm = TRUE), 2))
)

# =============================================================================
# 6. SSH QC plot — per-strain fit grid
# =============================================================================
plot_ssh_grid <- function(strain_subset, filename, title_text) {
  n_strains <- nrow(strain_subset)
  ncols     <- 6
  nrows     <- ceiling(n_strains / ncols)

  plot_list <- list()
  for (i in seq_len(n_strains)) {
    row_i  <- strain_subset[i, ]
    sid    <- row_i$strain
    color  <- EVO_COLORS[row_i$evo_history]
    is_bad <- sid %in% bad_strains
    obs    <- mu_mean %>% filter(strain == sid)
    prd    <- tpc_preds %>% filter(strain == sid) %>%
      mutate(pred = pmax(pred, -2))

    lbl <- sprintf("%s\nR²=%.2f%s", sid, row_i$r2, if (is_bad) " ⚠" else "")

    p <- ggplot() +
      geom_line(data = prd, aes(x = temp, y = pred),
                color = color,
                linewidth = if (is_bad) 0.8 else 1.2,
                linetype  = if (is_bad) "dashed" else "solid") +
      geom_point(data = obs, aes(x = temp, y = mu),
                 color = color, size = 1.8, alpha = 0.9) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
      labs(title = lbl) +
      theme_bw(base_size = 7) +
      theme(plot.title = element_text(face = "bold", size = 6),
            axis.title = element_blank())

    plot_list[[i]] <- p
  }

  # Use patchwork if available, otherwise cowplot
  if (requireNamespace("patchwork", quietly = TRUE)) {
    library(patchwork)
    combined <- wrap_plots(plot_list, ncol = ncols) +
      plot_annotation(title = title_text,
                      theme = theme(plot.title = element_text(face = "bold",
                                                               size = 14)))
    ggsave(filename, combined,
           width = ncols * 3.2, height = nrows * 3, dpi = 150,
           limitsize = FALSE)
    cat(sprintf("Saved: %s\n", filename))
  } else {
    message("Install 'patchwork' for the SSH QC grid plot.")
  }
}

plot_ssh_grid(tpc_params,
              file.path(OUT, "fig_ssh_all_strains.png"),
              "Sharpe–Schoolfield High Fits — All Strains")

# =============================================================================
# 7. TPC overlay figure
# =============================================================================
LEGEND_LABELS <- c(
  "40°C Evolved" = "40°C",
  "35°C Evolved" = "35°C",
  "Ancestor"     = "Ancestor"
)

# Individual strain curves (clean only)
indiv_curves <- tpc_preds %>%
  filter(evo_history %in% PRIMARY,
         !strain %in% bad_strains) %>%
  mutate(pred = ifelse(pred < 0, NA_real_, pred))

# Group mean curves
mean_curves <- tpc_params_clean %>%
  filter(evo_history %in% PRIMARY) %>%
  group_by(evo_history) %>%
  summarise(r_tref = mean(r_tref), e = mean(e),
            eh = mean(eh), th = mean(th), .groups = "drop") %>%
  rowwise() %>%
  mutate(data = list(data.frame(
    temp = T_pred,
    pred = sharpeschoolhigh(T_pred, r_tref, e, eh, th)
  ))) %>%
  unnest(data)

p_tpc <- ggplot() +
  geom_line(data = indiv_curves,
            aes(x = temp, y = pred, group = strain, color = evo_history),
            alpha = 0.22, linewidth = 1.2) +
  geom_line(data = mean_curves,
            aes(x = temp, y = pred, color = evo_history),
            linewidth = 3.0) +
  geom_point(data = mu_mean %>% filter(evo_history %in% PRIMARY),
             aes(x = temp, y = mu, color = evo_history),
             size = 3, alpha = 0.55) +
  scale_color_manual(values = EVO_COLORS,
                     labels = LEGEND_LABELS, name = NULL) +
  coord_cartesian(xlim = c(24, 50), ylim = c(0, 10)) +
  labs(x = "Temperature (°C)", y = "Growth Rate (day⁻¹)",
       title = "Thermal Performance Curves") +
  theme_bw(base_size = 18) +
  theme(legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        axis.text  = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

ggsave(file.path(OUT, "fig1b_overlay_primary.png"), p_tpc,
       width = 10, height = 7, dpi = 300)
print(p_tpc)
cat("Saved: fig1b_overlay_primary.png\n")

# =============================================================================
# 8. Linear mixed-effects model
# =============================================================================
gr_lme <- growth_rates %>%
  filter(evo_history %in% c("35°C Evolved", "40°C Evolved", "Ancestor")) %>%
  mutate(
    temp_fac   = factor(as.character(temp)),
    evo_history = relevel(factor(evo_history), ref = "35°C Evolved"),
    temp_fac   = relevel(temp_fac, ref = "25")
  )

lme_model <- lmer(
  mu ~ evo_history * temp_fac + (1 | strain),
  data = gr_lme,
  REML = TRUE
)

wald_result <- Anova(lme_model, type = 3, test.statistic = "Chisq")
print(wald_result)

p_int <- wald_result["evo_history:temp_fac", "Pr(>Chisq)"]
cat(sprintf("\nInteraction p = %.2e\n", p_int))

# =============================================================================
# 9. Topt vs Thermal Performance Retention (TPR)
# =============================================================================
tpr <- mu_mean %>%
  filter(evo_history %in% PRIMARY,
         temp %in% c(38, 41),
         !strain %in% bad_strains) %>%
  group_by(strain, evo_history, temp) %>%
  summarise(mu = mean(mu, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = temp, values_from = mu,
              names_prefix = "growth_") %>%
  rename(growth_38C = growth_38, growth_41C = growth_41) %>%
  mutate(TPR = growth_41C / growth_38C)

scatter_df <- tpc_params_clean %>%
  filter(evo_history %in% PRIMARY) %>%
  select(strain, evo_history, topt) %>%
  inner_join(tpr %>% select(strain, TPR), by = "strain")

r_p <- cor.test(scatter_df$topt, scatter_df$TPR, method = "pearson")
cat(sprintf("Pearson r = %.3f, p = %.4f\n", r_p$estimate, r_p$p.value))

sig_label <- dplyr::case_when(
  r_p$p.value < 0.0001 ~ "****",
  r_p$p.value < 0.001  ~ "***",
  r_p$p.value < 0.01   ~ "**",
  r_p$p.value < 0.05   ~ "*",
  TRUE                  ~ "ns"
)
annot <- sprintf("r = %.2f, %s", r_p$estimate, sig_label)

p_tpr <- ggplot(scatter_df, aes(x = topt, y = TPR,
                                  color = evo_history)) +
  geom_smooth(method = "lm", se = FALSE,
              color = "grey60", linetype = "dashed", linewidth = 1.0,
              data = scatter_df) +
  geom_point(size = 3, alpha = 0.75) +
  annotate("text",
           x = min(scatter_df$topt) - 0.1,
           y = max(scatter_df$TPR, na.rm = TRUE) * 0.98,
           label = annot, hjust = 0,
           color = "grey40", size = 7, fontface = "bold") +
  scale_color_manual(values = EVO_COLORS,
                     labels = LEGEND_LABELS, name = NULL) +
  labs(
    x = "Optimal Temperature (°C)",
    y = "Thermal Performance Retention\n(Growth at 41°C / Growth at 38°C)"
  ) +
  theme_bw(base_size = 16) +
  theme(legend.position = "right",
        legend.background = element_blank(),
        axis.text  = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

ggsave(file.path(OUT, "fig_topt_vs_tpr.png"), p_tpr,
       width = 8, height = 7, dpi = 300)
print(p_tpr)
cat("Saved: fig_topt_vs_tpr.png\n")

# =============================================================================
# 10. Export CSVs
# =============================================================================

# Per-strain Topt and CTmax
tpc_params_clean %>%
  filter(evo_history %in% PRIMARY) %>%
  select(strain, evo_history, topt, ctmax) %>%
  arrange(evo_history, strain) %>%
  rename(T_opt_C = topt, CT_max_C = ctmax) %>% 
  write.csv(file.path(OUT, "prism_topt_ctmax_per_strain.csv"), row.names = FALSE)


# Growth rates at 41°C and 42°C per strain
prism_hot <- growth_rates %>%
  filter(evo_history %in% PRIMARY,
         temp %in% c(41, 42),
         !strain %in% bad_strains) %>%
  group_by(strain, evo_history, temp) %>%
  summarise(mu = mean(mu, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = temp, values_from = mu,
              names_prefix = "growth_") %>%
  rename(growth_41C = growth_41, growth_42C = growth_42)

write.csv(prism_hot,
          file.path(OUT, "prism_growth_41_42_per_strain.csv"),
          row.names = FALSE)

# Observed growth rates (temperature rows × strain columns)
mu_mean %>%
  filter(evo_history %in% PRIMARY) %>%
  group_by(strain, temp) %>%
  summarise(mu = mean(mu, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = strain, values_from = mu) %>%
  rename(Temperature_C = temp) %>%
  write.csv(file.path(OUT, "prism_observed_growth_rates.csv"), row.names = FALSE)

# Fitted TPC curves (temperature rows × strain columns)
tpc_preds %>%
  filter(evo_history %in% PRIMARY,
         !strain %in% bad_strains) %>%
  pivot_wider(id_cols = temp, names_from = strain, values_from = pred) %>%
  write.csv(file.path(OUT, "prism_tpc_curves.csv"), row.names = FALSE)

# Thermal Performance Retention per strain
tpr %>%
  select(strain, evo_history, TPR) %>%
  write.csv(file.path(OUT, "prism_thermal_performance_retention.csv"),
            row.names = FALSE)

# Full SSH parameters per strain
write.csv(tpc_params,
          file.path(OUT, "tpc_params_ssh.csv"),
          row.names = FALSE)

cat(sprintf("\nExported to %s/\n", OUT))
for (f in list.files(OUT, full.names = FALSE)) {
  cat(sprintf("  %s\n", f))
}

