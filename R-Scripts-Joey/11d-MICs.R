# =============================================================================
# 11d-MICs.R
# Ancestor-normalised IC50 analysis
#
# Approach:
#   1. Read raw OD data (all_mic_data-new.csv)
#   2. Blank-correct per plate × concentration (same as 11c)
#   3. Fit 4-parameter logistic IC50 per population × rep × set × drug × month
#      (point estimates only — no bootstrap needed since we're computing ratios)
#   4. Normalise each evolved-strain plate IC50 to the ancestor IC50 on the
#      same plate: log_ratio = log(strain IC50 / ancestor IC50)
#   5. Average log_ratio across plates per strain per drug
#   6. Statistical tests: Welch t-test (40 vs 35 evolved) + one-sample t-test
#      vs 0 (log ratio = 0 ↔ strain equals ancestor); Holm-corrected
#   7. Dotplots + export
#
# Evolved-strain plate observations whose ancestor IC50 fit failed are dropped.
# All 36 strains retain ≥1 matched observation. Counts printed at runtime.
#
# Outputs:
#   data-processed/normalised-ic50-per-plate.csv   (per-plate IC50s + log ratio)
#   data-processed/normalised-ic50-per-strain.csv  (per-strain mean log ratio)
#   data-processed/stats-results-normalised-ic50.csv
#   figures/normalised-ic50-dotplot.png
#   figures/normalised-ic50-dotplot-sig.png
# =============================================================================

library(tidyverse)
library(minpack.lm)
library(ggsignif)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")

p_stars <- function(p) case_when(
  p < 0.001 ~ "***",
  p < 0.01  ~ "**",
  p < 0.05  ~ "*",
  TRUE      ~ "ns"
)

# =============================================================================
# 1. Load raw OD data
# =============================================================================

all_mic_data <- read_csv("data-processed/all_mic_data-new.csv", show_col_types = FALSE)

# =============================================================================
# 2. Blank correction — per plate (sheet_name) × concentration × month
# =============================================================================
# Blank rows carry media+drug background at each concentration step.
# sheet_name alone is not unique across months, so month is part of the join key.

blank_lookup <- all_mic_data |>
  filter(population == "Blank") |>
  select(sheet_name, month, concentration, blank_OD = OD)

mic_data <- all_mic_data |>
  filter(population != "Blank") |>
  left_join(blank_lookup, by = c("sheet_name", "month", "concentration"),
            relationship = "many-to-one") |>
  mutate(OD = pmax(OD - blank_OD, 0)) |>
  mutate(
    evolution_history = case_when(
      str_detect(population, "^35_") ~ "35 evolved",
      str_detect(population, "^40_") ~ "40 evolved",
      population == "fRS585"          ~ "fRS585"
    )
  )

stopifnot(all(!is.na(mic_data$blank_OD)))

cat("Blank-corrected rows:", nrow(mic_data), "\n")

# =============================================================================
# 3. Fit IC50 per plate (point estimate)
# =============================================================================
# 3-parameter logistic: OD = d / (1 + exp(b * (log(conc) - log(IC50))))
# Fit per population × rep × set × drug × month.
# Replace zero concentration with min_conc / 10 (same as 11c).

mic_data <- mic_data |>
  group_by(population, drug) |>
  mutate(
    min_conc      = min(concentration[concentration > 0], na.rm = TRUE),
    concentration = if_else(concentration == 0, min_conc / 10, concentration)
  ) |>
  ungroup() |>
  select(-min_conc)

# 4-parameter logistic: OD = bot + (top - bot) / (1 + exp(slope * (log(conc) - log(IC50))))
# bot: lower asymptote (residual growth at high drug);  top: upper asymptote;
# slope: Hill coefficient;  IC50: half-maximal inhibitory concentration.
# Three starting values for IC50 improve convergence on steep or atypical curves.
fit_ic50 <- function(df) {
  df <- filter(df, concentration > 0)
  if (nrow(df) < 5) {
    return(tibble(ic50 = NA_real_, bot = NA_real_, top = NA_real_,
                  slope = NA_real_, converged = FALSE))
  }
  e_starts <- c(median(df$concentration),
                quantile(df$concentration, 0.25),
                quantile(df$concentration, 0.75))
  for (e0 in e_starts) {
    start <- list(bot = 0, top = max(df$OD), slope = 10, e = e0)
    fit <- tryCatch(
      nlsLM(
        OD ~ bot + (top - bot) / (1 + exp(slope * (log(concentration) - log(e)))),
        data    = df,
        start   = start,
        lower   = c(bot = 0,   top = 0,   slope = 0.01, e = min(df$concentration)),
        upper   = c(bot = 0.5, top = 2,   slope = 200,  e = max(df$concentration) * 5),
        control = nls.lm.control(maxiter = 500)
      ),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      co <- coef(fit)
      return(tibble(ic50  = co[["e"]],
                    bot   = co[["bot"]],
                    top   = co[["top"]],
                    slope = co[["slope"]],
                    converged = TRUE))
    }
  }
  tibble(ic50 = NA_real_, bot = NA_real_, top = NA_real_,
         slope = NA_real_, converged = FALSE)
}

cat("Fitting IC50s per plate — this may take a minute...\n")

plate_ic50 <- mic_data |>
  group_by(population, evolution_history, rep, set, drug, month) |>
  group_modify(~ fit_ic50(.x)) |>
  ungroup()

n_fail <- sum(!plate_ic50$converged, na.rm = TRUE)
cat(sprintf("Fits: %d converged, %d failed\n",
            sum(plate_ic50$converged, na.rm = TRUE), n_fail))
if (n_fail > 0) {
  cat("Failed fits:\n")
  plate_ic50 |> filter(!converged) |>
    select(population, rep, set, drug, month) |>
    print()
}

# =============================================================================
# 3b. Diagnostic PDF — fitted 4PL curves + IC50 for all strains
# =============================================================================

make_4pl_curve <- function(ic50, bot, top, slope, min_c, max_c) {
  conc <- exp(seq(log(min_c), log(max_c), length.out = 300))
  od   <- bot + (top - bot) / (1 + exp(slope * (log(conc) - log(ic50))))
  tibble(concentration = conc, od_pred = od)
}

# Concentration range per plate (used to span the prediction curve)
conc_range <- mic_data |>
  filter(concentration > 0) |>
  group_by(population, drug, month, rep, set) |>
  summarise(min_c = min(concentration), max_c = max(concentration), .groups = "drop")

# Smooth predicted curves for all converged fits
pred_curves <- plate_ic50 |>
  filter(converged) |>
  left_join(conc_range, by = c("population", "drug", "month", "rep", "set")) |>
  mutate(plate_id = paste(month, set, rep)) |>
  rowwise() |>
  mutate(curve = list(make_4pl_curve(ic50, bot, top, slope, min_c, max_c))) |>
  ungroup() |>
  select(population, evolution_history, drug, plate_id, ic50, curve) |>
  unnest(curve)

raw_plot_data <- mic_data |>
  filter(concentration > 0) |>
  mutate(plate_id = paste(month, set, rep))

ic50_lines <- plate_ic50 |>
  filter(converged) |>
  mutate(
    plate_id   = paste(month, set, rep),
    ic50_label = sprintf("IC50 = %.3g", ic50)
  )

# Ancestor IC50, raw OD, and fitted curves — used as reference on evolved-strain pages
anc_ic50_ref <- plate_ic50 |>
  filter(evolution_history == "fRS585", converged) |>
  mutate(
    plate_id       = paste(month, set, rep),
    ic50_anc_label = sprintf("Anc = %.3g", ic50)
  ) |>
  select(drug, plate_id, ic50_anc = ic50, ic50_anc_label)

anc_raw_data <- raw_plot_data |> filter(population == "fRS585")
anc_curves   <- pred_curves   |> filter(population == "fRS585")

all_pops  <- sort(unique(mic_data$population[mic_data$population != "Blank"]))
all_drugs <- sort(unique(mic_data$drug))

cat("Writing diagnostic PDF...\n")
pdf("figures/ic50-curve-fits.pdf", width = 10, height = 6)

for (pop in all_pops) {
  for (drug_i in all_drugs) {
    raw_sub   <- raw_plot_data |> filter(population == pop, drug == drug_i)
    curve_sub <- pred_curves   |> filter(population == pop, drug == drug_i)
    ic50_sub  <- ic50_lines    |> filter(population == pop, drug == drug_i)

    if (nrow(raw_sub) == 0) next

    # Ancestor overlays — only for evolved strains, matched to the same plates
    if (pop != "fRS585") {
      matched_plates <- ic50_sub$plate_id
      anc_sub      <- anc_ic50_ref  |> filter(drug == drug_i, plate_id %in% matched_plates)
      anc_raw_sub  <- anc_raw_data  |> filter(drug == drug_i, plate_id %in% matched_plates)
      anc_curve_sub <- anc_curves   |> filter(drug == drug_i, plate_id %in% matched_plates)
    } else {
      anc_sub       <- tibble()
      anc_raw_sub   <- tibble()
      anc_curve_sub <- tibble()
    }

    # Ancestor layers drawn first so evolved-strain data renders on top
    p <- ggplot(raw_sub, aes(x = concentration, y = OD)) +
      scale_x_log10() +
      facet_wrap(~ plate_id) +
      labs(
        title    = paste0(pop, "  |  ", drug_i),
        subtitle = "Grey: ancestor (fRS585)  |  Black/blue: strain",
        x        = "Concentration (log scale)",
        y        = "OD (blank-corrected)"
      ) +
      theme_bw(base_size = 11)

    if (nrow(anc_raw_sub) > 0)
      p <- p + geom_point(data = anc_raw_sub, aes(y = OD),
                          color = "grey65", size = 1.2, alpha = 0.7)

    if (nrow(anc_curve_sub) > 0)
      p <- p + geom_line(data = anc_curve_sub, aes(y = od_pred),
                         color = "grey50", linewidth = 0.7)

    p <- p + geom_point(size = 1.5, alpha = 0.8, color = "steelblue")

    if (nrow(curve_sub) > 0)
      p <- p + geom_line(data = curve_sub, aes(y = od_pred),
                         color = "steelblue", linewidth = 0.8)

    if (nrow(anc_sub) > 0)
      p <- p +
        geom_vline(data = anc_sub, aes(xintercept = ic50_anc),
                   linetype = "dotted", color = "grey30", linewidth = 0.7) +
        geom_text(data = anc_sub,
                  aes(x = ic50_anc, y = Inf, label = ic50_anc_label),
                  vjust = 2.8, hjust = -0.1, size = 2.8, color = "grey30",
                  inherit.aes = FALSE)

    if (nrow(ic50_sub) > 0)
      p <- p +
        geom_vline(data = ic50_sub, aes(xintercept = ic50),
                   linetype = "dashed", color = "firebrick", linewidth = 0.6) +
        geom_text(data = ic50_sub,
                  aes(x = ic50, y = Inf, label = ic50_label),
                  vjust = 1.4, hjust = -0.1, size = 2.8, color = "firebrick",
                  inherit.aes = FALSE)

    print(p)
  }
}

dev.off()
cat("Saved: figures/ic50-curve-fits.pdf\n")

# =============================================================================
# 4. Normalise to per-plate ancestor
# =============================================================================

anc_ref <- plate_ic50 |>
  filter(evolution_history == "fRS585", converged) |>
  select(drug, month, rep, set, ic50_anc = ic50)

normalised_obs <- plate_ic50 |>
  filter(evolution_history != "fRS585", converged) |>
  left_join(anc_ref, by = c("drug", "month", "rep", "set")) |>
  filter(!is.na(ic50_anc)) |>
  mutate(log_ratio = log(ic50) - log(ic50_anc))

n_total   <- plate_ic50 |> filter(evolution_history != "fRS585", converged) |> nrow()
n_matched <- nrow(normalised_obs)
cat(sprintf("Normalised: %d of %d evolved-strain plate obs matched to ancestor (%d dropped)\n",
            n_matched, n_total, n_total - n_matched))

# =============================================================================
# 5. Per-strain mean log ratio
# =============================================================================

strain_normalised <- normalised_obs |>
  group_by(population, evolution_history, drug) |>
  summarise(
    log_ratio = mean(log_ratio, na.rm = TRUE),
    n_obs     = n(),
    .groups   = "drop"
  )

# Summary table
cat("\n=== Per-group summary (fold-change = exp(mean log ratio)) ===\n")
strain_normalised |>
  group_by(drug, evolution_history) |>
  summarise(
    n              = n(),
    mean_log_ratio = mean(log_ratio),
    fold_change    = exp(mean(log_ratio)),
    .groups        = "drop"
  ) |>
  print()

# =============================================================================
# 6. Statistical tests
# =============================================================================

run_tests_norm <- function(x, y = NULL, mu = NULL, label_x, label_y = "ancestor") {
  if (!is.null(y)) {
    t_res <- tryCatch(t.test(x, y),                           error = function(e) NULL)
    w_res <- tryCatch(wilcox.test(x, y, exact = FALSE),       error = function(e) NULL)
    ref   <- mean(y, na.rm = TRUE)
  } else {
    t_res <- tryCatch(t.test(x, mu = mu),                     error = function(e) NULL)
    w_res <- tryCatch(wilcox.test(x, mu = mu, exact = FALSE), error = function(e) NULL)
    ref   <- mu
  }
  tibble(
    comparison = paste(label_x, "vs", label_y),
    mean_x     = mean(x, na.rm = TRUE),
    ref        = ref,
    diff       = mean(x, na.rm = TRUE) - ref,
    ci_lo      = if (!is.null(t_res)) t_res$conf.int[1] else NA_real_,
    ci_hi      = if (!is.null(t_res)) t_res$conf.int[2] else NA_real_,
    p_welch    = if (!is.null(t_res)) t_res$p.value     else NA_real_,
    p_wilcox   = if (!is.null(w_res)) w_res$p.value     else NA_real_
  )
}

results_norm <- strain_normalised |>
  group_by(drug) |>
  group_modify(function(d, key) {
    g35 <- d |> filter(evolution_history == "35 evolved") |> pull(log_ratio)
    g40 <- d |> filter(evolution_history == "40 evolved") |> pull(log_ratio)
    bind_rows(
      run_tests_norm(g40, g35,    label_x = "40 evolved", label_y = "35 evolved"),
      run_tests_norm(g35, mu = 0, label_x = "35 evolved"),
      run_tests_norm(g40, mu = 0, label_x = "40 evolved")
    ) |>
      mutate(
        p_welch_holm  = p.adjust(p_welch,  method = "holm"),
        p_wilcox_holm = p.adjust(p_wilcox, method = "holm")
      )
  }) |>
  ungroup()

cat("\n=== Normalised IC50 — statistical results (Holm-corrected) ===\n")
print(results_norm, n = Inf)

# =============================================================================
# 7. Dot plots
# =============================================================================

plot_df_norm <- strain_normalised |>
  filter(evolution_history %in% c("35 evolved", "40 evolved")) |>
  mutate(evolution_history = factor(evolution_history,
                                    levels = c("35 evolved", "40 evolved")))

gmeans_norm <- plot_df_norm |>
  group_by(drug, evolution_history) |>
  summarise(m = mean(log_ratio), se = sd(log_ratio) / sqrt(n()), .groups = "drop")

y_range_norm <- plot_df_norm |>
  summarise(y_max = max(log_ratio), y_span = diff(range(log_ratio)), .by = drug)

# ── Without significance annotations ─────────────────────────────────────────

ggplot(plot_df_norm, aes(x = evolution_history, y = log_ratio, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(width = 0.12, size = 1.8, alpha = 0.6) +
  geom_pointrange(
    data = gmeans_norm,
    aes(y = m, ymin = m - se, ymax = m + se),
    size = 0.7, linewidth = 1.1
  ) +
  facet_wrap(~ drug, scales = "free_y") +
  scale_color_manual(values = EVO_COLORS) +
  labs(
    x       = NULL,
    y       = "log(IC50 / ancestor IC50)",
    caption = paste(
      "Points: individual strains (mean across plates)  |",
      "Large point \u00b1 bar: group mean \u00b1 SE  |",
      "Dashed line: ancestor level (log ratio = 0)"
    )
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        strip.text   = element_text(size = 13, face = "bold"),
        plot.caption = element_text(size = 8, color = "grey40"))

ggsave("figures/normalised-ic50-dotplot.png", width = 12, height = 5, dpi = 300)

# ── With significance annotations ────────────────────────────────────────────

bracket_df_norm <- results_norm |>
  filter(comparison == "40 evolved vs 35 evolved") |>
  mutate(
    xmin        = "35 evolved",
    xmax        = "40 evolved",
    annotations = p_stars(p_welch_holm)
  ) |>
  left_join(y_range_norm, by = "drug") |>
  mutate(y_position = y_max + y_span * 0.06)

anc_star_df_norm <- results_norm |>
  filter(str_detect(comparison, "vs ancestor")) |>
  mutate(
    evolution_history = factor(str_remove(comparison, " vs ancestor"),
                               levels = c("35 evolved", "40 evolved")),
    label = p_stars(p_welch_holm)
  ) |>
  left_join(gmeans_norm, by = c("drug", "evolution_history")) |>
  left_join(y_range_norm, by = "drug") |>
  mutate(y_pos = m + se + y_span * 0.04)

ggplot(plot_df_norm, aes(x = evolution_history, y = log_ratio, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(width = 0.12, size = 1.8, alpha = 0.6) +
  geom_pointrange(
    data = gmeans_norm,
    aes(y = m, ymin = m - se, ymax = m + se),
    size = 0.7, linewidth = 1.1
  ) +
  suppressWarnings(geom_signif(
    data       = bracket_df_norm,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual     = TRUE, tip_length = 0.02, textsize = 4.5, color = "black"
  )) +
  geom_text(
    data    = anc_star_df_norm,
    aes(x = evolution_history, y = y_pos, label = label),
    color   = "black", size = 4, fontface = "bold", nudge_x = 0.3
  ) +
  facet_wrap(~ drug, scales = "free_y") +
  scale_color_manual(values = EVO_COLORS) +
  labs(
    x       = NULL,
    y       = "log(IC50 / ancestor IC50)",
    caption = paste(
      "Points: individual strains (mean across plates)  |",
      "Large point \u00b1 bar: group mean \u00b1 SE  |",
      "Dashed line: ancestor level (log ratio = 0)\n",
      "Brackets: Welch t-test, 40 vs 35 evolved (Holm-corrected)  |",
      "Stars to right of mean: one-sample t-test vs 0"
    )
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        strip.text   = element_text(size = 13, face = "bold"),
        plot.caption = element_text(size = 8, color = "grey40"))

ggsave("figures/normalised-ic50-dotplot-sig.png", width = 12, height = 5, dpi = 300)

# =============================================================================
# 8. Month-adjusted LM (all drugs)
# =============================================================================
# Fits log(IC50) ~ evolution_history + month per drug using per-strain
# per-month means. emmeans gives marginal means adjusted for month batch
# effect. Assumes additive (no interaction) month effect on log scale.
# The 40-evolved group has measurements in both months for all drugs,
# making the month effect estimable even though 35-evolved is March-only.

library(emmeans)

# Per-strain mean IC50 within each drug × month (includes fRS585)
strain_by_month <- plate_ic50 |>
  filter(converged,
         evolution_history %in% c("35 evolved", "40 evolved", "fRS585")) |>
  mutate(month = factor(month, levels = c("January", "February", "March"))) |>
  group_by(population, evolution_history, drug, month) |>
  summarise(mean_ic50 = mean(ic50), .groups = "drop")

lm_results <- strain_by_month |>
  mutate(
    log_ic50 = log(mean_ic50),
    evolution_history = factor(evolution_history,
                               levels = c("40 evolved", "35 evolved", "fRS585"))
  ) |>
  group_by(drug) |>
  group_modify(function(d, key) {
    fit  <- lm(log_ic50 ~ evolution_history + month, data = d)
    emm  <- emmeans(fit, ~ evolution_history)
    contrast(emm, method = "pairwise", adjust = "holm") |>
      as_tibble() |>
      rename(diff = estimate, se = SE, p_holm = p.value) |>
      mutate(
        fold_change = exp(diff),
        comparison  = str_replace(contrast, " - ", " vs ")
      ) |>
      select(comparison, diff, fold_change, se, df, p_holm)
  }) |>
  ungroup()

cat("\n=== Month-adjusted LM results (Holm-corrected) ===\n")
lm_results |>
  mutate(
    fold_change = round(fold_change, 2),
    diff        = round(diff, 3),
    p_holm      = ifelse(p_holm < 0.001, "<0.001", sprintf("%.3f", p_holm))
  ) |>
  as.data.frame() |>
  print()

# =============================================================================
# 9. Export
# =============================================================================

write_csv(normalised_obs,   "data-processed/normalised-ic50-per-plate.csv")
write_csv(strain_normalised, "data-processed/normalised-ic50-per-strain.csv")
write_csv(results_norm,      "data-processed/stats-results-normalised-ic50.csv")
write_csv(lm_results,        "data-processed/stats-results-month-adjusted-lm-ic50.csv")
