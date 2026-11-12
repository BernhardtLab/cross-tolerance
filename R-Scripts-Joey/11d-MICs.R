# =============================================================================
# 11d-MICs.R
# Ancestor-normalised IC50 analysis
## Author: Joey Bernhardt
## Last updated: November 10 2026

#
# Approach:
#   1. Read raw OD data (all_mic_data-new.csv)
#   2. Blank-correct per plate × concentration
#   3. Fit 4-parameter logistic IC50 per population × rep × set × drug × month
#      (point estimates only — no bootstrap needed since we're computing ratios)
#   4. Normalise each evolved-strain plate IC50 to the ancestor IC50 on the
#      same plate: log2_ratio = log2(strain IC50 / ancestor IC50)
#   5. Average log2_ratio across plates per strain per drug
#   6. Statistical tests: Welch t-test (40 vs 35 evolved) + one-sample t-test
#      vs 0 (log ratio = 0 ↔ strain equals ancestor); p-values uncorrected
#   7. Dotplots + export
#
# Five converged-but-poorly-fitting individual dose-response curves (one strain
# × drug on one plate each) are manually excluded (sec. 3a); the rest of each
# plate, including the ancestor curve, is retained. We determined this by visually inspecting the curves.
# Evolved-strain plate observations whose ancestor IC50 fit failed are dropped.
# All 36 strains retain ≥1 matched observation.
#
# Outputs:
#   data-processed/normalised-ic50-per-plate.csv   (per-plate IC50s + log ratio)
#   data-processed/normalised-ic50-per-strain.csv  (per-strain mean log ratio)
#   data-processed/stats-results-normalised-ic50.csv
#   data-processed/supp-table-ic50-pvalues.csv     (uncorrected vs Holm p-values)
#   data-processed/supp-table-ancestor-fit-flags.csv (ancestor 4PL fit diagnostics + poor-fit flags)
#   figures/normalised-ic50-dotplot.png
#   figures/normalised-ic50-dotplot-sig.png
#   figures/supp-ic50-curves-ancestor-{drug}.pdf  (ancestor-only fitted curves, one per drug; poorly constrained fits shaded)
# =============================================================================

library(tidyverse)
library(minpack.lm)
library(ggsignif)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")

theme_evo <- function(base_size = 18) {
  theme_classic(base_size = base_size) +
    theme(
      panel.background = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA),
      plot.background  = element_rect(fill = "transparent", colour = NA)
    )
}

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


# =============================================================================
# 3. Fit IC50 per plate (point estimate)
# =============================================================================
# Fit per population × rep × set × drug × month.
# Replace zero concentration with min_conc / 10.

mic_data2 <- mic_data |>
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

plate_ic50 <- mic_data2 |>
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
# 3a. Manual exclusion of poorly fitting dose-response curves
# =============================================================================
# These fits converged, but visual inspection of the fitted curves plotted on top of the OD data showed that the 4PL curve does not describe the data well (see analysis_notes).
# Each row of bad_fits identifies a single dose-response curve (one strain ×
# drug on one plate); other strains and the ancestor on the same plate are kept.
# The curves are kept in plate_ic50 so they still appear in the diagnostic and
# supplement figures, but are removed before normalisation further analysis.

bad_fits <- tribble(
  ~population, ~drug,          ~month,     ~set,     ~rep,
  "40_E9",     "fluconazole",  "February", "Plate2", "Rep1",  # IC50 = 2.49
  "40_G9",     "fluconazole",  "March",    "Set4",   "Rep3",  # IC50 = 127
  "40_C9",     "caspofungin",  "January",  "Set1",   "Rep3",  # IC50 = 0.00154
  "40_D8",     "amphotericin", "January",  "Set2",   "Rep3",  # IC50 = 0.0622
  "40_E11",    "amphotericin", "March",    "Set4",   "Rep3"   # IC50 = 0.0214
)

# Guard against typos: every listed curve must match exactly one row
stopifnot(nrow(semi_join(plate_ic50, bad_fits,
                         by = c("population", "drug", "month", "set", "rep"))) ==
            nrow(bad_fits))

plate_ic50_clean <- plate_ic50 |>
  anti_join(bad_fits, by = c("population", "drug", "month", "set", "rep"))


# =============================================================================
# 3b. Diagnostic PDF — fitted 4PL curves + IC50 for all strains
# =============================================================================

make_4pl_curve <- function(ic50, bot, top, slope, min_c, max_c) {
  conc <- exp(seq(log(min_c), log(max_c), length.out = 300))
  od   <- bot + (top - bot) / (1 + exp(slope * (log(conc) - log(ic50))))
  tibble(concentration = conc, od_pred = od)
}

# Concentration range per plate (used to span the prediction curve)
conc_range <- mic_data2 |>
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
      theme_evo(base_size = 11)

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

normalised_obs <- plate_ic50_clean |>
  filter(evolution_history != "fRS585", converged) |>
  left_join(anc_ref, by = c("drug", "month", "rep", "set")) |>
  filter(!is.na(ic50_anc)) |>
  mutate(log_ratio = log2(ic50) - log2(ic50_anc))

n_total   <- plate_ic50_clean |> filter(evolution_history != "fRS585", converged) |> nrow()
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
cat("\n=== Per-group summary (fold-change = 2^(mean log2 ratio)) ===\n")
strain_normalised |>
  group_by(drug, evolution_history) |>
  summarise(
    n              = n(),
    mean_log2_ratio = mean(log_ratio),
    fold_change    = 2^mean(log_ratio),
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
    )
  }) |>
  ungroup()

cat("\n=== Normalised IC50 — statistical results (uncorrected p-values) ===\n")
print(results_norm, n = Inf)

# =============================================================================
# 6b. Supplementary table — uncorrected vs Holm-corrected p-values
# =============================================================================
# Main text reports uncorrected p-values. For transparency, this table shows
# Welch t-test p-values alongside two Holm corrections:
#   - within drug (3 tests per drug)
#   - across all 9 tests (3 drugs × 3 comparisons)
# Fold changes and 95% CIs are back-transformed from the log2 scale.

group_n <- strain_normalised |>
  count(drug, evolution_history) |>
  pivot_wider(names_from = evolution_history, values_from = n)

fmt_p <- function(p) if_else(p < 0.001, "<0.001", sprintf("%.3f", p))

supp_table_p <- results_norm |>
  group_by(drug) |>
  mutate(p_holm_within_drug = p.adjust(p_welch, method = "holm")) |>
  ungroup() |>
  mutate(p_holm_all_tests = p.adjust(p_welch, method = "holm")) |>
  left_join(group_n, by = "drug") |>
  mutate(n = case_when(
    comparison == "40 evolved vs 35 evolved" ~
      paste(`40 evolved`, `35 evolved`, sep = " / "),
    comparison == "35 evolved vs ancestor" ~ as.character(`35 evolved`),
    comparison == "40 evolved vs ancestor" ~ as.character(`40 evolved`)
  )) |>
  transmute(
    Drug                       = str_to_sentence(drug),
    Comparison                 = str_replace(comparison, "vs ancestor", "vs ancestor (fRS585)"),
    `n (strains)`              = n,
    `Fold change`              = sprintf("%.2f", 2^diff),
    `95% CI`                   = sprintf("[%.2f, %.2f]", 2^ci_lo, 2^ci_hi),
    `p (uncorrected)`          = fmt_p(p_welch),
    `p (Holm, within drug)`    = fmt_p(p_holm_within_drug),
    `p (Holm, all 9 tests)`    = fmt_p(p_holm_all_tests)
  )

cat("\n=== Supplementary table: uncorrected vs Holm-corrected p-values ===\n")
print(as.data.frame(supp_table_p))

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
  facet_wrap(~ drug, scales = "free_y", strip.position = "bottom") +
  scale_color_manual(values = EVO_COLORS) +
  labs(
    x       = NULL,
    y       = expression(log[2](IC50 / "ancestor IC50")),
    caption = paste(
      "Points: individual strains (mean across plates)  |",
      "Large point \u00b1 bar: group mean \u00b1 SE  |",
      "Dashed line: ancestor level (log2 ratio = 0)"
    )
  ) +
  theme_evo(base_size = 22) +
  theme(legend.position = "none",
        strip.placement = "outside",
        strip.text   = element_text(size = 16, face = "bold"),
        plot.caption = element_text(size = 11, color = "grey40"))

ggsave("figures/normalised-ic50-dotplot.png", width = 8, height = 4, dpi = 300, bg = "transparent")

# ── With significance annotations ────────────────────────────────────────────

bracket_df_norm <- results_norm |>
  filter(comparison == "40 evolved vs 35 evolved") |>
  mutate(
    xmin        = "35 evolved",
    xmax        = "40 evolved",
    annotations = p_stars(p_welch)
  ) |>
  left_join(y_range_norm, by = "drug") |>
  mutate(y_position = y_max + y_span * 0.18)

anc_star_df_norm <- results_norm |>
  filter(str_detect(comparison, "vs ancestor")) |>
  mutate(
    evolution_history = factor(str_remove(comparison, " vs ancestor"),
                               levels = c("35 evolved", "40 evolved")),
    label = p_stars(p_welch)
  ) |>
  left_join(gmeans_norm, by = c("drug", "evolution_history")) |>
  left_join(y_range_norm, by = "drug") |>
  mutate(y_pos = m + se + y_span * 0.10)

ggplot(plot_df_norm, aes(x = evolution_history, y = log_ratio, color = evolution_history)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#000000", linewidth = 1.0) +
  geom_jitter(width = 0.12, size = 3.0, alpha = 0.6) +
  geom_pointrange(
    data = gmeans_norm,
    aes(y = m, ymin = m - se, ymax = m + se),
    size = 1.0, linewidth = 1.6
  ) +
  suppressWarnings(geom_signif(
    data       = bracket_df_norm,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual     = TRUE, tip_length = 0.02, textsize = 6.5, color = "black"
  )) +
  geom_text(
    data    = anc_star_df_norm,
    aes(x = evolution_history, y = y_pos, label = label),
    color   = "black", size = 6, fontface = "bold", nudge_x = 0.3
  ) +
  facet_wrap(~ drug, scales = "free_y", strip.position = "bottom") +
  scale_color_manual(values = EVO_COLORS) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
  labs(
    x       = NULL,
    y       = expression(log[2](IC50 / "ancestor IC50")),
    # caption = paste(
    #   "Points: individual strains (mean across plates)  |",
    #   "Large point \u00b1 bar: group mean \u00b1 SE  |",
    #   "Dashed line: ancestor level (log2 ratio = 0)\n",
    #   "Brackets: Welch t-test, 40 vs 35 evolved (uncorrected)  |",
    #   "Stars to right of mean: one-sample t-test vs 0")
  ) +
  theme_evo(base_size = 22) +
  theme(legend.position = "none",
        strip.placement  = "outside",
        strip.background = element_blank(),
        strip.text   = element_text(size = 18),
        plot.caption = element_text(size = 13, color = "grey40"))

ggsave("figures/normalised-ic50-dotplot-sig.png", width = 8, height = 4, dpi = 300, bg = "transparent")


# =============================================================================
# 9. Export
# =============================================================================

write_csv(normalised_obs,   "data-processed/normalised-ic50-per-plate.csv")
write_csv(strain_normalised, "data-processed/normalised-ic50-per-strain.csv")
write_csv(results_norm,      "data-processed/stats-results-normalised-ic50.csv")
write_csv(supp_table_p,      "data-processed/supp-table-ic50-pvalues.csv")

# =============================================================================
# 9b. Fit-quality flags for all converged 4PL fits (diagnostic only)
# =============================================================================
# A fit is flagged as poorly constrained if any of:
#   - shallow:      slope < 1 (IC50 poorly located along the concentration axis)
#   - top_extrap:   fitted top > 1.1 × max observed OD (curve extrapolates
#                   beyond the data rather than following the upper plateau)
#   - top_support:  ≤ 2 tested concentrations below the IC50 (upper plateau
#                   barely sampled)
# Wald standard errors are deliberately not used: for steep curves (slope > ~20)
# the OD drop falls between two adjacent concentrations, the IC50 is only
# bracketed by that interval, and the SE blows up even though the fit is fine.
# Flagged fits are NOT removed from the analysis; they are only marked in the
# supplement figures (sections 10 and 7c).

fit_flags <- mic_data2 |>
  filter(population != "Blank", concentration > 0) |>
  group_by(population, drug, month, set, rep) |>
  summarise(max_od = max(OD), concs = list(sort(unique(concentration))),
            .groups = "drop") |>
  inner_join(plate_ic50 |> filter(converged),
             by = c("population", "drug", "month", "set", "rep")) |>
  mutate(
    plate_id     = paste(month, set, rep),
    n_conc_below = map2_int(concs, ic50, \(cc, e) sum(cc < e)),
    top_ratio    = top / max_od,
    shallow      = slope < 1,
    top_extrap   = top_ratio > 1.1,
    top_support  = n_conc_below <= 2,
    poor_fit     = shallow | top_extrap | top_support,
    flag_reason  = pmap_chr(
      list(shallow, top_extrap, top_support),
      \(a, b, c) paste(c("shallow slope", "top extrapolated",
                         "few points above IC50")[c(a, b, c)], collapse = "; ")
    )
  ) |>
  select(population, drug, plate_id, ic50, top, max_od, top_ratio, slope,
         n_conc_below, poor_fit, flag_reason)

anc_fit_flags <- fit_flags |>
  filter(population == "fRS585") |>
  select(-population)

# =============================================================================
# 9c. Sensitivity analysis — exclude observations with poorly constrained fits
# =============================================================================
# Drops every evolved-strain plate observation where the strain's fit OR the
# same-plate ancestor's fit is flagged in 9b, then reruns sections 5–6.

flagged_obs <- fit_flags |>
  filter(poor_fit) |>
  select(population, drug, plate_id)

normalised_obs_sens <- normalised_obs |>
  mutate(plate_id = paste(month, set, rep)) |>
  # strain fit flagged
  anti_join(flagged_obs, by = c("population", "drug", "plate_id")) |>
  # ancestor fit on the same plate flagged
  anti_join(flagged_obs |> filter(population == "fRS585") |> select(-population),
            by = c("drug", "plate_id"))

cat(sprintf("Sensitivity: dropped %d of %d plate observations with a flagged strain or ancestor fit\n",
            nrow(normalised_obs) - nrow(normalised_obs_sens), nrow(normalised_obs)))

strain_normalised_sens <- normalised_obs_sens |>
  group_by(population, evolution_history, drug) |>
  summarise(log_ratio = mean(log_ratio), n_obs = n(), .groups = "drop")

results_sens <- strain_normalised_sens |>
  group_by(drug) |>
  group_modify(function(d, key) {
    g35 <- d |> filter(evolution_history == "35 evolved") |> pull(log_ratio)
    g40 <- d |> filter(evolution_history == "40 evolved") |> pull(log_ratio)
    bind_rows(
      run_tests_norm(g40, g35,    label_x = "40 evolved", label_y = "35 evolved"),
      run_tests_norm(g35, mu = 0, label_x = "35 evolved"),
      run_tests_norm(g40, mu = 0, label_x = "40 evolved")
    )
  }) |>
  ungroup()

sens_compare <- results_norm |>
  select(drug, comparison, diff, ci_lo, ci_hi, p_welch) |>
  left_join(results_sens |> select(drug, comparison, diff, ci_lo, ci_hi, p_welch),
            by = c("drug", "comparison"), suffix = c("_main", "_sens")) |>
  left_join(strain_normalised |> count(drug, evolution_history, name = "n_strains_main") |>
              pivot_wider(names_from = evolution_history, values_from = n_strains_main,
                          names_prefix = "n_main_"), by = "drug") |>
  left_join(strain_normalised_sens |> count(drug, evolution_history, name = "n_strains_sens") |>
              pivot_wider(names_from = evolution_history, values_from = n_strains_sens,
                          names_prefix = "n_sens_"), by = "drug") |>
  transmute(
    drug, comparison,
    fold_change_main = 2^diff_main, ci_lo_main = 2^ci_lo_main, ci_hi_main = 2^ci_hi_main,
    p_welch_main,
    fold_change_sens = 2^diff_sens, ci_lo_sens = 2^ci_lo_sens, ci_hi_sens = 2^ci_hi_sens,
    p_welch_sens,
    n_obs_main = nrow(normalised_obs), n_obs_sens = nrow(normalised_obs_sens),
    across(starts_with("n_main_")), across(starts_with("n_sens_"))
  )

cat("\n=== Sensitivity: main vs excluding poorly constrained fits ===\n")
print(as.data.frame(sens_compare |> select(drug, comparison, fold_change_main, p_welch_main,
                                           fold_change_sens, p_welch_sens)))

write_csv(sens_compare, "data-processed/sensitivity-ic50-excl-poor-fits.csv")

# =============================================================================
# 10. Supplement figures — IC50 curve fits (evolved strains + same-plate ancestor)
#     One PDF per drug; one facet per evolved strain × plate. The ancestor
#     (fRS585) from the same plate is overlaid in each facet as a reference.
#     Curves excluded in section 3a are labelled and drawn dashed.
#     Note these are for Figure S5 in the SI.
# =============================================================================

evo_raw    <- raw_plot_data |> filter(population != "fRS585")
evo_curves <- pred_curves   |> filter(population != "fRS585")
evo_ic50   <- ic50_lines    |> filter(population != "fRS585")

anc_raw   <- raw_plot_data |> filter(population == "fRS585")
anc_curve <- pred_curves   |> filter(population == "fRS585")
anc_ic50  <- ic50_lines    |> filter(population == "fRS585") |>
  mutate(ic50_label = sprintf("Anc = %.3g", ic50))

# Curves manually excluded in section 3a are flagged in the facet title and
# their fitted curve is drawn dashed
excluded_ids <- with(bad_fits, paste(population, drug, paste(month, set, rep)))
is_excluded  <- function(population, drug, plate_id) {
  paste(population, drug, plate_id) %in% excluded_ids
}

# Poor-fit flags (section 9b): a panel is flagged if the evolved strain's fit,
# the same-plate ancestor's fit, or both are poorly constrained
panel_flags <- evo_raw |>
  distinct(population, drug, plate_id) |>
  left_join(fit_flags |> select(population, drug, plate_id,
                                strain_poor = poor_fit, strain_reason = flag_reason),
            by = c("population", "drug", "plate_id")) |>
  left_join(anc_fit_flags |> select(drug, plate_id,
                                    anc_poor = poor_fit, anc_reason = flag_reason),
            by = c("drug", "plate_id")) |>
  mutate(
    strain_poor = coalesce(strain_poor, FALSE),
    anc_poor    = coalesce(anc_poor, FALSE),
    poor_fit    = strain_poor | anc_poor,
    flag_text   = case_when(
      strain_poor & anc_poor ~ paste0("POOR FIT: strain (", strain_reason,
                                      "), ancestor (", anc_reason, ")"),
      strain_poor            ~ paste0("POOR FIT: strain (", strain_reason, ")"),
      anc_poor               ~ paste0("POOR FIT: ancestor (", anc_reason, ")"),
      .default               = ""
    )
  )
panel_flag_key <- with(panel_flags, paste(population, drug, plate_id))

# Second strip line is always present (blank if unflagged) so strips align
make_label <- function(population, plate_id, drug) {
  lab <- paste0(population, " | ", plate_id)
  lab <- if_else(is_excluded(population, drug, plate_id),
                 paste0(lab, " (excluded from analysis)"), lab)
  flag <- panel_flags$flag_text[match(paste(population, drug, plate_id), panel_flag_key)]
  paste0(lab, "\n", coalesce(flag, ""))
}

# Copy the ancestor data for a plate into every evolved-strain facet on that plate
expand_to_facets <- function(anc_df, facets) {
  anc_df |>
    select(-population) |>
    inner_join(facets, by = c("drug", "plate_id"), relationship = "many-to-many") |>
    mutate(facet_label = make_label(population, plate_id, drug))
}

NCOL <- 8

for (drug_i in all_drugs) {
  raw_sub   <- evo_raw    |> filter(drug == drug_i)
  curve_sub <- evo_curves |> filter(drug == drug_i)
  ic50_sub  <- evo_ic50   |> filter(drug == drug_i)

  raw_sub   <- raw_sub   |> mutate(facet_label = make_label(population, plate_id, drug))
  curve_sub <- curve_sub |> mutate(facet_label = make_label(population, plate_id, drug),
                                   excluded    = is_excluded(population, drug, plate_id))
  ic50_sub  <- ic50_sub  |> mutate(facet_label = make_label(population, plate_id, drug))

  facets <- raw_sub |> distinct(drug, plate_id, population)

  anc_raw_sub   <- anc_raw   |> filter(drug == drug_i) |> expand_to_facets(facets)
  anc_curve_sub <- anc_curve |> filter(drug == drug_i) |> expand_to_facets(facets)
  anc_ic50_sub  <- anc_ic50  |> filter(drug == drug_i) |> expand_to_facets(facets)

  flag_panels <- panel_flags |>
    filter(drug == drug_i, poor_fit) |>
    mutate(facet_label = make_label(population, plate_id, drug))

  n_facets <- n_distinct(raw_sub$facet_label)
  n_rows   <- ceiling(n_facets / NCOL)

  caption_lines <- c(
    if (any(curve_sub$excluded))
      paste("Dashed strain curve, \"(excluded from analysis)\": poor 4PL fit,",
            "removed before normalisation and statistical tests."),
    if (nrow(flag_panels) > 0)
      paste("Shaded panels, \"POOR FIT\": strain and/or ancestor fit poorly constrained",
            "(slope < 1, top > 1.1 × max OD, or ≤ 2 concentrations below IC50);",
            "diagnostic only, not excluded.")
  )

  # Ancestor layers first so the evolved strain renders on top
  p <- ggplot(raw_sub, aes(x = concentration, y = OD, color = evolution_history)) +
    # Tint flagged panels; xmin = 0 maps to -Inf on the log scale. An empty
    # geom_rect layer breaks facet rendering, so only add it when needed.
    (if (nrow(flag_panels) > 0)
      geom_rect(data = flag_panels,
                aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf),
                fill = "firebrick", alpha = 0.08, inherit.aes = FALSE)) +
    geom_point(data = anc_raw_sub, aes(color = "fRS585"), size = 1.6, alpha = 0.4) +
    geom_line(data = anc_curve_sub, aes(y = od_pred, color = "fRS585"),
              linewidth = 0.8, alpha = 0.6) +
    geom_vline(data = anc_ic50_sub, aes(xintercept = ic50),
               linetype = "dotted", color = "grey30", linewidth = 0.7) +
    geom_text(data = anc_ic50_sub,
              aes(x = ic50, y = Inf, label = ic50_label),
              vjust = 2.9, hjust = -0.1, size = 3.5, color = "grey30",
              inherit.aes = FALSE) +
    geom_point(size = 2.0, alpha = 0.7) +
    geom_line(data = curve_sub,
              aes(y = od_pred, color = evolution_history, linetype = excluded),
              linewidth = 1.0) +
    geom_vline(data = ic50_sub, aes(xintercept = ic50),
               linetype = "dashed", color = "firebrick", linewidth = 0.7) +
    geom_text(data = ic50_sub,
              aes(x = ic50, y = Inf, label = ic50_label),
              vjust = 1.4, hjust = -0.1, size = 3.5, color = "firebrick",
              inherit.aes = FALSE) +
    scale_x_log10() +
    scale_color_manual(
      values = EVO_COLORS, name = NULL,
      breaks = c("35 evolved", "40 evolved", "fRS585"),
      labels = c("35 evolved", "40 evolved", "Ancestor (fRS585)")
    ) +
    scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "dashed"),
                          guide = "none") +
    facet_wrap(~ facet_label, ncol = NCOL) +
    labs(
      title   = drug_i,
      x       = paste(str_to_sentence(drug_i), "concentration (log scale)"),
      y       = "OD (blank-corrected)",
      caption = if (length(caption_lines) > 0) paste(caption_lines, collapse = "\n")
    ) +
    theme_evo(base_size = 20) +
    theme(
      strip.text     = element_text(size = 9, margin = margin(2, 0, 2, 0)),
      strip.background = element_blank(),
      legend.position = "bottom",
      legend.text     = element_text(size = 20),
      plot.caption    = element_text(size = 14, color = "grey40")
    )

  out_file <- sprintf("figures/supp-ic50-curves-%s.pdf",
                      gsub("[^a-zA-Z0-9]", "-", tolower(drug_i)))
  ggsave(out_file, p,
         width  = NCOL * 4.1,
         height = n_rows * 2.5 + 1.2,
         bg     = "white")
  cat("Saved:", out_file, "\n")
}

# =============================================================================
# 7c. Supplement — ancestor-only fitted curves (one facet per plate)
# =============================================================================
# Visual check of ancestor fit quality, since every evolved-strain IC50 is
# normalised to the ancestor on the same plate. Flags are computed in
# section 9b.

cat("Ancestor fits flagged as poorly constrained:\n")
anc_fit_flags |>
  filter(poor_fit) |>
  select(drug, plate_id, ic50, slope, top_ratio, n_conc_below, flag_reason) |>
  print()

write_csv(anc_fit_flags |> arrange(drug, plate_id),
          "data-processed/supp-table-ancestor-fit-flags.csv")

anc_facet_labels <- anc_fit_flags |>
  mutate(facet_label = if_else(poor_fit,
                               paste0(plate_id, "\nPOOR FIT: ", flag_reason),
                               paste0(plate_id, "\n"))) |>
  select(drug, plate_id, poor_fit, facet_label)

NCOL_ANC <- 7

for (drug_i in all_drugs) {
  labs_i <- anc_facet_labels |> filter(drug == drug_i) |> arrange(plate_id)
  add_label <- function(df) {
    df |>
      filter(drug == drug_i) |>
      inner_join(labs_i, by = c("drug", "plate_id")) |>
      mutate(facet_label = factor(facet_label, levels = labs_i$facet_label))
  }
  anc_raw_sub   <- add_label(anc_raw)
  anc_curve_sub <- add_label(anc_curve)
  anc_ic50_sub  <- add_label(anc_ic50)
  flag_panels   <- labs_i |> filter(poor_fit) |>
    mutate(facet_label = factor(facet_label, levels = labs_i$facet_label))

  n_rows <- ceiling(nrow(labs_i) / NCOL_ANC)

  p <- ggplot(anc_raw_sub, aes(x = concentration, y = OD)) +
    # Tint flagged panels; xmin = 0 maps to -Inf on the log scale. An empty
    # geom_rect layer breaks facet rendering, so only add it when needed.
    (if (nrow(flag_panels) > 0)
      geom_rect(data = flag_panels,
                aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf),
                fill = "firebrick", alpha = 0.08, inherit.aes = FALSE)) +
    geom_point(size = 2.0, alpha = 0.7, color = EVO_COLORS[["fRS585"]]) +
    geom_line(data = anc_curve_sub, aes(y = od_pred, linetype = poor_fit),
              linewidth = 1.0, color = EVO_COLORS[["fRS585"]]) +
    geom_vline(data = anc_ic50_sub, aes(xintercept = ic50),
               linetype = "dashed", color = "firebrick", linewidth = 0.7) +
    geom_text(data = anc_ic50_sub,
              aes(x = ic50, y = Inf, label = ic50_label),
              vjust = 1.4, hjust = -0.1, size = 3.5, color = "firebrick",
              inherit.aes = FALSE) +
    scale_x_log10() +
    scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "dashed"),
                          guide = "none") +
    facet_wrap(~ facet_label, ncol = NCOL_ANC) +
    labs(
      title   = paste(drug_i, "— ancestor (fRS585)"),
      x       = paste(str_to_sentence(drug_i), "concentration (log scale)"),
      y       = "OD (blank-corrected)",
      caption = if (nrow(flag_panels) > 0)
        paste("Shaded panels / dashed curves: poorly constrained fit",
              "(slope < 1, top > 1.1 × max OD, or ≤ 2 concentrations below IC50).",
              "Diagnostic only; not excluded from analysis.")
    ) +
    theme_evo(base_size = 20) +
    theme(
      strip.text       = element_text(size = 11, margin = margin(2, 0, 2, 0)),
      strip.background = element_blank(),
      plot.caption     = element_text(size = 14, color = "grey40")
    )

  out_file <- sprintf("figures/supp-ic50-curves-ancestor-%s.pdf",
                      gsub("[^a-zA-Z0-9]", "-", tolower(drug_i)))
  ggsave(out_file, p,
         width  = NCOL_ANC * 4.1,
         height = n_rows * 3.0 + 1.2,
         bg     = "white")
  cat("Saved:", out_file, "\n")
}
