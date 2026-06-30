
# =============================================================================
# MIC Analysis
# =============================================================================

library(tidyverse)
library(readxl)
library(drc)
library(pracma)
library(cowplot)
library(minpack.lm)
library(car)
library(conflicted)

# Prefer dplyr's select function
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
theme_set(theme_cowplot())


# =============================================================================
# 1. Data import
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

write_csv(all_mic_data, "data-processed/all_mic_data-new.csv")


# =============================================================================
# 1b. Diagnostic: raw blank OD by month, before correction
# =============================================================================

a1 <- all_mic_data |>
  filter(drug == "amphotericin") |>
  filter(population == "Blank")
a1 |>
  ggplot(aes(x = concentration, y = OD, color = month)) + geom_point() +
  ylab("OD of blank")


# =============================================================================
# 2. Blank correction — per plate (sheet), per concentration, per month
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
# 3. IC50 fitting
# =============================================================================

# Prepare data: add evolution_history, replace zero concentration with
# min_conc / 10 per strain × drug (more sensible than a hardcoded tiny value).
# Built on top of the blank-corrected OD from section 2.
mic_data <- mic_data |>
  mutate(
    evolution_history = case_when(
      str_detect(population, "^35_") ~ "35 evolved",
      str_detect(population, "^40_") ~ "40 evolved",
      population == "fRS585"          ~ "fRS585"
    )
  ) |>
  group_by(population, drug) |>
  mutate(
    min_conc    = min(concentration[concentration > 0], na.rm = TRUE),
    concentration = if_else(concentration == 0, min_conc / 10, concentration)
  ) |>
  ungroup() |>
  select(-min_conc)


a2 <- mic_data %>%
  # separate(sheet_name, into = c("set", "rep", "drug_type"), sep = "_", remove = FALSE) %>% 
  unite("pop_rep", population, rep, set, drug, month, sep = "_", remove = FALSE)

# 3-parameter logistic: OD = d / (1 + exp(b * (log(conc) - log(IC50))))
# Fit per strain × drug, pooling all replicates and sets


# boostrapping function ---------------------------------------------------



fit_bootstrap_ic50 <- function(data, group_var = "pop_rep", R = 5000, seed = 123) {
  set.seed(seed)
  
  warning_log <- list()  # NEW: capture warnings
  pops <- unique(data[[group_var]])
  ic50_list <- list()
  pred_list <- list()
  raw_list <- list()
  boot_param_list <- list()
  fit_param_list <- list()  # NEW
  error_log <- list()
  
  for (pop in pops) {
    message("Fitting: ", pop)
    
    subdata <- data %>%
      filter(.data[[group_var]] == pop, concentration > 0)
    
    if (nrow(subdata) < 5) {
      error_log[[pop]] <- data.frame(
        pop_rep = pop,
        error_message = "Insufficient data points"
      )
      next
    }
    
    start_vals <- list(
      d = max(subdata$OD),
      b = 10,
      e = median(subdata$concentration)
    )
    
    fit_warnings <- character(0)  # container for warnings
    
    fit <- tryCatch(
      withCallingHandlers(
        expr = eval(bquote(
          nlsLM(
            OD ~ d / (1 + exp(b * (log(concentration) - log(e)))),
            data = .(subdata),
            start = .(start_vals),
            lower = c(d = 0, b = 0.01, e = min(.(subdata)$concentration)),
            upper = c(d = 2, b = 50, e = max(.(subdata)$concentration) * 5),
            control = nls.lm.control(maxiter = 500)
          )
        )),
        warning = function(w) {
          fit_warnings <<- c(fit_warnings, conditionMessage(w))
          invokeRestart("muffleWarning")  # suppress console output
        }
      ),
      error = function(e) {
        error_log[[pop]] <<- data.frame(
          pop_rep = pop,
          error_message = e$message
        )
        return(NULL)
      }
    )
    
    # After fitting, store warnings if any
    if (length(fit_warnings) > 0) {
      warning_log[[pop]] <- data.frame(
        pop_rep = pop,
        warning_message = fit_warnings,
        stringsAsFactors = FALSE
      )
    }
    
    if (is.null(fit)) next
    
    # Store point-estimate parameters
    fit_param_list[[pop]] <- data.frame(
      pop_rep = pop,
      t(coef(fit))
    )
    
    # Bootstrap
    boot_fit <- Boot(fit, f = coef, method = "residual", R = R)
    boot_params <- boot_fit$t
    boot_params <- boot_params[complete.cases(boot_params), ]
    
    # IC50 summary
    ic50_vals <- boot_params[, "e"]
    ic50_summary <- data.frame(
      pop_rep = pop,
      IC50 = coef(fit)["e"],
      IC50_lower = quantile(ic50_vals, 0.025, na.rm = TRUE),
      IC50_upper = quantile(ic50_vals, 0.975, na.rm = TRUE),
      n_boot = nrow(boot_params)
    )
    ic50_list[[pop]] <- ic50_summary
    
    # Bootstrapped parameter values
    boot_df <- as.data.frame(boot_params)
    boot_df$bootstrap_id <- seq_len(nrow(boot_df))
    boot_df[[group_var]] <- pop
    boot_param_list[[pop]] <- boot_df
    
    # Prediction grid
    grid <- data.frame(
      concentration = exp(seq(
        log(min(subdata$concentration)),
        log(max(subdata$concentration)),
        length.out = 100
      ))
    )
    
    pred_matrix <- apply(boot_params, 1, function(pars) {
      d <- pars["d"]
      b <- pars["b"]
      e <- pars["e"]
      d / (1 + exp(b * (log(grid$concentration) - log(e))))
    })
    
    grid$fit <- predict(fit, newdata = grid)
    grid$lower <- apply(pred_matrix, 1, quantile, probs = 0.025, na.rm = TRUE)
    grid$upper <- apply(pred_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)
    grid[[group_var]] <- pop
    
    pred_list[[pop]] <- grid
    raw_list[[pop]] <- subdata
  }
  
  # Combine error messages into a dataframe (if any)
  error_df <- if (length(error_log) > 0) bind_rows(error_log) else data.frame()
  fit_param_df <- if (length(fit_param_list) > 0) bind_rows(fit_param_list) else data.frame()
  warning_df <- if (length(warning_log) > 0) bind_rows(warning_log) else data.frame()
  
  list(
    warning_log = warning_df,
    ic50_table = bind_rows(ic50_list),
    fit_data = bind_rows(pred_list),
    raw_data = bind_rows(raw_list),
    boot_params = bind_rows(boot_param_list),
    fit_params = fit_param_df,         
    error_log = error_df
  )
}


results <- fit_bootstrap_ic50(a2) ### come back here to make sure none of these are hitting against the bounds

View(results$fit_data)
View(results$warning_log)


boot_params1 <- results$boot_params %>% 
  filter(b < 50) |>  ### filtering out the bound hitting ones
  group_by(pop_rep) |> 
  slice_sample(n = 1000)

e2 <- boot_params1 %>%
  mutate(drug = case_when(grepl("amph", pop_rep) ~ "amphotericin",
                          grepl("fluc", pop_rep) ~ "fluconazole",
                          grepl("casp", pop_rep) ~ "caspofungin")) %>% 
  group_by(pop_rep, drug) %>% 
  summarise(mean_e = mean(e),
            upper_e = quantile(e, 0.975),
            lower_e = quantile(e, 0.025)) %>% 
  mutate(evolution_history = case_when(grepl(40, pop_rep) ~ "evolved 40",
                                       grepl(35, pop_rep) ~ "evolved 35",
                                       grepl("fR", pop_rep) ~ "fRS585")) |> 
  mutate(month = case_when(grepl("February", pop_rep) ~ "February",
                           grepl("March", pop_rep) ~ "March",
                           grepl("January", pop_rep) ~ "January")) 


a4 <- a2 |> 
  select(pop_rep, population, rep) |> 
  distinct()
e3 <- e2 |> 
  left_join(a4)

write_csv(e3, "data-processed/ic50-june-results-bootstrapping.csv")


e3 |> 
  ggplot(aes(x = evolution_history, y = mean_e, color = month)) + geom_point() +
  facet_wrap( ~ drug, scales = "free")


e3 |> 
  filter(evolution_history == "fRS585") |> 
  group_by(month, drug)



b2 <- boot_params1 %>%
  group_by(pop_rep) %>% 
  summarise(mean_b = mean(b),
            upper_b = quantile(b, 0.975),
            lower_b = quantile(b, 0.025)) %>% 
  mutate(evolution_history = case_when(grepl(40, pop_rep) ~ "evolved 40",
                                       grepl(35, pop_rep) ~ "evolved 35",
                                       grepl("fR", pop_rep) ~ "fRS585")) %>% 
  mutate(month = case_when(grepl("February", pop_rep) ~ "February",
                                       grepl("March", pop_rep) ~ "March",
                                       grepl("January", pop_rep) ~ "January")) %>% 
  mutate(drug = case_when(grepl("amph", pop_rep) ~ "amphotericin",
                          grepl("fluc", pop_rep) ~ "fluconazole",
                          grepl("casp", pop_rep) ~ "caspofungin"))

ggplot() +
  geom_pointrange(aes(x = pop_rep, y = mean_b, ymin = lower_b, ymax = upper_b, color = evolution_history), data = b2) +
  facet_wrap( ~ drug)
ggsave("figures/mics-pointrange-all-drugs.png", width = 12, height = 5)



  


ggplot() +
  geom_pointrange(aes(x = pop_rep, y = mean_e, ymin = lower_e, ymax = upper_e, color = evolution_history), data = e2) +
  facet_wrap( ~ drug, scales = "free")
ggsave("figures/mics-pointrange-e-all-drugs.png", width = 12, height = 5)


write_csv(results$ic50_table,  "data-processed/ic50-table-june.csv")
# write_csv(results$pred_curves, "data-processed/ic50-pred-curves-june.csv")



amph <- e3 |> 
  filter(drug == "amphotericin") |> 
  group_by(population, month, evolution_history) |> 
  summarise(mean_e = mean(mean_e))


amph |> 
  ggplot(aes( x = evolution_history, y = mean_e, color = month)) + geom_point()

mod1 <- lm(log(mean_e) ~ evolution_history + month, data = amph)
summary(mod1)
library(visreg)
visreg(mod1)


# caspofungin model testing -----------------------------------------------



library(nlme)
model1 <- lme(log(mean_e) ~  evolution_history + month,
              random = ~ 1 | population,
              data = amph)
summary(model1)


casp <- e3 |> 
  filter(drug == "caspofungin") |> 
  group_by(population, month, evolution_history) |> 
  summarise(mean_e = mean(mean_e)) |> 
  filter(mean_e < .03) ### there is one big outlier that's driving this pattern, let's see if there's a reason to why its's so high

mod_casp <- lm(log(mean_e) ~ evolution_history + month, data = casp)
summary(mod_casp)

visreg(mod_casp)

model2 <- lme(log(mean_e) ~  evolution_history + month,
              random = ~ 1 | population,
              data = casp)
summary(model2)


# fluconazole model testing -----------------------------------------------


fluc <- e3 |> 
  filter(drug == "fluconazole") |> 
  group_by(population, month, evolution_history) |> 
  summarise(mean_e = mean(mean_e)) |> 
  ungroup() |> 
  mutate(evolution_history = factor(evolution_history,
                                    levels = c("fRS585", "evolved 35", "evolved 40"))) |> 
  filter(evolution_history %in% c("evolved 35", "evolved 40"))


fluc |> 
  ggplot(aes(x = evolution_history, y = mean_e, color = month)) + geom_point()


mod_fluc <- lm(log(mean_e) ~ evolution_history + month, data = fluc)
mod_fluc2 <- lm(log(mean_e) ~ evolution_history, data = fluc)

summary(mod_fluc)
AIC(mod_fluc, mod_fluc2)

visreg(mod_fluc)

model3 <- lme(log(mean_e) ~  evolution_history + month,
              random = ~ 1 | population,
              data = fluc)
summary(model3)


# =============================================================================
# Fluconazole IC50 — statistical analysis (ignoring month)
# Mirrors script 17 Section 7 approach: Welch t-test (35 vs 40),
# one-sample t-test (each group vs ancestor), Holm correction.
# =============================================================================

library(tidyverse)
library(ggsignif)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")

# ── 1. Per-strain IC50, averaged across months ────────────────────────────────
# e3 labels are "evolved 35" / "evolved 40"; recode to match EVO_COLORS

fluc_strain <- e3 |>
  filter(drug == "fluconazole") |>
  mutate(evolution_history = case_when(
    evolution_history == "evolved 35" ~ "35 evolved",
    evolution_history == "evolved 40" ~ "40 evolved",
    TRUE ~ evolution_history               # fRS585 unchanged
  )) |>
  group_by(population, evolution_history) |>
  summarise(ic50 = mean(mean_e), .groups = "drop")

anc_ic50 <- fluc_strain |> filter(evolution_history == "fRS585") |> pull(ic50)
g35_mic  <- fluc_strain |> filter(evolution_history == "35 evolved") |> pull(ic50)
g40_mic  <- fluc_strain |> filter(evolution_history == "40 evolved") |> pull(ic50)

# ── 2. Normality check (Shapiro-Wilk on log scale) ───────────────────────────
# IC50 is typically log-normally distributed; tests run on log(ic50).

sw_mic <- tibble(
  group = c("35 evolved", "40 evolved"),
  W     = c(shapiro.test(log(g35_mic))$statistic, shapiro.test(log(g40_mic))$statistic),
  p_sw  = c(shapiro.test(log(g35_mic))$p.value,   shapiro.test(log(g40_mic))$p.value)
)
cat("\n=== Shapiro-Wilk normality (log fluconazole IC50) ===\n"); print(sw_mic)

# ── 3. Test helper (same as script 17) ───────────────────────────────────────

run_tests_mic <- function(x, y = NULL, mu = NULL, label_x, label_y = "ancestor", trait) {
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

# ── 4. Run tests + Holm correction ───────────────────────────────────────────
# All tests on log(IC50); mean_x, ref, diff, ci_lo/hi are log-scale (log-ratios).

results_mic <- bind_rows(
  run_tests_mic(log(g35_mic), log(g40_mic),       label_x = "35 evolved", label_y = "40 evolved", trait = "Fluconazole IC50"),
  run_tests_mic(log(g35_mic), mu = log(anc_ic50), label_x = "35 evolved",                         trait = "Fluconazole IC50"),
  run_tests_mic(log(g40_mic), mu = log(anc_ic50), label_x = "40 evolved",                         trait = "Fluconazole IC50")
) |>
  mutate(
    p_welch_holm  = p.adjust(p_welch,  method = "holm"),
    p_wilcox_holm = p.adjust(p_wilcox, method = "holm")
  )

cat("\n=== Fluconazole IC50 — statistical results on log scale (Holm-corrected) ===\n")
print(results_mic)
View(results_mic)

# ── 5. Dot plot with significance annotations ─────────────────────────────────

p_stars <- function(p) case_when(
  p < 0.001 ~ "***",
  p < 0.01  ~ "**",
  p < 0.05  ~ "*",
  TRUE      ~ "ns"
)

plot_df_mic <- fluc_strain |>
  filter(evolution_history != "fRS585") |>
  mutate(evolution_history = factor(evolution_history, levels = c("35 evolved", "40 evolved")))

# Geometric mean ± SE on log scale, back-transformed for plotting
gmeans_mic <- plot_df_mic |>
  group_by(evolution_history) |>
  summarise(
    mean_log = mean(log(ic50)),
    se_log   = sd(log(ic50)) / sqrt(n()),
    mean_val = exp(mean_log),
    ymin     = exp(mean_log - se_log),
    ymax     = exp(mean_log + se_log),
    .groups  = "drop"
  )

y_max_mic   <- max(plot_df_mic$ic50)
bracket_y   <- y_max_mic * 1.12   # horizontal bar position
tip_bottom  <- y_max_mic * 1.04   # where vertical tips meet the data

bracket_ann <- results_mic |>
  filter(comparison == "35 evolved vs 40 evolved") |>
  mutate(label = p_stars(p_welch_holm))

anc_star_df_mic <- results_mic |>
  filter(str_detect(comparison, "vs ancestor")) |>
  mutate(
    evolution_history = factor(str_remove(comparison, " vs ancestor"),
                               levels = c("35 evolved", "40 evolved")),
    label = p_stars(p_welch_holm)
  ) |>
  left_join(gmeans_mic, by = "evolution_history") |>
  mutate(y_pos = ymax * 1.06)

ggplot(plot_df_mic, aes(x = evolution_history, y = ic50, color = evolution_history)) +
  geom_hline(yintercept = anc_ic50, linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(width = 0.12, size = 1.8, alpha = 0.6) +
  geom_pointrange(
    data = gmeans_mic,
    aes(y = mean_val, ymin = ymin, ymax = ymax),
    size = 0.7, linewidth = 1.1
  ) +
  # Manual bracket (ggsignif unreliable on log axes)
  annotate("segment", x = 1, xend = 1, y = tip_bottom, yend = bracket_y, color = "black") +
  annotate("segment", x = 2, xend = 2, y = tip_bottom, yend = bracket_y, color = "black") +
  annotate("segment", x = 1, xend = 2, y = bracket_y,  yend = bracket_y,  color = "black") +
  annotate("text", x = 1.5, y = bracket_y * 1.06,
           label = bracket_ann$label, size = 4.5, color = "black") +
  geom_text(
    data    = anc_star_df_mic,
    aes(x = evolution_history, y = y_pos, label = label),
    color   = "black", size = 4, fontface = "bold", nudge_x = 0.3
  ) +
  scale_y_log10(limits = c(min(plot_df_mic$ic50) * 0.7, y_max_mic * 1.35)) +
  scale_color_manual(values = EVO_COLORS) +
  labs(
    x       = NULL,
    y       = "Fluconazole IC50 (\u03bcg/mL, log scale)",
    caption = paste(
      "Points: individual strains  |  Large point \u00b1 bar: mean \u00b1 SE  |",
      "Dashed line: ancestor (fRS585)\n",
      "Bracket: Welch t-test (Holm-corrected)  |",
      "Stars to right of mean: vs. ancestor (one-sample t-test)"
    )
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        plot.caption = element_text(size = 8, color = "grey40"))

ggsave("figures/fluconazole-ic50-dotplot-sig.png", width = 5, height = 5, dpi = 300)

# ── 6. Export ─────────────────────────────────────────────────────────────────

write_csv(fluc_strain,  "data-processed/fluconazole-ic50-per-strain.csv")
write_csv(results_mic,  "data-processed/stats-results-fluconazole-ic50.csv")


# =============================================================================
# Caspofungin IC50 — statistical analysis (ignoring month)
# Same approach as fluconazole section above. Outlier retained (mean_e >= .03
# excluded from the exploratory `casp` df above is included here).
# =============================================================================

# ── 1. Per-strain IC50, averaged across months ────────────────────────────────

casp_strain <- e3 |>
  filter(drug == "caspofungin") |>
  mutate(evolution_history = case_when(
    evolution_history == "evolved 35" ~ "35 evolved",
    evolution_history == "evolved 40" ~ "40 evolved",
    TRUE ~ evolution_history
  )) |>
  group_by(population, evolution_history) |>
  summarise(ic50 = mean(mean_e), .groups = "drop")

anc_casp  <- casp_strain |> filter(evolution_history == "fRS585")  |> pull(ic50)
g35_casp  <- casp_strain |> filter(evolution_history == "35 evolved") |> pull(ic50)
g40_casp  <- casp_strain |> filter(evolution_history == "40 evolved") |> pull(ic50)

# ── 2. Normality check (Shapiro-Wilk on log scale) ───────────────────────────

sw_casp <- tibble(
  group = c("35 evolved", "40 evolved"),
  W     = c(shapiro.test(log(g35_casp))$statistic, shapiro.test(log(g40_casp))$statistic),
  p_sw  = c(shapiro.test(log(g35_casp))$p.value,   shapiro.test(log(g40_casp))$p.value)
)
cat("\n=== Shapiro-Wilk normality (log caspofungin IC50) ===\n"); print(sw_casp)

# ── 3. Run tests + Holm correction ───────────────────────────────────────────
# All tests on log(IC50); mean_x, ref, diff, ci_lo/hi are log-scale (log-ratios).

results_casp <- bind_rows(
  run_tests_mic(log(g35_casp), log(g40_casp),      label_x = "35 evolved", label_y = "40 evolved", trait = "Caspofungin IC50"),
  run_tests_mic(log(g35_casp), mu = log(anc_casp), label_x = "35 evolved",                         trait = "Caspofungin IC50"),
  run_tests_mic(log(g40_casp), mu = log(anc_casp), label_x = "40 evolved",                         trait = "Caspofungin IC50")
) |>
  mutate(
    p_welch_holm  = p.adjust(p_welch,  method = "holm"),
    p_wilcox_holm = p.adjust(p_wilcox, method = "holm")
  )

cat("\n=== Caspofungin IC50 — statistical results on log scale (Holm-corrected) ===\n")
print(results_casp)

# ── 4. Dot plot with significance annotations ─────────────────────────────────

plot_df_casp <- casp_strain |>
  filter(evolution_history != "fRS585") |>
  mutate(evolution_history = factor(evolution_history, levels = c("35 evolved", "40 evolved")))

gmeans_casp <- plot_df_casp |>
  group_by(evolution_history) |>
  summarise(
    mean_log = mean(log(ic50)),
    se_log   = sd(log(ic50)) / sqrt(n()),
    mean_val = exp(mean_log),
    ymin     = exp(mean_log - se_log),
    ymax     = exp(mean_log + se_log),
    .groups  = "drop"
  )

y_max_casp  <- max(plot_df_casp$ic50)
bracket_y_c  <- y_max_casp * 1.12
tip_bottom_c <- y_max_casp * 1.04

bracket_ann_casp <- results_casp |>
  filter(comparison == "35 evolved vs 40 evolved") |>
  mutate(label = p_stars(p_welch_holm))

anc_star_casp <- results_casp |>
  filter(str_detect(comparison, "vs ancestor")) |>
  mutate(
    evolution_history = factor(str_remove(comparison, " vs ancestor"),
                               levels = c("35 evolved", "40 evolved")),
    label = p_stars(p_welch_holm)
  ) |>
  left_join(gmeans_casp, by = "evolution_history") |>
  mutate(y_pos = ymax * 1.06)

ggplot(plot_df_casp, aes(x = evolution_history, y = ic50, color = evolution_history)) +
  geom_hline(yintercept = anc_casp, linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(width = 0.12, size = 1.8, alpha = 0.6) +
  geom_pointrange(
    data = gmeans_casp,
    aes(y = mean_val, ymin = ymin, ymax = ymax),
    size = 0.7, linewidth = 1.1
  ) +
  annotate("segment", x = 1, xend = 1, y = tip_bottom_c, yend = bracket_y_c, color = "black") +
  annotate("segment", x = 2, xend = 2, y = tip_bottom_c, yend = bracket_y_c, color = "black") +
  annotate("segment", x = 1, xend = 2, y = bracket_y_c,  yend = bracket_y_c,  color = "black") +
  annotate("text", x = 1.5, y = bracket_y_c * 1.06,
           label = bracket_ann_casp$label, size = 4.5, color = "black") +
  geom_text(
    data    = anc_star_casp,
    aes(x = evolution_history, y = y_pos, label = label),
    color   = "black", size = 4, fontface = "bold", nudge_x = 0.3
  ) +
  scale_y_log10(limits = c(min(plot_df_casp$ic50) * 0.7, y_max_casp * 1.35)) +
  scale_color_manual(values = EVO_COLORS) +
  labs(
    x       = NULL,
    y       = "Caspofungin IC50 (\u03bcg/mL, log scale)",
    caption = paste(
      "Points: individual strains  |  Large point \u00b1 bar: mean \u00b1 SE  |",
      "Dashed line: ancestor (fRS585)\n",
      "Bracket: Welch t-test (Holm-corrected)  |",
      "Stars to right of mean: vs. ancestor (one-sample t-test)"
    )
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        plot.caption = element_text(size = 8, color = "grey40"))

ggsave("figures/caspofungin-ic50-dotplot-sig.png", width = 5, height = 5, dpi = 300)

# ── 5. Export ─────────────────────────────────────────────────────────────────

write_csv(casp_strain,   "data-processed/caspofungin-ic50-per-strain.csv")
write_csv(results_casp,  "data-processed/stats-results-caspofungin-ic50.csv")


# =============================================================================
# Amphotericin IC50 — statistical analysis (month as fixed covariate)
#
# 35-evolved is only measured in March; its batch correction relies on the
# month effect estimated from 40-evolved and fRS585 (which span both months).
# Model: lm(log(ic50) ~ evolution_history + month), contrasts via emmeans.
# =============================================================================

library(emmeans)

# ── 1. Data: one row per population × month (averaging reps within month) ────
# Month-level data is needed so month can enter as a covariate.

amph_lm <- e3 |>
  filter(drug == "amphotericin") |>
  mutate(evolution_history = case_when(
    evolution_history == "evolved 35" ~ "35 evolved",
    evolution_history == "evolved 40" ~ "40 evolved",
    TRUE ~ evolution_history
  )) |>
  group_by(population, evolution_history, month) |>
  summarise(ic50 = mean(mean_e), .groups = "drop")

# Per-strain mean across months — used for plotting jittered points only
amph_strain <- amph_lm |>
  group_by(population, evolution_history) |>
  summarise(ic50 = mean(ic50), .groups = "drop")

# ── 2. Normality check on model residuals ────────────────────────────────────

mod_amph_lm <- lm(log(ic50) ~ evolution_history + month, data = amph_lm)

sw_amph <- shapiro.test(residuals(mod_amph_lm))
cat("\n=== Shapiro-Wilk on lm residuals (log amphotericin IC50) ===\n"); print(sw_amph)

# ── 3. emmeans pairwise contrasts (Holm-corrected) ───────────────────────────

emm_amph <- emmeans(mod_amph_lm, ~ evolution_history)

results_amph <- pairs(emm_amph, adjust = "holm") |>
  summary(infer = TRUE) |>
  as_tibble() |>
  rename(comparison = contrast, diff = estimate, ci_lo = lower.CL, ci_hi = upper.CL,
         p_holm = p.value) |>
  mutate(trait = "Amphotericin IC50")

cat("\n=== Amphotericin IC50 — emmeans contrasts on log scale (Holm-corrected) ===\n")
print(results_amph)

# ── 4. Dot plot with significance annotations ─────────────────────────────────

# Raw geometric mean ± SE on log scale (back-transformed) — dots sit in the
# centre of the data. Emmeans estimates are used only for p-values above.
anc_amph <- amph_strain |>
  filter(evolution_history == "fRS585") |>
  summarise(anc = exp(mean(log(ic50)))) |>
  pull(anc)

plot_df_amph <- amph_strain |>
  filter(evolution_history != "fRS585") |>
  mutate(evolution_history = factor(evolution_history, levels = c("35 evolved", "40 evolved")))

gmeans_amph <- plot_df_amph |>
  group_by(evolution_history) |>
  summarise(
    mean_log = mean(log(ic50)),
    se_log   = sd(log(ic50)) / sqrt(n()),
    mean_val = exp(mean_log),
    ymin     = exp(mean_log - se_log),
    ymax     = exp(mean_log + se_log),
    .groups  = "drop"
  )

y_max_amph   <- max(plot_df_amph$ic50)
bracket_y_a  <- y_max_amph * 1.12
tip_bottom_a <- y_max_amph * 1.04

bracket_ann_amph <- results_amph |>
  filter(comparison == "35 evolved - 40 evolved") |>
  mutate(label = p_stars(p_holm))

anc_star_amph <- results_amph |>
  filter(str_detect(comparison, "fRS585")) |>
  mutate(
    evolution_history = factor(str_remove(comparison, " - fRS585"),
                               levels = c("35 evolved", "40 evolved")),
    label = p_stars(p_holm)
  ) |>
  left_join(gmeans_amph, by = "evolution_history") |>
  mutate(y_pos = ymax * 1.06)

ggplot(plot_df_amph, aes(x = evolution_history, y = ic50, color = evolution_history)) +
  geom_hline(yintercept = anc_amph, linetype = "dashed", color = "#000000", linewidth = 0.6) +
  geom_jitter(width = 0.12, size = 1.8, alpha = 0.6) +
  geom_pointrange(
    data = gmeans_amph,
    aes(y = mean_val, ymin = ymin, ymax = ymax),
    size = 0.7, linewidth = 1.1
  ) +
  annotate("segment", x = 1, xend = 1, y = tip_bottom_a, yend = bracket_y_a, color = "black") +
  annotate("segment", x = 2, xend = 2, y = tip_bottom_a, yend = bracket_y_a, color = "black") +
  annotate("segment", x = 1, xend = 2, y = bracket_y_a,  yend = bracket_y_a,  color = "black") +
  annotate("text", x = 1.5, y = bracket_y_a * 1.06,
           label = bracket_ann_amph$label, size = 4.5, color = "black") +
  geom_text(
    data    = anc_star_amph,
    aes(x = evolution_history, y = y_pos, label = label),
    color   = "black", size = 4, fontface = "bold", nudge_x = 0.3
  ) +
  scale_y_log10(limits = c(min(plot_df_amph$ic50) * 0.7, y_max_amph * 1.35)) +
  scale_color_manual(values = EVO_COLORS) +
  labs(
    x       = NULL,
    y       = "Amphotericin B IC50 (\u03bcg/mL, log scale)",
    caption = paste(
      "Points: per-strain means  |  Large point \u00b1 bar: geometric mean \u00b1 SE  |",
      "Dashed line: ancestor (fRS585)\n",
      "Bracket and stars: emmeans contrasts adjusted for month (Holm-corrected)"
    )
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        plot.caption = element_text(size = 8, color = "grey40"))

ggsave("figures/amphotericin-ic50-dotplot-sig.png", width = 5, height = 5, dpi = 300)

# ── 5. Export ─────────────────────────────────────────────────────────────────

write_csv(amph_strain,  "data-processed/amphotericin-ic50-per-strain.csv")
write_csv(results_amph, "data-processed/stats-results-amphotericin-ic50.csv")


amph_strain1 <- amph_strain |> 
  mutate(drug = "amphotericin")

casp_strain1 <- casp_strain |> 
  mutate(drug = "caspofungin")

fluc_strain1 <- fluc_strain |> 
  mutate(drug = "fluconazole")

all_ic50 <- bind_rows(amph_strain1, casp_strain1, fluc_strain1)

write_csv(all_ic50, "data-processed/all-ic50-estimates.csv")
