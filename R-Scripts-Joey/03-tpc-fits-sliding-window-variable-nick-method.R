library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())


mu_mean <- read_csv("data-processed/all-blocks-growth-sliding-window-variable.csv") |>
  filter(evolution_history %in% c("35 evolved", "40 evolved", "fRS585")) |> 
  # filter(window_size == 5) |>       # use the chosen window size
  filter(!is.na(mu)) |>     # drop failed fits
  # anti_join(excluded_series, by = c("well", "block", "test_temperature")) |>
  rename(temp = test_temperature)


mu_mean |> 
  ggplot(aes(x = temp, y = mu)) + geom_point()



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


K_B      <- 8.617333e-5
TREF_K   <- 288.15
CTMAX_CAP <- 50.0
N_STARTS  <- 500
set.seed(42)


BOUNDS_LO <- c(r_tref = 0.01, e = 0.01, eh =  0.5, th = 303.0)
BOUNDS_HI <- c(r_tref = 50.0, e =  3.0, eh = 50.0, th = 335.0)
START_LO  <- c(r_tref =  0.5, e =  0.1, eh =  0.5, th = 305.0)
START_HI  <- c(r_tref = 20.0, e =  2.0, eh = 20.0, th = 325.0)

T_pred <- seq(15, 50, length.out = 300)

fit_params_ssh <- list()
fit_preds_ssh  <- list()

for (sid in unique(mu_mean$strain)) {
  d     <- mu_mean %>% filter(strain == sid)
  evo   <- d$evolution_history[1]
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
  ggplot(aes(x = evo_history, y = topt)) + geom_point()

library(plotrix)
tpc_params_sum <- tpc_params |> 
  group_by(evo_history) |> 
  summarise(mean_tmax = mean(ctmax),
            se_tmax = std.error(ctmax))



tpc_params |> 
  ggplot(aes(x = evo_history, y = ctmax)) + geom_point() +
  geom_pointrange(aes(x = evo_history, y = mean_tmax, ymin = mean_tmax - se_tmax, ymax = mean_tmax + se_tmax), data = tpc_params_sum, color = "blue")




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


EVO_COLORS <- c(
  "40°C Evolved" = "#FA3208",
  "35°C Evolved" = "#0E63FF",
  "Ancestor"     = "#000000"
)


OUT        <- "R-Script-Drafts/TPC-Analysis/Processed_Outputs"
plot_ssh_grid(tpc_params,
              file.path(OUT, "fig_ssh_all_strains-variable.png"),
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