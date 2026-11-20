# =============================================================================
# 34-conceptual-window-figure.R
# Conceptual figure: fitted TPC of the ancestor (fRS585) with the ancestor
# window (T_opt to T_max) shaded and the window mean marked.
# Same definition as the ancestor-window mean in Scripts 32, 33 and 22.
#
# Inputs:  data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv
#          data-processed/gcplyr/tpc-predictions-auc-gcplyr-17.csv
# Outputs: figures/conceptual-window-34.png
# =============================================================================

library(tidyverse)

OUT  <- "data-processed"
FIGS <- "figures"

tpc_par  <- read_csv(file.path(OUT, "gcplyr/tpc-params-auc-gcplyr-17.csv"), show_col_types = FALSE)
tpc_pred <- read_csv(file.path(OUT, "gcplyr/tpc-predictions-auc-gcplyr-17.csv"), show_col_types = FALSE)

anc_par   <- tpc_par |> filter(evolution_history == "fRS585")
stopifnot(nrow(anc_par) == 1)
anc_curve <- tpc_pred |> filter(evolution_history == "fRS585") |> arrange(temp)
stopifnot(nrow(anc_curve) > 0)

topt <- anc_par$topt
tmax <- anc_par$tmax

# Trapezoid integral over the window, with endpoints added by interpolation
x <- sort(unique(c(topt, tmax, anc_curve$temp[anc_curve$temp > topt & anc_curve$temp < tmax])))
y <- approx(anc_curve$temp, anc_curve$pred, xout = x)$y
window_mean <- sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2) / (tmax - topt)

ggplot(anc_curve, aes(x = temp, y = pred)) +
  annotate("rect", xmin = topt, xmax = tmax, ymin = -Inf, ymax = Inf,
           fill = "grey88") +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = window_mean, linetype = "dashed", linewidth = 0.5) +
  geom_vline(xintercept = c(topt, tmax), linetype = "dotted", linewidth = 0.5) +
  annotate("text", x = topt, y = Inf, label = "T[opt]", parse = TRUE,
           vjust = 1.5, hjust = 1.1, size = 4) +
  annotate("text", x = tmax, y = Inf, label = "T[max]", parse = TRUE,
           vjust = 1.5, hjust = -0.1, size = 4) +
  annotate("text", x = 33, y = window_mean, label = "mean growth over window",
           vjust = -0.6, size = 3.8) +
  scale_x_continuous(breaks = seq(20, 50, 5)) +
  coord_cartesian(xlim = c(20, 46), ylim = c(0, max(anc_curve$pred) * 1.1), expand = FALSE) +
  labs(x = "Temperature (\u00b0C)", y = "Growth rate (AUC)") +
  theme_classic(base_size = 12)

ggsave(file.path(FIGS, "conceptual-window-34.png"),
       width = 5, height = 3.8, dpi = 300, bg = "white")
