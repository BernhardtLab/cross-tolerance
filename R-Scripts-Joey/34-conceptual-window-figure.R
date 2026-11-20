# =============================================================================
# 34-conceptual-window-figure.R
# Conceptual figure: fitted TPCs of the ancestor (fRS585) and one representative
# 35 C-evolved and one 40 C-evolved strain. All three curves are compared over
# the shared ancestor window (ancestor T_opt to T_max, 39.4-42.2 C). The dashed
# line for each curve is its mean fitted growth over that shared window
# (Script 32 mean_fixed_window definition).
#
# Representative strain: within each evolution group, the strain whose
# shared-window mean is closest to the group median (not chosen for its
# curve shape).
#
# Inputs:  data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv
#          data-processed/gcplyr/tpc-predictions-auc-gcplyr-17.csv
# Outputs: figures/conceptual-window-34.png
# =============================================================================

library(tidyverse)

OUT  <- "data-processed"
FIGS <- "figures"

GROUP_COLORS <- c("ancestor" = "black", "35 evolved" = "#0E63FF", "40 evolved" = "#FA3208")

tpc_par  <- read_csv(file.path(OUT, "gcplyr/tpc-params-auc-gcplyr-17.csv"), show_col_types = FALSE)
tpc_pred <- read_csv(file.path(OUT, "gcplyr/tpc-predictions-auc-gcplyr-17.csv"), show_col_types = FALSE)

# Trapezoid mean of a fitted curve over [lo, hi]
window_mean <- function(temp, pred, lo, hi) {
  x <- sort(unique(c(lo, hi, temp[temp > lo & temp < hi])))
  y <- approx(temp, pred, xout = x)$y
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2) / (hi - lo)
}

anc_win <- tpc_par |> filter(evolution_history == "fRS585") |> select(topt, tmax)
stopifnot(nrow(anc_win) == 1)
anc_topt <- anc_win$topt
anc_tmax <- anc_win$tmax

# Shared-window mean for every strain, used to pick representatives
strain_win <- tpc_pred |>
  group_by(strain, evolution_history) |>
  summarise(wmean_shared = window_mean(temp, pred, anc_topt, anc_tmax), .groups = "drop")

pick_representative <- function(group) {
  d <- strain_win |> filter(evolution_history == group)
  d$strain[which.min(abs(d$wmean_shared - median(d$wmean_shared)))]
}

rep_35 <- pick_representative("35 evolved")
rep_40 <- pick_representative("40 evolved")
cat("Representative 35 evolved strain:", rep_35, "\n")
cat("Representative 40 evolved strain:", rep_40, "\n")

curves <- tibble(
  label  = c("ancestor", "35 evolved", "40 evolved"),
  strain = c("fRS585", rep_35, rep_40)
) |>
  mutate(
    curve = map(strain, \(s) tpc_pred |> filter(strain == s) |> arrange(temp)),
    wmean = map_dbl(curve, \(cv) window_mean(cv$temp, cv$pred, anc_topt, anc_tmax))
  )

curve_dat <- curves |>
  select(label, curve) |>
  unnest(curve)

cat("\nMean growth over the ancestor window (39.4–42.2 C):\n")
print(curves |> select(label, strain, wmean) |> mutate(wmean = round(wmean, 3)))

ggplot() +
  annotate("rect", xmin = anc_topt, xmax = anc_tmax, ymin = -Inf, ymax = Inf,
           fill = "grey90") +
  geom_line(data = curve_dat, aes(x = temp, y = pred, color = label), linewidth = 1) +
  geom_segment(data = curves,
               aes(x = anc_topt, xend = anc_tmax, y = wmean, yend = wmean, color = label),
               linetype = "dashed", linewidth = 0.6) +
  annotate("text", x = (anc_topt + anc_tmax) / 2, y = 1.07,
           label = "ancestor window", size = 3, color = "grey30", vjust = 1) +
  scale_color_manual(values = GROUP_COLORS, name = NULL) +
  scale_x_continuous(breaks = seq(20, 50, 5)) +
  coord_cartesian(xlim = c(20, 46), ylim = c(0, max(curve_dat$pred) * 1.1), expand = FALSE) +
  labs(x = "Temperature (\u00b0C)", y = "Growth rate (AUC)") +
  theme_classic(base_size = 12) +
  theme(legend.position = "top")

ggsave(file.path(FIGS, "conceptual-window-34.png"),
       width = 5.5, height = 4.3, dpi = 300, bg = "white")
