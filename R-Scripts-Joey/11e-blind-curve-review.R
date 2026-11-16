# =============================================================================
# 11e-blind-curve-review.R
# Group-blinded, randomised PDF of every 4PL dose-response curve for visual
# fit-quality review (criteria in 11d-MICs.R sec. 3a).
#
# Requires `mic_data2` and `plate_ic50` from 11d-MICs.R (sections 1-3).
#
# Each curve (population x drug x plate) gets a random code. Panels show only
# the code, the drug, the OD data used in the fit, and the fitted curve + IC50
# line. Strain ID, evolution history (35/40/ancestor), plate, and numeric IC50
# are hidden, and all curves are drawn in the same colour. Ancestor curves are
# blinded as well, so they are judged by the same criteria.
#
# Outputs:
#   figures/blind-review-curves.pdf          (give to reviewer)
#   data-processed/blind-review-scoresheet.csv (give to reviewer; codes only)
#   data-processed/blind-review-key.csv      (do NOT give to reviewer)
# =============================================================================

library(tidyverse)

stopifnot(exists("mic_data2"), exists("plate_ic50"))

set.seed(5817)

curve_keys <- c("population", "drug", "month", "set", "rep")

# One row per curve, in random order, with a random code
blind_key <- mic_data2 |>
  distinct(across(all_of(curve_keys)), evolution_history) |>
  left_join(plate_ic50 |> select(all_of(curve_keys), ic50, bot, top, slope, converged),
            by = curve_keys) |>
  mutate(converged = coalesce(converged, FALSE)) |>
  slice_sample(prop = 1) |>
  mutate(
    review_order = row_number(),
    code = sprintf("C%03d", sample(100:999, n()))
  )

stopifnot(!anyDuplicated(blind_key$code))

# Data points actually used in the fit (zero conc already replaced by min/10)
blind_raw <- mic_data2 |>
  inner_join(blind_key |> select(all_of(curve_keys), code, review_order),
             by = curve_keys)

blind_curves <- blind_key |>
  filter(converged) |>
  left_join(
    blind_raw |>
      group_by(code) |>
      summarise(min_c = min(concentration), max_c = max(concentration)),
    by = "code"
  ) |>
  mutate(curve = pmap(list(ic50, bot, top, slope, min_c, max_c), \(e, b, t, s, lo, hi) {
    conc <- exp(seq(log(lo), log(hi), length.out = 300))
    tibble(concentration = conc,
           od_pred = b + (t - b) / (1 + exp(s * (log(conc) - log(e)))))
  })) |>
  select(code, review_order, curve) |>
  unnest(curve)

blind_ic50 <- blind_key |> filter(converged) |> select(code, review_order, ic50)

# Panel label: code + drug only; flag curves with no converged fit
panel_labels <- blind_key |>
  transmute(code, review_order,
            panel = paste0(code, "  |  ", drug,
                           if_else(converged, "", "  (no converged fit)")))

add_panel <- function(df) {
  df |>
    select(-any_of("review_order")) |>
    inner_join(panel_labels, by = "code") |>
    mutate(panel = fct_reorder(panel, review_order))
}

blind_raw    <- add_panel(blind_raw)
blind_curves <- add_panel(blind_curves)
blind_ic50   <- add_panel(blind_ic50)

# -----------------------------------------------------------------------------
# PDF: 12 panels per page, in randomised order
# -----------------------------------------------------------------------------

PER_PAGE <- 12
n_pages  <- ceiling(nrow(blind_key) / PER_PAGE)
page_of  <- function(order) (order - 1) %/% PER_PAGE + 1

pdf("figures/blind-review-curves.pdf", width = 11, height = 8.5)

for (pg in seq_len(n_pages)) {
  raw_pg   <- blind_raw    |> filter(page_of(review_order) == pg)
  curve_pg <- blind_curves |> filter(page_of(review_order) == pg)
  ic50_pg  <- blind_ic50   |> filter(page_of(review_order) == pg)

  p <- ggplot(raw_pg, aes(x = concentration, y = OD)) +
    geom_point(size = 1.8, alpha = 0.8) +
    geom_line(data = curve_pg, aes(y = od_pred), linewidth = 0.8) +
    geom_vline(data = ic50_pg, aes(xintercept = ic50),
               linetype = "dashed", linewidth = 0.5) +
    scale_x_log10() +
    facet_wrap(~ panel, ncol = 4, nrow = 3, scales = "free") +
    labs(
      title    = sprintf("Blinded curve review — page %d of %d", pg, n_pages),
      subtitle = "Points: blank-corrected OD  |  Line: fitted 4PL  |  Dashed: fitted IC50",
      x        = "Concentration (log scale)",
      y        = "OD (blank-corrected)"
    ) +
    theme_classic(base_size = 11) +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"))

  print(p)
}

dev.off()

# -----------------------------------------------------------------------------
# Score sheet (for reviewer) and unblinding key (keep separate)
# -----------------------------------------------------------------------------

blind_key |>
  arrange(review_order) |>
  transmute(page = page_of(review_order), code, drug,
            exclude = NA, reason = NA_character_, notes = NA_character_) |>
  write_csv("data-processed/blind-review-scoresheet.csv", na = "")

blind_key |>
  arrange(review_order) |>
  select(code, review_order, all_of(curve_keys), evolution_history,
         converged, ic50, bot, top, slope) |>
  write_csv("data-processed/blind-review-key.csv")

cat(sprintf("Saved: figures/blind-review-curves.pdf (%d curves, %d pages)\n",
            nrow(blind_key), n_pages))
