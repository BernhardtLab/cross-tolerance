# Cross-Tolerance Experiment — Analysis Memory

## Project Overview

Cross-tolerance experiment testing whether yeast strains evolved at high temperature
(35°C or 40°C) show altered thermal performance relative to the ancestor (fRS585).
Three evolution history groups: `35 evolved`, `40 evolved`, `fRS585` (ancestor, n=1 strain).
18 strains per evolved group, 3 experimental blocks, 5 test temperatures (25, 35, 38, 41, 42°C).

## Main Data File

**`data-processed/all-blocks-tpc-experiment.csv`**

One row per OD measurement (~97 time points per well, ~15-min intervals over 1 day).

| Column | Description |
|---|---|
| `days` | Time in days (numeric) |
| `od` | OD600 measurement |
| `well` | Well ID (e.g. `b3`) |
| `strain` | Strain name (e.g. `40_B8`, `fRS585`) |
| `test_temperature` | Assay temperature (25, 35, 38, 41, 42) |
| `block` | Experimental block (1, 2, 3) |
| `evolution_history` | Group: `35 evolved`, `40 evolved`, `fRS585` |

Filter to focal groups: `filter(evolution_history %in% c("35 evolved", "40 evolved", "fRS585"))`

## Color Scheme (use in both scripts)

```r
EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")
```

---

## Script 15 — `R-Scripts-Joey/15-growth-rates.R`

**Purpose:** Fit growthcurver logistic model per well → extract growth metrics →
fit Sharpe-Schoolfield High (SSH) TPCs per strain using empirical AUC (`auc_e`).

**Key constants:** `TREF_K = 288.15` (15°C), `K_B = 8.617333e-5`, `N_STARTS = 500`,
`T_PRED = seq(15, 50, length.out = 500)`

**Outputs — `data-processed/growthcurver/`:**
- `growthcurver-metrics-per-well.csv` — k, n0, r, t_mid, t_gen, auc_l, auc_e, sigma per well
- `tpc-params-auc_e.csv` — SSH params + topt, ctmax, rmax, b80, r2 per strain
- `tpc-predictions-auc_e.csv` — predicted TPC curves (500 points, 15–50°C)

**Outputs — `figures/`:** `tpc-auc_e.png`, `thermal-traits-auc_e.png`

**Note:** `auc_l` (logistic AUC) and `auc_e` (empirical AUC) are nearly identical
(r = 0.999). Script 15 uses `auc_e` as primary metric.

---

## Script 16 — `R-Scripts-Joey/16-growth-rates-gcplyr.R`

**Purpose:** Model-free growth metric estimation via gcplyr smoothing + derivatives →
fit SSH TPCs per strain using AUC only.

**Key constants:** `SMOOTH_N = 5` (moving-average window, ~75 min),
`BLANK_OD = min(od)` (global instrument blank, ~0.067)

**Note:** Trait column is `tmax` (not `ctmax`) in `tpc_params_auc` and output CSVs.

**Sections:**
1. Setup
2. SSH model functions (same as script 15)
3. Load data
4. gcplyr per-well metrics: `mu_max`, `doub_time`, `lag`, `auc_gc`
5. SSH TPC fitting — AUC only
6. Plots (TPC curves, thermal traits strip chart)
7. Thermal traits dot plot with ancestor reference line
8. Statistical tests (Welch + one-sample t-tests vs ancestor, Holm-corrected)
9. Dot plot with significance annotations (`ggsignif` brackets + `geom_text` stars)
10. AUC at 41°C and 42°C — predicted from TPC fit and observed; tests + dot plots
11. Per-strain TPC PDF
12. Export

**Outputs — `data-processed/gcplyr/`:**
- `gcplyr-metrics-per-well.csv` — mu_max, doub_time, lag, auc_gc per well
- `tpc-params-auc-gcplyr.csv` — SSH params + topt, tmax, rmax, b80, r2 per strain
- `tpc-predictions-auc-gcplyr.csv` — predicted TPC curves

**Outputs — `figures/`:**
- `tpc-auc-gcplyr.png` — TPC curves per evolution history group
- `tpc-auc-gcplyr-faceted.png` — TPC curves faceted by evolution history
- `thermal-traits-auc-gcplyr.png` — jitter + crossbar strip chart (topt, tmax, b80)
- `thermal-traits-dotplot-auc-gcplyr.png` — dot plot with group means ± SE and ancestor reference line
- `thermal-traits-dotplot-sig-auc-gcplyr.png` — same with significance annotations
- `auc-dotplot-predicted-gcplyr.png` — AUC at 41°C and 42°C from TPC-predicted values, with sig annotations
- `auc-dotplot-observed-gcplyr.png` — AUC at 41°C and 42°C from observed per-strain means, with sig annotations

**Why AUC-only (not µ_max) for TPC fitting:**
µ_max SSH fits produce biologically implausible CTmax (~45°C vs ~42°C from AUC).
Root cause: with only one temperature above Topt (42°C) and very noisy µ_max there,
the SSH model fits low deactivation energy (eh ~7 eV vs ~20 eV from AUC), causing
the curve to decline slowly and extrapolate CTmax far beyond the data range.
Tightening the `th` bound (to 320K) and `eh` lower bound (to 10) both reduced CTmax
slightly but at a meaningful r² cost (~0.03–0.04 drop), confirming the data simply
cannot constrain CTmax via µ_max for this experimental design.

---

## Key Statistical Results (June 2026)

### Script 15 (growthcurver, `auc_e`)

Welch two-sample t-tests, Holm-corrected, n = 18 strains per evolved group:

| Trait | 35 evolved | 40 evolved | Δ (40−35) | 95% CI | p (Holm) |
|---|---|---|---|---|---|
| Topt | 38.9°C | 39.5°C | +0.64°C | [+0.26, +1.01] | 0.003 |
| CTmax | 42.2°C | 42.7°C | +0.53°C | [+0.20, +0.86] | 0.003 |

Ancestor fRS585: Topt = 39.5°C, CTmax = 42.1°C

### Script 16 (gcplyr, `auc_gc`)

**Statistical approach:**
- Group vs group: Welch two-sample t-test
- Group vs ancestor (n=1 strain): one-sample t-test with ancestor value as µ₀
- Holm correction applied within each trait across all 3 comparisons
- Wilcoxon (nonparametric) alternatives agree with all parametric conclusions
  despite mild non-normality in `tmax` (Shapiro-Wilk p < 0.05 for both evolved groups)

Ancestor fRS585: Topt = 39.4°C, Tmax = 42.2°C

**Between evolved groups (Welch t-test, Holm-corrected):**

| Trait | 35 evolved mean | 40 evolved mean | Δ (40−35) | 95% CI | p (Holm) |
|---|---|---|---|---|---|
| Topt | 38.8°C | 39.5°C | +0.75°C | [+0.37, +1.13] | 0.001 |
| Tmax | 42.3°C | 42.7°C | +0.47°C | [+0.14, +0.79] | 0.014 |

**Each group vs ancestor (one-sample t-test, Holm-corrected):**

| Trait | Group | Δ vs ancestor | 95% CI | p (Holm) |
|---|---|---|---|---|
| Topt | 35 evolved | −0.67°C | [−1.01, −0.34] | 0.001 |
| Topt | 40 evolved | +0.08°C | [−0.13, +0.29] | 0.444 |
| Tmax | 35 evolved | +0.08°C | [−0.02, +0.18] | 0.111 |
| Tmax | 40 evolved | +0.55°C | [+0.23, +0.86] | 0.006 |

**Interpretation — divergent trajectories:**
- 35 evolved: Topt shifted down ~0.7°C below ancestor; Tmax unchanged
- 40 evolved: Topt maintained at ancestral level; Tmax elevated ~0.55°C above ancestor
- Trait shift ~10–13% of the 5°C selection differential
- fRS585 is a single strain; its values are reference points, not testable

### Script 16 — AUC at 41°C and 42°C (June 2026)

Both predicted (SSH model evaluated at T) and observed (per-strain mean across blocks)
AUCs were compared. Results are nearly identical between methods, confirming robustness.
Holm correction applied within each temperature across 3 comparisons.

**At 41°C:**

| Comparison | Δ AUC | 95% CI | p (Welch, Holm) |
|---|---|---|---|
| 35 evolved vs 40 evolved | −0.293 | [−0.347, −0.239] | <0.001 |
| 35 evolved vs ancestor | −0.143 | [−0.189, −0.097] | <0.001 |
| 40 evolved vs ancestor | +0.150 | [+0.118, +0.182] | <0.001 |

**At 42°C:**

| Comparison | Δ AUC | 95% CI | p (Welch, Holm) |
|---|---|---|---|
| 35 evolved vs 40 evolved | −0.123 | [−0.186, −0.060] | 0.002 |
| 35 evolved vs ancestor | ~0 | [−0.011, +0.012] | ns |
| 40 evolved vs ancestor | +0.124 | [+0.061, +0.186] | 0.002 |

**Interpretation:** Extends the Topt/Tmax story to raw AUC performance:
- At 41°C both groups have diverged from the ancestor (35-evolved down, 40-evolved up)
- At 42°C the pattern mirrors Tmax: 35-evolved matches ancestor; 40-evolved is elevated
- The 35-evolved group shows high among-strain variance at 42°C

### TPC fit quality note — strain 40_D4

40_D4 has the highest Tmax in the 40-evolved group (44.1°C) but the lowest r² (0.793)
and lowest deactivation energy (eh = 11.5 eV vs 13–20 eV for comparable strains).
The steep observed AUC drop at 42°C is real and consistent across all 3 replicates,
but the SSH model cannot simultaneously fit the warming limb shape and the sharp
decline with only 5 temperature points, resulting in an overly gradual deactivation
tail and extrapolated Tmax beyond the data range. This is a fitting artifact, not a
data artifact; it inflates the 40-evolved Tmax mean slightly.

---

## Per-Well Diagnostic Plots

- `figures/auc-empirical/T{25,35,38,41,42}/` — 585 individual PNGs, one per well,
  showing raw OD over time with empirical AUC region shaded (auc_e)
- `figures/logistic-fits-per-well.pdf` — 30-page PDF of growthcurver logistic fits
  (note: known geom_line grouping bug in the PDF version; individual PNGs are correct)
