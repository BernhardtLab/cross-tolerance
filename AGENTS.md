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

**Sections:**
1. Setup
2. SSH model functions (same as script 15)
3. Load data
4. gcplyr per-well metrics: `mu_max`, `doub_time`, `lag`, `auc_gc`
5. SSH TPC fitting — AUC only
6. Plots
7. Export

**Outputs — `data-processed/gcplyr/`:**
- `gcplyr-metrics-per-well.csv` — mu_max, doub_time, lag, auc_gc per well
- `tpc-params-auc-gcplyr.csv` — SSH params + topt, ctmax, rmax, b80, r2 per strain
- `tpc-predictions-auc-gcplyr.csv` — predicted TPC curves

**Outputs — `figures/`:** `tpc-auc-gcplyr.png`, `thermal-traits-auc-gcplyr.png`

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

Using strain-level SSH estimates from `tpc-params-auc_e.csv` (script 15),
Welch t-tests (Holm-corrected, n = 18 strains per evolved group):

| Trait | 35 evolved | 40 evolved | Δ (40−35) | 95% CI | p (Holm) |
|---|---|---|---|---|---|
| Topt | 38.9°C | 39.5°C | +0.64°C | [+0.26, +1.01] | 0.003 |
| CTmax | 42.2°C | 42.7°C | +0.53°C | [+0.20, +0.86] | 0.003 |

- Ancestor fRS585 (n=1 strain): Topt = 39.5°C, CTmax = 42.1°C
- Trait shift scales at ~10–13% of the 5°C selection differential
- 40 evolved maintained ancestral Topt but elevated CTmax; 35 evolved reduced Topt
  below ancestor with little CTmax change — divergent trajectories
- fRS585 is a single strain; its values are reference points, not testable

## Per-Well Diagnostic Plots

- `figures/auc-empirical/T{25,35,38,41,42}/` — 585 individual PNGs, one per well,
  showing raw OD over time with empirical AUC region shaded (auc_e)
- `figures/logistic-fits-per-well.pdf` — 30-page PDF of growthcurver logistic fits
  (note: known geom_line grouping bug in the PDF version; individual PNGs are correct)
