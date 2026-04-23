# Growth Rate Estimation Methods: Analysis Guide

This directory contains scripts for estimating growth rates from bacterial growth curve data using multiple approaches. This guide explains when to use each method.

## Overview of Methods

### 1. **Growthtools** (`02-growth-rates-growth-tools.R`)
- **Approach**: Fits log-linear (exponential) model to growth data
- **Valid phase**: Exponential growth only (before saturation)
- **Output**: μ (exponential growth rate, units: log(OD)/day)
- **Best for**: Comparing early growth dynamics, competitive fitness assays
- **Key limitation**: Only describes exponential phase; invalid once growth saturates
- **Implementation**: `get.growth.rate()` from growthTools package with multiple methods (sat, flr, lagsat)

### 2. **Logistic Growth Model** (`10-growth-rates-logistic-tpc.R`)
- **Approach**: 3-parameter logistic model: `OD(t) = K / (1 + exp(-μ·(t - t₀)))`
- **Valid phase**: Full growth trajectory including saturation
- **Output**: μ (logistic growth rate), K (carrying capacity), t₀ (time midpoint)
- **Best for**: Characterizing complete growth trajectories, temperature-performance curves
- **Key advantage**: Captures saturation dynamics
- **Implementation**: `nls()` with logistic model formula

### 3. **Lag-Phase Adjusted** (`09-growth-rates-no-lag.R`)
- **Approach**: Growthtools approach applied without explicit lag-phase detection
- **Effect**: Includes all time points in exponential fit (including lag phase)
- **Output**: Similar to growthtools but with different n_obs
- **Best for**: When lag phase is minimal or not well-defined
- **Comparison**: More conservative estimate than lag-aware methods

---

## Choosing a Method for Your Analysis

### For Temperature-Performance Curves (TPC)
**Primary**: Logistic growth model  
**Why**: Need to capture full trajectory across temperature range

### For Competitive Fitness Comparisons
**Primary**: Growthtools with lag-phase adjustment (lagsat)  
**Why**: Exponential growth rate best captures competitive difference

### For Cross-Tolerance Analysis
**Primary**: Logistic (to understand saturation changes across conditions)  
**Secondary**: Growthtools (to compare early growth strategy)

---

## Important Caveats

### The Growthtools Plot Mistake (April 2026)
⚠️ **Never plot growthtools-derived predictions across the saturation phase.**

- Growthtools estimates exponential growth rate (μ)
- Exponential model: `OD(t) = OD₀ · exp(μ·t)` is **only valid before saturation**
- Plotting across saturation is **misleading** about method validity
- See: `12-comparison-logistic-vs-growthtools-CORRECTED.R` for proper visualization

### Data Preparation
1. **Remove blanks/controls** before growth rate fitting (unless needed for normalization)
2. **Check for contamination** - look for wells with unexpected growth patterns
3. **Verify log-linearity** - plot ln(OD) vs. time to confirm exponential assumptions
4. **Identify outliers** - extreme OD values or growth rates need investigation

### Quality Control
- Always plot raw data alongside fitted models
- Check residuals for systematic patterns
- For multiple replicates, verify rate consistency
- Report n_obs (number of points used in fit) and R² (fit quality)

---

## Script Organization

```
R-Scripts-Joey/
├── 02-growth-rates-growth-tools.R          # Growthtools method
├── 09-growth-rates-no-lag.R                # Growthtools (no lag adjustment)
├── 10-growth-rates-logistic-tpc.R          # Logistic model method
├── 12-comparison-logistic-vs-growthtools-CORRECTED.R
│   └── Proper visualization of both methods
├── README-GROWTH-RATE-METHODS.md           # This file
├── CORRECTION-LOG.md                       # Details on the April 2026 correction
└── METHOD-SELECTION-GUIDE*.md              # Additional guidance (if present)
```

---

## Output Files

### CSV Summaries
- `data-processed/growth-rates-growth-tools.csv` - Growthtools results
- `data-processed/all-blocks-growth-no-lag.csv` - Growth-tools without lag adjustment
- `data-processed/growth-rates-logistic.csv` - Logistic model results

### Figures
- `figures/growth-tools-figures/` - Per-well fits from growthtools
- `figures/block{N}/` - TPC figures organized by block
- `figures/comparison_logistic_vs_growthtools_42C_CORRECTED.pdf` - Method comparison

---

## Example: Computing TPC from Logistic Fits

```r
# Load logistic growth rates
gr_logistic <- read_csv("data-processed/growth-rates-logistic.csv")

# Filter to single temperature
gr_25 <- gr_logistic |> filter(test_temperature == 25)

# Summarize by strain
tpc_25 <- gr_25 |>
  group_by(evolution_history, strain) |>
  summarise(
    mean_mu = mean(mu, na.rm = TRUE),
    se_mu = sd(mu, na.rm = TRUE) / sqrt(n()),
    n_reps = n()
  )
```

---

## References

**Growthtools**: [Citation/URL if available]
**Logistic model**: Standard 3-parameter logistics; widely used in microbiology

---

**Last updated**: April 22, 2026  
**Maintained by**: Joanna Bernhardt  
**Key update**: Corrected growthtools visualization (April 2026)
