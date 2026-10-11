# Thermal Performance Curve Analysis: Comparison of Growth Rate Methods

## Overview

This analysis compares two methods for estimating bacterial growth rates and evaluates whether the choice of method affects conclusions about thermal adaptation (evolution history effects on thermal optimum and critical thermal maximum).

## Methods Compared

1. **GrowthTools package** - Uses multiple automated growth rate estimation methods (sat, flr, lagsat) and selects the best model
2. **3-parameter Logistic** - Fits a simple logistic model: K / (1 + exp(-r*(t - t_mid)))

## Data

- **Strains**: 37 bacterial strains (18 "35°C evolved", 18 "40°C evolved", 1 ancestral "fRS585")
- **Temperatures tested**: 25°C, 35°C, 38°C, 41°C, 42°C
- **Growth rate measurements**: 585 wells total

## Key Findings

### 1. Growth Rate Comparison

**Correlation between methods**: r = 0.932 (very strong agreement)

The two methods give highly correlated results, though the logistic method produces rates approximately 17% lower on average (regression: logistic = -1.27 + 0.83 × growthTools).

### 2. Thermal Optimum (Topt)

**GrowthTools method:**
- 35°C evolved: Topt = 37.07 ± 1.00°C
- 40°C evolved: Topt = 38.68 ± 1.00°C  
- fRS585: Topt = 37.53°C (n=1)

**Logistic method:**
- 35°C evolved: Topt = 38.79 ± 0.58°C
- 40°C evolved: Topt = 39.77 ± 0.58°C
- fRS585: Topt = 39.27°C (n=1)

**Statistical test (evolution history effect on Topt):**
- GrowthTools: F = 11.68, p = 0.0001 ***
- Logistic: F = 13.03, p = 0.00006 ***

**Conclusion**: Both methods show **consistent, significant effects** of evolution history on Topt. Strains evolved at 40°C have thermal optima approximately **1.0-1.6°C higher** than 35°C-evolved strains (p < 0.001 for both methods).

### 3. Critical Thermal Maximum (CTmax)

**GrowthTools method:**
- 35°C evolved: CTmax = 44.03 ± 0.98°C
- 40°C evolved: CTmax = 43.56 ± 0.98°C
- fRS585: CTmax = 43.83°C (n=1)

**Logistic method:**
- 35°C evolved: CTmax = 42.04 ± 0.32°C
- 40°C evolved: CTmax = 42.35 ± 0.32°C
- fRS585: CTmax = 42.02°C (n=1)

**Statistical test (evolution history effect on CTmax):**
- GrowthTools: F = 1.00, p = 0.378 (NOT significant)
- Logistic: F = 4.14, p = 0.025 *

**Conclusion**: The choice of growth rate method **DOES affect** conclusions about CTmax evolution:
- GrowthTools suggests **no significant evolution history effect** on CTmax
- Logistic method suggests **40°C-evolved strains have ~0.3°C higher CTmax** (p = 0.025)

## Important Differences Between Methods

| Aspect | GrowthTools | Logistic |
|--------|------------|----------|
| Topt effect | p < 0.001 | p < 0.001 |
| CTmax effect | p = 0.378 (NS) | p = 0.025 * |
| Conclusion | No CTmax evolution | Modest CTmax evolution |
| Absolute CTmax values | ~44°C | ~42°C |

## Interpretation

The substantial difference in CTmax conclusions highlights that **methodological choices matter** when studying thermal tolerance evolution. The logistic method's greater sensitivity to evolution history effects might reflect:

1. Different assumptions about growth curve shape
2. Different weighting of high-temperature data points
3. Systematic biases in how each method handles temperature-dependent variation

The **consistent Topt signal** across both methods suggests that thermal optimum evolution is robust to methodological choice, while CTmax estimates may be more sensitive.

## Outputs Generated

All results saved to `data-processed/tpc-comparison/`:
- `01-method-comparison.png` - Scatterplot comparing growth rates
- `02-topt-comparison.png` - Topt estimates by method
- `03-ctmax-comparison.png` - CTmax estimates by method
- `growth-rates-comparison.csv` - All growth rate estimates (both methods)
- `thermal-traits-all-methods.csv` - SSH model parameters and thermal traits
- `thermal-traits-growthtools.csv` - Results from growthTools method only
- `thermal-traits-logistic.csv` - Results from logistic method only
- `summary-thermal-traits.csv` - Aggregated summary statistics

## Code Location

Scripts:
- `03-growth-rates-logistic.R` - Fits 3-parameter logistic to growth curves
- `04-compare-methods-tpc.R` - Comparison and TPC fitting analysis
