# Growth Rate Estimation: Correction Log

## Correction: Growthtools Line Visualization (April 22, 2026)

### Issue Identified
The comparison plot between logistic and growthtools growth rate estimates incorrectly plotted the **growthtools-derived exponential line across the entire time course**, including the saturation phase where the exponential model is invalid.

### Root Cause
- **Growthtools** estimates exponential growth rates (μ) assuming log-linear growth: `ln(OD) = ln(OD₀) + μ·t`
- This assumption is **only valid during the exponential phase** before saturation
- In saturation, growth decelerates and levels off, violating the exponential assumption
- The previous plot created `OD(t) = OD₀·exp(μ·t)` for the entire time span, which is misleading

### Solution Implemented
Script: `R-Scripts-Joey/12-comparison-logistic-vs-growthtools-CORRECTED.R`

**Key changes:**
1. **Exponential phase detection**: Automatically identifies where exponential growth ends by fitting log-linear models to progressively larger time windows and detecting where R² drops significantly
2. **Truncated growthtools line**: Blue dashed line now only extends from t₀ to the detected end of exponential phase
3. **Full-range logistic line**: Red solid line continues across the entire trajectory, showing transition to saturation
4. **Visual clarity**: Subtitle on each plot shows the detected exponential phase end time

### Technical Details: Exponential Phase Detection

```r
find_exponential_phase(days, od, min_points = 5)
```

Algorithm:
- Fit log-linear models: `ln(OD[1:i]) ~ days[1:i]` for i = min_points to n
- Track R² values for each fit
- Exponential phase ends when:
  - R² drops > 0.05 (indicating model mismatch)
  - OR R² < 0.90 (indicating poor fit quality)

### Comparison: Before vs. After

**BEFORE (Incorrect):**
- Growthtools line extended full duration (0 to ~1.0 days shown)
- Suggested exponential model works for saturation region
- Visually misleading about method validity

**AFTER (Correct):**
- Growthtools line extends only ~0.3-0.5 days (exp phase)
- Clearly shows where exponential assumptions break down
- Red logistic line fills the gap, showing actual saturation trajectory

### Output Files
- `figures/comparison_logistic_vs_growthtools_42C_CORRECTED.pdf`
- `figures/comparison_logistic_vs_growthtools_42C_CORRECTED.png`

These replace the previous (incorrect) versions.

### Why This Matters
- **Growthtools** provides estimates of exponential growth rate (μ) — useful for comparing early growth dynamics
- **Logistic models** characterize full growth trajectory including carrying capacity
- Conflating these approaches or misrepresenting their valid domains can lead to incorrect interpretations
- The corrected visualization makes each method's domain of validity explicit

### Recommendations for Future Use
1. **When using growthtools estimates**: Only interpret μ as the exponential growth rate during exponential phase
2. **For saturation-phase analysis**: Use logistic or other saturation-aware models
3. **In reports/papers**: Clearly state which phase of growth is being analyzed
4. **For QC**: Plot both raw data and fitted models; verify that fits match actual growth dynamics

---

**Script author**: Joanna Bernhardt  
**Correction implemented**: April 22, 2026  
**Associated script**: `12-comparison-logistic-vs-growthtools-CORRECTED.R`
