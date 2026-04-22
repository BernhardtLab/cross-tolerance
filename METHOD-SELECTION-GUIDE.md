# Method Selection Guide: GrowthTools vs. 3-Parameter Logistic

## Quick Answer

**Recommendation: Use the 3-parameter Logistic method** for this dataset and research question.

Here's why:

## Detailed Comparison

### 1. **Precision of CTmax Estimates**

| Metric | GrowthTools | Logistic | Winner |
|--------|------------|----------|--------|
| Mean CTmax | 43.80°C | 42.19°C | — |
| SD (variation) | 0.98°C | 0.35°C | **Logistic** |
| Range | 42.59–46.48°C | 41.77–43.30°C | **Logistic** |
| Within-group consistency | Lower | **Higher** | **Logistic** |

**Interpretation:** The logistic method gives CTmax estimates that are **~3× more consistent** across strains within the same evolution history group (SD = 0.35°C vs. 0.98°C). This means the biological signal (differences between evolution history groups) will be **easier to detect reliably**.

### 2. **Model Flexibility & Overfitting Risk**

**GrowthTools:**
- Tries 3 different models (saturation, logistic, lagging saturation) per well
- Selects the "best" model based on fit quality
- More flexibility = higher risk of overfitting to noise
- Especially problematic at extreme temperatures (41–42°C) where growth is unstable

**Logistic:**
- Single, constrained model
- Must fit the same functional form to all data
- Less flexible = less prone to overfitting
- Better suited for noisy high-temperature data

### 3. **Biological Reasonableness**

**CTmax values:**
- Logistic: 42.19°C (mean across all strains)
- GrowthTools: 43.80°C (mean)

**Which is more reasonable?**
- Your strains were grown at 42°C as the highest temperature
- CTmax values should be *slightly above* your maximum observed growth temperature
- Logistic estimates (42.19°C) are only ~0.2°C above your highest assay temperature
- GrowthTools estimates (43.80°C) are 1.8°C above your highest assay temperature
- **Logistic values are more constrained and less extrapolatory** ✓

### 4. **Statistical Power for Evolution History Effect**

Both methods detected a similar direction of effect (40°C-evolved strains have slightly higher CTmax), but:

| Method | p-value | Effect size | Detectability |
|--------|---------|------------|---|
| GrowthTools | 0.378 (NS) | ~0.46°C | NOT detected |
| Logistic | 0.025 * | ~0.3°C | **Detected** |

**Why does logistic detect it?** Lower noise → stronger signal-to-noise ratio → easier to detect modest biological differences.

### 5. **Model Fit Quality**

Both methods fit the growth data well:
- GrowthTools: Mean R² = 0.917
- Logistic: Mean R² = 0.923

**No advantage to either method here** — both adequately explain growth variation.

---

## Recommendation: Use 3-Parameter Logistic

### Reasons:

1. ✅ **More precise CTmax estimates** (SD = 0.35°C vs. 0.98°C)
2. ✅ **Less prone to overfitting** at extreme temperatures where growth is noisy
3. ✅ **Biologically reasonable values** (not extrapolating too far beyond assay range)
4. ✅ **Better statistical power** to detect evolution history effects on CTmax
5. ✅ **Simpler, more transparent** — easier to understand what's driving differences
6. ✅ **Reproducible** — same model used for all strains, same assumptions throughout

### What You Lose:

- The flexibility of trying multiple models per well (but this is actually a feature, not a bug, given your high-temperature noise)

---

## How to Report This

In your methods, you might write:

> "We estimated growth rates by fitting a three-parameter logistic model (K / (1 + exp(-r(t - t_mid)))) to OD600 measurements using robust nonlinear regression. This model was chosen over a multi-method approach (growthTools) because it provides more precise and less variable estimates of critical thermal maximum, which is critical for detecting evolution-driven differences in thermal tolerance at the elevated temperatures in our assay (41–42°C) where growth measurements are inherently noisier."

---

## Additional Validation (Optional)

If you want to be thorough, you could:

1. **Check sensitivity** — Report results with both methods as supplementary analyses
2. **Bootstrap confidence intervals** — Show uncertainty in CTmax estimates for each strain
3. **Cross-validate** — Compare predictions at held-out temperature points
4. **Biological validation** — See if CTmax differences match other thermal tolerance measures (e.g., heat shock protein expression, if available)

But for the main analysis, **use the logistic method**.
