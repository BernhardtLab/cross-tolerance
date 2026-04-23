# Growth Rate Analysis Scripts - Index

**Last Updated**: April 22, 2026  
**Key Update**: Corrected growthtools visualization (see CORRECTION-LOG.md)

## Quick Navigation

### For Analysis
- **Temperature-Performance Curves**: See script `10-growth-rates-logistic-tpc.R`
- **Competitive Fitness**: See script `02-growth-rates-growth-tools.R`
- **Method Comparison**: See script `12-comparison-logistic-vs-growthtools-CORRECTED.R`

### For Understanding
- **Which method to use?** → `README-GROWTH-RATE-METHODS.md`
- **What changed in April 2026?** → `CORRECTION-LOG.md`
- **Overview of all scripts** → `SCRIPT-SUMMARY.txt` (this file)

---

## Scripts at a Glance

| Script | Method | Use Case | Output |
|--------|--------|----------|--------|
| `02-growth-rates-growth-tools.R` | Growthtools (exponential) | Competitive fitness, early growth | growth-rates-growth-tools.csv |
| `09-growth-rates-no-lag.R` | Growthtools (no lag correction) | Alternative exponential estimation | all-blocks-growth-no-lag.csv |
| `10-growth-rates-logistic-tpc.R` | Logistic 3-parameter | TPCs, full trajectory analysis | growth-rates-logistic.csv |
| `03-growth-rates-logistic.R` | Logistic (earlier version) | (Check relevance for your work) | growth-rates-logistic.csv |
| `12-comparison-logistic-vs-growthtools-CORRECTED.R` | Both methods compared | Visualization, validation | comparison_logistic_vs_growthtools_42C_CORRECTED.{pdf,png} |

---

## Documentation Files

| File | Purpose |
|------|---------|
| `README-GROWTH-RATE-METHODS.md` | Complete method guide with caveats and examples |
| `CORRECTION-LOG.md` | Technical details of April 2026 correction |
| `SCRIPT-SUMMARY.txt` | Overview, data flow, quick reference |
| `INDEX.md` | This file |

---

## The April 2026 Correction (Important!)

**Problem**: Previous plots extended growthtools predictions across the saturation phase where they're invalid.

**Solution**: Script `12-comparison-logistic-vs-growthtools-CORRECTED.R` now:
- Detects exponential phase boundary automatically
- Limits growthtools line to exponential phase only
- Shows both methods' validity domains clearly

**Why it matters**: Growthtools estimates exponential growth rate (μ), which is only meaningful during exponential growth. Plotting across saturation misrepresents the method.

---

## How to Use This Directory

### First Time?
1. Read `README-GROWTH-RATE-METHODS.md` to understand the approaches
2. Decide which method(s) you need
3. Run the corresponding script(s)
4. Use script `12` to validate your results

### Reproducing Results?
1. Run scripts in numerical order: 02→10→12 (or 02→09→12)
2. Compare against saved outputs in `data-processed/`
3. Check figures in `figures/`
4. Read CORRECTION-LOG.md if results differ from old versions

### Analyzing New Data?
1. Ensure data follows the same format as previous inputs
2. Adjust file paths as needed in the scripts
3. Run scripts with new data
4. Inspect results for quality (R², fit diagnostics)

---

## Understanding the Methods

### Exponential Growth (Growthtools)
- **Model**: ln(OD) = ln(OD₀) + μ·t
- **Phase**: Only valid before saturation
- **Output**: μ (growth rate)
- **Best for**: Comparing early growth strategy

### Logistic Growth
- **Model**: OD(t) = K / (1 + exp(-μ·(t-t₀)))
- **Phase**: Valid full trajectory
- **Output**: μ (logistic rate), K (carrying capacity), t₀ (midpoint)
- **Best for**: Complete growth characterization, TPCs

---

## Key Recommendations

✓ **DO**:
- Plot raw data alongside fitted models
- Check R² and residuals for fit quality
- Use logistic for saturation-phase analysis
- Use growthtools for early growth comparison
- Report both data and methods clearly

✗ **DON'T**:
- Plot exponential lines across saturation phase
- Ignore lag phase without justification
- Report μ without specifying which phase
- Assume one method works for all questions
- Skip visualization of fits

---

## Output Locations

```
data-processed/
├── growth-rates-growth-tools.csv
├── all-blocks-growth-no-lag.csv
└── growth-rates-logistic.csv

figures/
├── growth-tools-figures/           (individual well fits)
├── block1/, block2/, block3/       (organized by block)
└── comparison_logistic_vs_growthtools_42C_CORRECTED.{pdf,png}
```

---

## Questions?

- **Method selection**: See `README-GROWTH-RATE-METHODS.md`
- **Technical details**: See `CORRECTION-LOG.md`
- **Script functionality**: See comments in individual scripts
- **Data structure**: See header comments in each script

---

**Author**: Joanna Bernhardt  
**Corrections/Updates**: April 22, 2026  
**Repository**: cross-tolerance-experiment
