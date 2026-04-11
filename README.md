# cross-tolerance

An experiment investigating cross-tolerance in *Nakaseomyces glabrata* (*C. glabrata*) — examining whether strains evolved at different temperatures or under antifungal drug exposure show altered growth across a range of temperatures and drug concentrations.

---

## Repository structure

```
cross-tolerance-experiment/
├── data-raw/               # Raw plate reader outputs and plate layouts
│   ├── Growth-Curves/      # Excel files from BioTek plate reader (blocks 1–3)
│   ├── MICs/               # Raw MIC data
│   └── well-plate-layout.xlsx
├── data-processed/         # Cleaned and summarised data, produced by R scripts
├── figures/                # Output plots
│   └── diagnostic/         # Per-well diagnostic plots from sliding window analysis
├── R-Scripts-Joey/         # Main analysis scripts (see below)
├── R-Script-Drafts/        # Draft/exploratory scripts
│   ├── TPC-Analysis/
│   └── MIC-Analysis/
└── Processed_Outputs/      # Processed outputs from draft analyses
```

---

## Scripts

Scripts in `R-Scripts-Joey/` are numbered roughly in the order they should be run.

| Script | Description |
|--------|-------------|
| `01-OD-data-import.R` | Imports raw OD data from BioTek Excel outputs and combines across blocks and temperatures |
| `02b-growth-rates-sliding-window.R` | Estimates maximum growth rate for each time series using a sliding window linear model on ln(OD) vs time; compares window sizes of 4, 5, and 6 time points |
| `02-growth-rates-tpcs.R` / `02-growth-rates-tpcs-no-lag.R` | Growth rate and TPC fitting variants |
| `03-tpc-fits-thomas.R` / `03-tpc-fits-thomas-no-lag.R` | Fits Thomas et al. thermal performance curve models |
| `03b-compare-TPCs.R` | Compares TPC model fits |
| `09-growth-rates.R` / `09-growth-rates-no-lag.R` | Earlier growth rate fitting scripts using the `growthTools` package |
| `10-growth-rates-logistic-nlsLM.R` | Logistic growth model fitting using `nlsLM` |
| `10-spot-plates.R` | Spot plate analysis |
| `11-MICs.R` / `11b-MICs.R` | Minimum inhibitory concentration analysis |

---

## Dependencies

All scripts are written in R. The following packages are required:

- **Data wrangling:** `tidyverse`, `janitor`, `lubridate`, `readxl`, `glue`
- **Plotting:** `ggplot2` (via `tidyverse`), `cowplot`
- **Growth rate fitting:** `growthTools`, `growthrates`, `nls.multstart`, `nlsLM` (via `minpack.lm`)
- **Statistics:** `plotrix`

Install missing packages with `install.packages()`. `growthTools` may need to be installed from GitHub:

```r
# install.packages("remotes")
remotes::install_github("ctkremer/growthTools")
```

---

## Data

Growth curve data were collected using a BioTek plate reader measuring OD at 600 nm over time. Experiments were run in three blocks across five test temperatures (25, 35, 38, 41, 42°C). Strains include a wild-type ancestor (*f*RS585), temperature-evolved lines (35°C and 40°C), and drug-evolved lines (fluconazole and caspofungin).

> **Note:** Raw plate reader Excel files are not tracked in version control due to file size. Analysis scripts read from `data-processed/all-blocks-tpc-experiment.csv`, which is the combined and cleaned version of the raw data.

---

## Status

Scripts are under active development. Analysis pipeline is not yet finalised.