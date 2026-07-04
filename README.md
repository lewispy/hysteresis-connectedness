# Hysteresis between Bitcoin, Gold, Oil, and the S&P 500 index: Evidence from a Multi-threshold Connectedness Approach

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21193874.svg)](https://doi.org/10.5281/zenodo.21193874)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Made with R](https://img.shields.io/badge/Made%20with-R-1f425f.svg)](https://www.r-project.org/)

Replication code and materials for:

> Onodje, P. (2026). *Hysteresis between Bitcoin, Gold, Oil, and the S&P 500 index: Evidence from a multi-threshold connectedness approach.* **Finance Research Open**, 2, 100091.

**Author:** Patrick Onodje — Department of Economics, University of Ibadan, Ibadan, Nigeria

---

## Abstract

This study analyses the presence of hysteresis in the volatility spillovers between Bitcoin, Gold, Oil, and the S&P 500 index, applying a novel multi-threshold connectedness framework. Daily returns from January 2015 to May 2024 are decomposed into three regimes, large positive changes (*increasing*), moderate changes (*stable*), and large negative changes (*decreasing*), which allows regime-dependent dynamics in intermarket connectedness to be captured. The final sample comprises 2,360 common daily observations after aligning trading days across markets.

The findings reveal robust evidence of hysteresis: connectedness intensifies during turbulent periods and does not symmetrically revert during tranquil periods. The S&P 500 index consistently emerges as a dominant shock transmitter across all regimes, confirming its systemic role in global financial contagion. Gold exhibits a regime-dependent reversal in net positioning, challenging the common belief that it is always a safe haven. Bitcoin is equally regime-sensitive, with connectedness that strengthens during volatile episodes, reflecting its increasing integration into mainstream finance. Oil turns out to be a predominant shock receiver, especially during turbulent periods. Statistical tests confirm that the differences in connectedness across regimes are significant, upholding the hysteresis hypothesis. Relative to traditional quantile-based connectedness approaches, the multi-threshold approach offers better interpretability and computational efficiency in capturing asymmetric spillover connectedness.

**Keywords:** Financial contagion · Hysteresis · Multi-threshold connectedness · Volatility spillovers · TVP-VAR

**JEL codes:** G15, C32, G11, E44, C58

---

## Repository contents

```
hysteresis-connectedness/
├── code/
│   └── Hysteresis_Connectedness.R   # Full replication code (tables + figures)
├── paper/
│   └── Onodje_2026.pdf              # Published paper
├── poster/
│   └── Onodje_2026_Poster.pdf       # Conference poster
├── slides/
│   └── Onodje_2026_Presentation.pptx # Presentation slides
├── CITATION.cff                     # Machine-readable citation
├── LICENSE                          # MIT License
└── README.md
```

---

## What the code does

`code/Hysteresis_Connectedness.R` reproduces the tables and figures in the paper using a
TVP-VAR dynamic connectedness approach applied separately to the three regimes. The script:

- reads a regimes workbook (an `.xlsx` file with three sheets: `increasing`, `stable`, `decreasing`);
- runs the TVP-VAR connectedness estimation for each regime via the
  [`ConnectednessApproach`](https://cran.r-project.org/package=ConnectednessApproach) package;
- re-attaches the correct dates to the estimation output;
- exports **Table 2** (regime-by-regime connectedness matrices) and **Table 3**
  (regime means, paired *t*-tests, and hysteresis significance flags) to Excel; and
- produces **Figures 3–8** (FROM, TO, NET, pairwise PCI, net pairwise NPDC, and TCI) as PNGs.

A robustness loop at the end re-runs the full pipeline for alternative threshold quantiles
(30/70, 25/75, 20/80).

## Requirements

- R (≥ 4.1 recommended)
- R packages (installed automatically by the script if missing):
  `ConnectednessApproach`, `zoo`, `readxl`, `openxlsx`, `ggplot2`, `reshape2`

## Input data format

The script expects one Excel workbook per threshold specification, each containing three sheets
named `increasing`, `stable`, and `decreasing`. In every sheet:

- the **first column** is the date (Date, POSIXct, Excel serial number, or a parseable date string);
- the **remaining columns** are the series in the order `bitcoin`, `gold`, `oil`, `sp500`.

The main robustness loop looks for `regimes_30_70.xlsx`, `regimes_25_75.xlsx`, and
`regimes_20_80.xlsx` in the working directory.

> **Note on data:** the underlying market price series are sourced from third-party data
> providers and are not redistributed here. Users should supply their own regime workbooks in
> the format above. Please open an issue if you need help preparing the input files.

## How to run

```r
# From an R session with the working directory set to the repo root:
source("code/Hysteresis_Connectedness.R")
```

Sourcing the script runs the robustness loop over the three threshold specifications and writes
all outputs to `robustness_outputs_files/<tag>/`. To run a single specification manually:

```r
res <- run_export_and_plot(
  regimes_xlsx = "regimes_25_75.xlsx",
  out_dir      = "outputs",
  window.size  = 200,
  nfore        = 12
)
```

## Key parameters

| Parameter     | Default        | Meaning                                   |
|---------------|----------------|-------------------------------------------|
| `nlag`        | 1              | VAR lag order                             |
| `nfore`       | 12             | Forecast horizon for the decomposition    |
| `window.size` | 200            | Rolling window size                       |
| `kappa1`      | 0.99           | TVP-VAR forgetting factor                 |
| `kappa2`      | 0.96           | TVP-VAR decay factor                      |
| `prior`       | `"BayesPrior"` | Prior for the TVP-VAR                      |

---

## Citation

If you use this code or build on this work, please cite the **paper**:

```bibtex
@article{Onodje2026Hysteresis,
  author  = {Onodje, Patrick},
  title   = {Hysteresis between Bitcoin, Gold, Oil, and the S\&P 500 index:
             Evidence from a multi-threshold connectedness approach},
  journal = {Finance Research Open},
  volume  = {2},
  pages   = {100091},
  year    = {2026}
}
```

To cite this **archived code and materials** directly:

> Onodje, P. (2026). *Hysteresis between Bitcoin, Gold, Oil, and the S&P 500 index:
> Evidence from a multi-threshold connectedness approach* (Version 1.0.0) [Software].
> Zenodo. https://doi.org/10.5281/zenodo.21193874

## Archiving & DOI

This repository is archived on [Zenodo](https://zenodo.org) with a citable DOI:
[10.5281/zenodo.21193874](https://doi.org/10.5281/zenodo.21193874). Each new GitHub
release is automatically archived and versioned there.

## License

The code in this repository is released under the [MIT License](LICENSE).

The paper, poster, and presentation are the author's academic work; please cite the published
article when referencing them.

## Contact

Questions, replication issues, or collaboration inquiries are welcome — please open an issue or
reach out to the author.
