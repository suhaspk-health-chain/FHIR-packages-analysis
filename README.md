# FHIR Packages Analysis

**Exploratory Data Analysis of the global FHIR Implementation Guide ecosystem**

> An EDA project by **Suhas P K**

[![Live Dashboard](https://img.shields.io/badge/Shiny-Live%20Dashboard-f26d21?style=flat-square&logo=r)](https://suhas-pk.shinyapps.io/xig-FHIR-resources-eda/)
[![RPubs Report](https://img.shields.io/badge/RPubs-EDA%20Report-0c223f?style=flat-square&logo=r)](https://rpubs.com/suhasPK/xig-FHIR-resources-eda)
[![GitHub](https://img.shields.io/badge/GitHub-Source%20Code-333?style=flat-square&logo=github)](https://github.com/suhaspk-health-chain/FHIR-packages-analysis)

---

## Overview

This project scrapes, cleans, and analyses every publicly registered FHIR package from the [HL7 XIG Registry](https://packages2.fhir.org/xig) — over **75,000 resource definitions** across **1,000+ published Implementation Guides** spanning six FHIR versions (R2 through R6) and 30+ countries.

The primary output is an **interactive R Shiny dashboard** with nine tabs covering version evolution, global landscape, text/NLP analysis, statistical hypothesis testing, and full browsable data tables.

A companion **[EDA report on RPubs](https://rpubs.com/suhasPK/xig-FHIR-resources-eda)** tells the full story in Veritasium style — from a hook to data insights — for a technical and non-technical audience alike.

---

## Key Findings

| Finding | Detail |
|---------|--------|
| FHIR R4 dominates | 83% of all published packages target R4 |
| Two types rule | ValueSet + StructureDefinition = 76% of all resources |
| US leads globally | More IGs than any other country, driven by CMS/ONC mandates |
| 62% growth | Resource types grew from 103 (R2/DSTU2) to 167 (R5) |
| Conformance first | The registry is predominantly rules and terminology, not clinical data |

---

## Repository Structure

```
FHIR-packages-analysis/
│
├── fhir_pkg_data_collection.R   # Web scraper — collects data from packages2.fhir.org/xig
│
├── scripts/                     # Analysis pipeline (run in order)
│   ├── 00_setup_packages.R      # Install/load all R dependencies
│   ├── 01_config.R              # Paths and constants
│   ├── 02_load_and_clean.R      # Parse JSON → clean parquet/CSV
│   ├── 03_derive_resources.R    # Derive resource-level features
│   ├── 04_basic_summaries.R     # Frequency tables and counts
│   ├── 05_theme_healthchain.R   # ggplot theme and colour palette
│   ├── 06_plots_quick.R         # Quick EDA plots
│   ├── 07_compare_resources_across_versions.R  # Version diff analysis
│   ├── 08_plot_version_resource_comparison.R   # Version comparison charts
│   ├── 09_summary_tables.R      # Export summary CSVs
│   ├── 10_build_shiny_data.R    # Prepare data files for Shiny app
│   ├── 11_US_realm_resources.R  # US-specific analysis
│   └── 12_VERIFY_IG_RESOURCE_ALIGNMENT.R       # Data quality checks
│
├── data/
│   ├── raw/                     # Raw scraped files (gitignored — too large)
│   │   ├── xig_resources.json
│   │   └── xig_resources.ndjson
│   ├── interim/                 # Cleaned intermediate data
│   │   ├── fhir_packages_clean.parquet
│   │   └── fhir_packages_clean.csv
│   └── processed/               # Analysis-ready outputs
│       ├── xig_resources.json           # Used by Shiny app
│       ├── fhir_resources.parquet/csv
│       ├── resource_presence_matrix.csv
│       ├── packages_by_version.csv
│       ├── resources_added_removed_by_transition.csv
│       └── ...
│
├── shiny_app/                   # Interactive dashboard
│   ├── app.R                    # Entry point
│   ├── global.R                 # Data loading and global state
│   ├── 05_theme_healthchain.R   # ggplot theme (used by app)
│   ├── assets/                  # Static assets
│   ├── data/processed/          # Copy of processed data for the app
│   └── R/                       # Shiny modules
│       ├── utils.R              # Shared helpers, colour scales, caption utility
│       ├── mod_overview.R       # Overview KPIs + interactive plot builder
│       ├── mod_evolution.R      # Version transition charts
│       ├── mod_verification.R   # Global landscape, hierarchy, catalog, US deep dive, about
│       ├── mod_data_tables.R    # Browsable raw data tables
│       ├── mod_nlp.R            # Text analysis & pattern discovery
│       └── mod_hypothesis.R     # Statistical hypothesis testing lab
│
├── figs/                        # Saved plot images (gitignored)
│
├── outputs/
│   ├── tables/                  # CSV/XLSX summary tables
│   ├── quarto site/             # Quarto website (exploratory)
│   └── fhir_ecosystem_blog.md   # Companion article / write-up
│
├── scripts_v1.1/                # Archived v1 scripts (reference only)
│
└── .gitignore
```

---

## Running the Shiny Dashboard

### Prerequisites

R 4.2+ with the following packages:

```r
install.packages(c(
  "shiny", "dplyr", "tidyr", "stringr", "ggplot2",
  "DT", "scales", "jsonlite", "readr", "bslib",
  "cowplot", "magick", "arrow", "lubridate", "forcats"
))
```

### Launch

```r
# From the project root
shiny::runApp("shiny_app")
```

The app reads data from `shiny_app/data/processed/`. If that directory is empty, run the data pipeline first (see below).

---

## Data Pipeline

To reproduce the dataset from scratch:

```r
# 1. Collect raw data (~75k rows, takes ~20 min, be polite to the server)
source("fhir_pkg_data_collection.R")

# 2. Run analysis scripts in order
source("scripts/00_setup_packages.R")
source("scripts/01_config.R")
source("scripts/02_load_and_clean.R")
source("scripts/03_derive_resources.R")
source("scripts/04_basic_summaries.R")
source("scripts/07_compare_resources_across_versions.R")
source("scripts/08_plot_version_resource_comparison.R")
source("scripts/09_summary_tables.R")
source("scripts/10_build_shiny_data.R")
```

Raw JSON files (`data/raw/`) are gitignored due to size. Processed CSVs in `data/processed/` and `shiny_app/data/processed/` are also gitignored — see `.gitignore`.

---

## Dashboard Tabs

| Tab | What it shows |
|-----|--------------|
| **Overview** | KPI cards + interactive plot builder (bar, grouped, stacked; facet support; status deduplication) |
| **Evolution** | Resources added/removed per FHIR version transition (R2 -> R3 -> R4 -> R4B -> R5 -> R6) |
| **Global Landscape** | Which countries publish the most IGs; resource counts by realm |
| **Data Hierarchy** | How 75k resources break down by version and type |
| **Resource Catalog** | Every resource type ranked by count with percentages |
| **US Deep Dive** | United States FHIR version adoption and clinical category breakdown |
| **Text Analysis** | NLP on resource titles: word frequency, publisher patterns, lifecycle, working groups |
| **Hypothesis Lab** | Six pre-built statistical tests (Wilcoxon, Chi-square, Proportion) with plain-English verdicts |
| **Data Tables** | Full browsable dataset, presence matrix, stable resources |
| **Verification** | Data provenance, methodology, and source links |
| **About** | Project background and FHIR primer |

---

## Data Sources

| Source | URL |
|--------|-----|
| HL7 FHIR XIG Registry | https://packages2.fhir.org/xig |
| HL7 IG Registry (GitHub) | https://github.com/FHIR/ig-registry |

Data scraped: early 2026. Registry is continuously updated; re-scraping will yield a more current snapshot.

---

## Download Captions

Every plot downloaded from the dashboard includes a dynamic caption line encoding exactly what was configured:
- X-axis variable, plot type, grouping, facet variable
- Any active realm or version filters
- Data source attribution and date

---

## Tech Stack

- **R** — data wrangling, statistics, visualisation
- **R Shiny** — interactive dashboard
- **ggplot2** — all charts
- **DT** — interactive data tables
- **Apache Arrow / Parquet** — efficient data storage
- **bslib** — Bootstrap 5 theming
- **jsonlite / readr** — data I/O

---

## Live Links

| | Link |
|---|---|
| Interactive Dashboard | https://suhas-pk.shinyapps.io/xig-FHIR-resources-eda/ |
| EDA Report (RPubs) | https://rpubs.com/suhasPK/xig-FHIR-resources-eda |
| Source Code (GitHub) | https://github.com/suhaspk-health-chain/FHIR-packages-analysis |

---

## Author

**Suhas P K**
Personal EDA project | March 2026
