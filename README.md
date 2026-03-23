# DHS mCPR Analysis

This repository is the streamlined project repo for the DHS modern contraceptive prevalence rate (mCPR) analysis. It keeps the main data pull notebook, the three core model reports, the saved outputs used for decision-making, and the Shiny dashboard used to explore the results interactively.

The project is centered on four linked tasks:

- estimating country-level and region-level mCPR trajectories from DHS data
- understanding Sub-Saharan Africa's path toward a 40% mCPR goal
- comparing required annualized growth rates to observed and model-implied growth
- translating the results into clear investment, stage, and heterogeneity views for countries and regions

## Main modeling tracks

The repository keeps three complementary analysis reports in [`analysis/`](analysis/):

- [`dhs_analysis_production.Rmd`](analysis/dhs_analysis_production.Rmd)
  - main report and main decision-facing workflow
  - hierarchical logistic model with country-level `K`, bounded country-level `t0`, and region-fixed `s`
  - includes diagnostics, validation, SSA 40% goal analysis, outlier analysis, S-curve stages, and export tables
- [`dhs_analysis_region_fixedK_countryS.Rmd`](analysis/dhs_analysis_region_fixedK_countryS.Rmd)
  - alternative logistic specification
  - flips the production identification strategy by fixing `K` at the region level and letting `s` vary by country
  - used to assess sensitivity to the `K` versus `s` tradeoff
- [`bayesian_gamm_mcpr_report_v4.Rmd`](analysis/bayesian_gamm_mcpr_report_v4.Rmd)
  - flexible Bayesian hierarchical GAMM alternative
  - focuses on curve shape, growth, acceleration, and deceleration rather than a fitted asymptote parameter

## Data pipeline

The DHS extraction workflow lives in [`data_pipeline/get_dhs.Rmd`](data_pipeline/get_dhs.Rmd).

At a high level, that notebook:

- connects to the DHS API through `rdhs`
- identifies DHS `IR` datasets from 1985 onward
- computes design-based all-women mCPR using:
  - `v313` for modern-method use
  - `v005` for person weights
  - `v021` for clusters
  - `v022` or `v023` for strata when available
- aggregates survey results to a country-year table with `mCPR`, `se_mCPR`, and `n_women`

Important: the repository does not include DHS microdata. Those data remain behind the DHS API and local `rdhs` cache. The repo keeps the code and the derived project inputs needed to reproduce the analysis workflow on a machine with DHS access.

## SSA decision workflow

The main reporting and app assets focus especially on Sub-Saharan Africa. The core decision questions are:

- What is the current weighted SSA mCPR level under observed and model-based baselines?
- How large is the gap to 40% for the region and for each country?
- What annualized growth rate would be needed to hit 40% by 2035?
- Are those required growth rates realistic relative to observed survey growth and model-implied growth?
- Which countries are structurally below the target, approaching it, or already above it?
- Which countries are outliers relative to the weighted SSA regional trajectory?
- Where are countries and the weighted SSA region on the contraceptive S-curve?

## Repository structure

```text
.
|-- analysis/
|   |-- dhs_analysis_production.Rmd
|   |-- dhs_analysis_region_fixedK_countryS.Rmd
|   `-- bayesian_gamm_mcpr_report_v4.Rmd
|-- data_pipeline/
|   `-- get_dhs.Rmd
|-- shiny_dashboard/
|   |-- app.R
|   `-- www/styles.css
|-- dashboard_assets/
|   |-- inputs/
|   |-- models/production/
|   `-- outputs/
|-- scripts/
|   |-- build_country_pop_weights.R
|   `-- sync_dashboard_assets.R
`-- ssa_validation_audit.csv
```

## Key folders

- [`analysis/`](analysis/): the three main R Markdown analyses
- [`data_pipeline/`](data_pipeline/): DHS pull and derived-input construction
- [`dashboard_assets/`](dashboard_assets/): saved app inputs, fit object, and exported summary tables
- [`shiny_dashboard/`](shiny_dashboard/): the deployable Shiny application
- [`scripts/`](scripts/): lightweight helper scripts used to refresh app-facing assets

## Run the app locally

From the repository root in R:

```r
shiny::runApp("shiny_dashboard")
```

If you are using the project-local package library from the original workspace:

```r
.libPaths(c("C:/Users/Owner/Documents/dhs/r_libs", .libPaths()))
shiny::runApp("shiny_dashboard")
```

## Deploy the app

The app uses saved results only and does not fit the model live. Deploy from the repository root with `shinyapps.io`:

```r
rsconnect::deployApp(
  appDir = "C:/path/to/dhs-mcpr",
  appFiles = c("shiny_dashboard", "dashboard_assets", "ssa_validation_audit.csv"),
  appPrimaryDoc = "shiny_dashboard/app.R",
  appName = "dhs-mcpr-dashboard"
)
```

## Downloadable handoff bundle

If you just want a portable copy for another Windows laptop, use [`dhs_app_handoff/`](dhs_app_handoff/).

That folder includes:

- a minimal saved-asset copy of the app
- one-click package install scripts
- one-click local launch scripts

It is meant for local handoff and download, not for GitHub Pages hosting.

## What is intentionally not included

This repository is organized around the main analytical tasks and app workflow, but it is not the entire working directory. It does not include:

- DHS microdata exports
- large raw population workbooks
- report caches and rendered HTML artifacts
- exploratory scratch files and one-off outputs that are not part of the main workflow

## Additional notes

- [`analysis/README.md`](analysis/README.md) summarizes the three analysis tracks.
- [`data_pipeline/README.md`](data_pipeline/README.md) explains the DHS pull workflow.
- [`shiny_dashboard/README.md`](shiny_dashboard/README.md) documents the interactive app.
- [`dashboard_assets/README.md`](dashboard_assets/README.md) describes the saved runtime bundle used by the app.
