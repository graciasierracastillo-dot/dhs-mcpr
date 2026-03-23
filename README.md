# DHS mCPR Dashboard

This repository contains the deployable Shiny dashboard for the DHS modern contraceptive prevalence rate (mCPR) project.

The app is designed for interactive exploration of:

- observed DHS mCPR trajectories by country
- fitted Bayesian hierarchical logistic trajectories and uncertainty
- Sub-Saharan Africa (SSA) goal tracking relative to a 40% mCPR benchmark
- annualized growth requirements, modeled growth, and S-curve stage positioning

This is an **app-focused repository**, not the full analysis workspace. It keeps only the files needed to run and deploy the dashboard cleanly.

## Repository structure

```text
.
|-- shiny_dashboard/
|   |-- app.R
|   `-- www/styles.css
|-- dashboard_assets/
|   |-- inputs/
|   |-- models/production/
|   `-- outputs/
|-- scripts/
|   `-- build_country_pop_weights.R
|-- get_dhs.Rmd
`-- ssa_validation_audit.csv
```

## What is included

- `shiny_dashboard/`: the deployable Shiny application
- `dashboard_assets/inputs/`: the minimum country-year inputs needed by the app
- `dashboard_assets/models/production/`: the saved production fit used by the app
- `dashboard_assets/outputs/`: the saved tables that power the dashboard views
- `ssa_validation_audit.csv`: a simple SSA validation audit file used by the app
- `get_dhs.Rmd`: the DHS pull notebook used in the broader workflow
- `scripts/build_country_pop_weights.R`: script for building women ages 15-49 population weights from WPP source data

## What is intentionally not included

This repository does **not** include:

- the full analysis workspace
- large raw population workbooks
- caches and rendered report artifacts
- alternative model branches and intermediate scratch outputs

## Run locally

From the repository root in R:

```r
shiny::runApp("shiny_dashboard")
```

If you are using the project-local package library from the original workspace:

```r
.libPaths(c("C:/Users/Owner/Documents/dhs/r_libs", .libPaths()))
shiny::runApp("shiny_dashboard")
```

## Deploy to shinyapps.io

Deploy from the repository root and point `appPrimaryDoc` to `shiny_dashboard/app.R`.

```r
rsconnect::deployApp(
  appDir = "C:/path/to/dhs-mcpr",
  appPrimaryDoc = "shiny_dashboard/app.R",
  appName = "dhs-mcpr-dashboard"
)
```

## Notes

- The app uses saved results only. It does **not** refit the model live.
- Weighted regional views use women ages 15-49 population weights.
- The main production fit used by the app is stored in `dashboard_assets/models/production/`.
