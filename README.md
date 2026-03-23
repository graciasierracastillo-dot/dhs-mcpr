# DHS mCPR Dashboard

This repository contains the deployable Shiny dashboard for the DHS modern contraceptive prevalence rate (mCPR) project.

It includes:

- the Shiny app in `shiny_dashboard/`
- the minimum saved model outputs and tables needed to run the app in `dashboard_assets/`
- a simple SSA validation audit table in `ssa_validation_audit.csv`
- the DHS data pull notebook in `get_dhs.Rmd`
- the women ages 15-49 population-weight build script in `scripts/build_country_pop_weights.R`

This repository does **not** include the full analysis workspace, large raw population workbooks, or all intermediate report artifacts.

## Run locally

From the repository root in R:

```r
shiny::runApp("shiny_dashboard")
```

## Deploy

Deploy from the repository root and point `appPrimaryDoc` to `shiny_dashboard/app.R`.

```r
rsconnect::deployApp(
  appDir = "C:/path/to/repo",
  appPrimaryDoc = "shiny_dashboard/app.R"
)
```
