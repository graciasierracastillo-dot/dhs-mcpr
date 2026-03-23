# Scripts

This folder contains lightweight utility scripts retained in the project repository.

## Included scripts

- `build_country_pop_weights.R`
  - builds women ages 15-49 population weights from the WPP single-age female workbook
  - writes the app-ready weights file into `dashboard_assets/inputs/`
  - this is part of the broader update workflow, but it is not required just to run the app
- `sync_dashboard_assets.R`
  - copies the core saved inputs, model objects, and exported summary tables into `dashboard_assets/`
  - use this after refreshing analysis outputs so the app bundle stays aligned with the latest reports
  - if a file is already present in `dashboard_assets/`, the script will use that copy when no newer root-level source file is available
