# DHS App Handoff

This folder is the minimal local handoff bundle for the DHS mCPR Shiny dashboard.

## What is included

- `shiny_dashboard/`
  - the app code and styling
- `dashboard_assets/`
  - the saved model object and the app-facing input/output tables
- `ssa_validation_audit.csv`
  - the SSA validation audit file used in the app
- `r_libs/`
  - a local package library for this handoff copy
- `1_Install_DHS_Dashboard_Packages.bat`
  - installs the R packages the app needs into `r_libs/`
- `2_Open_DHS_Dashboard.bat`
  - launches the app locally

## Recommended setup on another laptop

1. Install R for Windows.
2. Copy this entire folder to the new laptop.
3. Double-click `1_Install_DHS_Dashboard_Packages.bat`.
4. After packages finish installing, double-click `2_Open_DHS_Dashboard.bat`.

## Notes

- The app uses saved results only. It does not fit the model live.
- The first package install can take a while because the app depends on `brms` and related packages.
- If the launch script says it cannot find R, install R first and then try again.
