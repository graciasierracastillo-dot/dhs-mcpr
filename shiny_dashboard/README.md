# Shiny Dashboard

This folder contains the deployable Shiny application for the DHS mCPR analysis project.

## Main files

- `app.R`: application code
- `www/styles.css`: dashboard styling

## App sections

The app is organized into four main sections:

1. `Trajectory Explorer`
   - observed DHS mCPR histories by country and region
   - fitted country trajectories with `K`, inflection year, and 95% credible intervals
   - observed versus model-based annualized growth rates
   - quick country validation checks
2. `SSA Goal Story`
   - population-weighted SSA current level, gap to 40%, and required annualized growth
   - required-versus-current annualized growth comparison
   - survey-to-survey growth tables and model growth history
   - all-country SSA trajectory views relative to 40%
   - weighted SSA S-curve stage view and country S-curve views
3. `Weighted Region Stage Views`
   - weighted S-curve view for any analytic region
   - global weighted curve option
4. `Methods And Validation`
   - short modeling explanation
   - region-filtered validation table for same-year fit checks

## Data dependencies

The app looks first for files at the repository root and then falls back to the bundled copies inside `../dashboard_assets/`.

The main files used at runtime are:

- `dashboard_assets/models/production/fit_logistic_final_model_regionFixedSpeed_countryK_boundedT0_dev.rds`
- `dashboard_assets/inputs/mcpr_data.csv`
- `dashboard_assets/inputs/country_pop_weights_15_49.csv`
- `dashboard_assets/outputs/core/mcpr_investment_table_final_model.csv`
- `dashboard_assets/outputs/core/mcpr_latest_observed_vs_projection.csv`
- `dashboard_assets/outputs/core/mcpr_country_growth_rates_over_time.csv`
- `dashboard_assets/outputs/core/mcpr_s_curve_stage_classification.csv`
- `dashboard_assets/outputs/core/mcpr_s_curve_stage_classification_2026.csv`
- `dashboard_assets/outputs/core/mcpr_time_to_99pct_k.csv`
- `dashboard_assets/outputs/ssa/mcpr_ssa_country_goals_summary.csv`
- `dashboard_assets/outputs/ssa/mcpr_ssa_region_goals_summary.csv`
- `dashboard_assets/outputs/ssa/mcpr_ssa_observed_interval_growth.csv`
- `ssa_validation_audit.csv`

## Run locally

From the repository root:

```r
shiny::runApp("shiny_dashboard")
```

This app is intended to sit beside the main analysis reports rather than replace them. The R Markdown analyses produce the saved outputs, and the app provides an interactive layer on top of those exported results.
