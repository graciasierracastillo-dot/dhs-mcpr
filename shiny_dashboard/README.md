# mCPR Shiny Dashboard

This app is the interactive front end for the current production mCPR workflow.

It is built around the same production fit and SSA goal outputs used in
`dhs_analysis_production.Rmd`, and it is organized as a story:

1. `Trajectory Explorer`
   - observed DHS mCPR histories by country and region
   - fitted country trajectories with `K`, inflection year, and 95% credible intervals
   - observed versus model-based annualized growth rates
   - quick country validation checks
2. `SSA Goal Story`
   - population-weighted SSA current level, gap to 40%, and required annualized growth
   - interactive version of the required-versus-current annualized growth scatter
   - survey-to-survey growth tables and model growth history
   - all-country SSA trajectory views relative to 40%
   - weighted SSA S-curve stage view and country S-curve panels
3. `Weighted Region Stage Views`
   - the same weighted S-curve view for any analytic region
   - a global weighted curve option
4. `Methods And Validation`
   - short modeling explanation
   - region-filtered validation table for same-year fit checks

## Data sources

The app prefers the current project-root artifacts first, then falls back to
the synchronized app bundle in `../dashboard_assets/`.

Key files:

- `fit_logistic_final_model_regionFixedSpeed_countryK_boundedT0_dev.rds`
- `mcpr_data.csv`
- `country_pop_weights_15_49.csv`
- `mcpr_investment_table_final_model.csv`
- `mcpr_latest_observed_vs_projection.csv`
- `mcpr_s_curve_stage_classification.csv`
- `mcpr_s_curve_stage_classification_2026.csv`
- `mcpr_ssa_country_goals_summary.csv`
- `mcpr_ssa_region_goals_summary.csv`
- `ssa_validation_audit.csv`

The app uses `country_pop_weights_15_49.csv` so weighted regional views do not
have to read the full WPP workbook at runtime.

## Run locally

From the project root:

```r
shiny::runApp("shiny_dashboard")
```

## Refresh app assets

If the production report or exports change, refresh the app bundle from the
project root:

```r
Rscript scripts/build_country_pop_weights.R
Rscript scripts/sync_dashboard_assets.R
```

## Deployment note

Because the app only visualizes saved results and does not refit the model
live, it is a good candidate for easy managed Shiny hosting. The main thing to
preserve is that the synchronized data bundle and the production fit stay in
step with the latest analysis rerun.
