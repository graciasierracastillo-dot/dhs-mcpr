# Analysis Reports

This folder contains the three main R Markdown analyses used in the project.

## Reports

- `dhs_analysis_production.Rmd`
  - the main report and primary decision-facing workflow
  - hierarchical logistic model with country-level `K`, bounded country-level `t0`, and region-fixed `s`
  - includes diagnostics, holdout validation, SSA 40% goal analysis, outlier analysis, S-curve stages, and app-facing exports
- `dhs_analysis_region_fixedK_countryS.Rmd`
  - alternative logistic report that fixes `K` at the region level and estimates country-specific `s`
  - used to assess sensitivity to the identification strategy and the `K` versus `s` tradeoff
- `bayesian_gamm_mcpr_report_v4.Rmd`
  - flexible Bayesian hierarchical GAMM alternative
  - emphasizes nonlinear trajectory shape, growth, and acceleration rather than a structural asymptote parameter

## How to use these reports

- Start with `dhs_analysis_production.Rmd` for the main project narrative and exported results.
- Use `dhs_analysis_region_fixedK_countryS.Rmd` when checking how conclusions change under the flipped logistic specification.
- Use `bayesian_gamm_mcpr_report_v4.Rmd` as the flexible benchmark when the main question is about curve shape and timing rather than `K`.

## Notes

- These reports assume access to the derived country-year input table built from the DHS pull workflow.
- The saved app bundle in `dashboard_assets/` is derived from exports produced by these analyses.
