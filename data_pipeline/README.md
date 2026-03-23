# Data Pipeline

This folder contains the DHS data pull and construction workflow used to build the analysis-ready country-year input table.

## Main notebook

- `get_dhs.Rmd`
  - connects to the DHS API through `rdhs`
  - identifies DHS `IR` datasets from 1985 onward
  - reads cached DHS files locally through the `rdhs` cache
  - computes design-based all-women mCPR using modern-method status from `v313`
  - uses `v005` weights and survey-design information from `v021`, `v022`, and `v023`
  - aggregates survey results into a country-year dataset with `mCPR`, `se_mCPR`, and `n_women`

## Important context

- DHS microdata are not stored in this repository.
- You need valid DHS API access and a configured `rdhs` cache on your machine to rerun the pull.
- The project analysis and app rely on the derived country-year table produced from this workflow, not on live DHS pulls at runtime.
