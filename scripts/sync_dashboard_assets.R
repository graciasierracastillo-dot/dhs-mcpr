get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) == 0) {
    stop("Run this script with Rscript so the script path is available.")
  }

  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
}

script_dir <- get_script_dir()
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
asset_root <- file.path(project_root, "dashboard_assets")

asset_map <- data.frame(
  category = c(
    "model", "model", "model", "model",
    "input", "input", "input",
    "output", "output", "output", "output", "output", "output", "output", "output",
    "output", "output", "output", "output", "output", "output",
    "output", "output"
  ),
  subcategory = c(
    "production", "legacy_dashboard", "legacy_dashboard", "validation",
    "raw", "raw", "raw",
    "core", "core", "core", "core", "core", "core", "core", "core",
    "ssa", "ssa", "ssa", "ssa", "ssa", "ssa",
    "validation", "validation"
  ),
  file_name = c(
    "fit_logistic_final_model_regionFixedSpeed_countryK_boundedT0_dev.rds",
    "fit_logistic_final_model_regionFixedSpeed_countryK_t0RegionFE.rds",
    "fit_logistic_final_model_regionFixedSpeed_countryK_t0RegionFE_dev.rds",
    "fit_logistic_holdout10_lastobs_final_model_regionFixedSpeed_boundedT0_dev.rds",
    "mcpr_data.csv",
    "ssa_pop_weights.csv",
    "country_pop_weights_15_49.csv",
    "mcpr_investment_table_final_model.csv",
    "mcpr_investment_table_lastobs_headroom.csv",
    "mcpr_latest_observed_vs_projection.csv",
    "mcpr_time_to_99pct_k.csv",
    "mcpr_country_growth_rates_over_time.csv",
    "mcpr_acceleration_status_2026.csv",
    "mcpr_s_curve_stage_classification.csv",
    "mcpr_s_curve_stage_classification_2026.csv",
    "mcpr_ssa_target40_summary.csv",
    "mcpr_ssa_planning_summary.csv",
    "mcpr_ssa_observed_interval_growth.csv",
    "mcpr_ssa_region_goals_summary.csv",
    "mcpr_ssa_country_goals_summary.csv",
    "mcpr_ssa_regional_influence_summary.csv",
    "mcpr_final_model_fixeds_phase_stability.csv",
    "mcpr_final_model_kfixed_sensitivity.csv"
  ),
  source_path = c(
    "fit_logistic_final_model_regionFixedSpeed_countryK_boundedT0_dev.rds",
    "fit_logistic_final_model_regionFixedSpeed_countryK_t0RegionFE.rds",
    "fit_logistic_final_model_regionFixedSpeed_countryK_t0RegionFE_dev.rds",
    "fit_logistic_holdout10_lastobs_final_model_regionFixedSpeed_boundedT0_dev.rds",
    "mcpr_data.csv",
    "ssa_pop_weights.csv",
    "country_pop_weights_15_49.csv",
    "mcpr_investment_table_final_model.csv",
    "mcpr_investment_table_lastobs_headroom.csv",
    "mcpr_latest_observed_vs_projection.csv",
    "mcpr_time_to_99pct_k.csv",
    "mcpr_country_growth_rates_over_time.csv",
    "mcpr_acceleration_status_2026.csv",
    "mcpr_s_curve_stage_classification.csv",
    "mcpr_s_curve_stage_classification_2026.csv",
    "mcpr_ssa_target40_summary.csv",
    "mcpr_ssa_planning_summary.csv",
    "mcpr_ssa_observed_interval_growth.csv",
    "mcpr_ssa_region_goals_summary.csv",
    "mcpr_ssa_country_goals_summary.csv",
    "mcpr_ssa_regional_influence_summary.csv",
    "mcpr_final_model_fixeds_phase_stability.csv",
    "mcpr_final_model_kfixed_sensitivity.csv"
  ),
  destination = c(
    "models/production/fit_logistic_final_model_regionFixedSpeed_countryK_boundedT0_dev.rds",
    "models/legacy_dashboard/fit_logistic_final_model_regionFixedSpeed_countryK_t0RegionFE.rds",
    "models/legacy_dashboard/fit_logistic_final_model_regionFixedSpeed_countryK_t0RegionFE_dev.rds",
    "models/validation/fit_logistic_holdout10_lastobs_final_model_regionFixedSpeed_boundedT0_dev.rds",
    "inputs/mcpr_data.csv",
    "inputs/ssa_pop_weights.csv",
    "inputs/country_pop_weights_15_49.csv",
    "outputs/core/mcpr_investment_table_final_model.csv",
    "outputs/core/mcpr_investment_table_lastobs_headroom.csv",
    "outputs/core/mcpr_latest_observed_vs_projection.csv",
    "outputs/core/mcpr_time_to_99pct_k.csv",
    "outputs/core/mcpr_country_growth_rates_over_time.csv",
    "outputs/core/mcpr_acceleration_status_2026.csv",
    "outputs/core/mcpr_s_curve_stage_classification.csv",
    "outputs/core/mcpr_s_curve_stage_classification_2026.csv",
    "outputs/ssa/mcpr_ssa_target40_summary.csv",
    "outputs/ssa/mcpr_ssa_planning_summary.csv",
    "outputs/ssa/mcpr_ssa_observed_interval_growth.csv",
    "outputs/ssa/mcpr_ssa_region_goals_summary.csv",
    "outputs/ssa/mcpr_ssa_country_goals_summary.csv",
    "outputs/ssa/mcpr_ssa_regional_influence_summary.csv",
    "outputs/validation/mcpr_final_model_fixeds_phase_stability.csv",
    "outputs/validation/mcpr_final_model_kfixed_sensitivity.csv"
  ),
  role = c(
    "current production fit for new app work",
    "current fit used by shiny_dashboard/app.R",
    "development variant of the legacy dashboard fit",
    "holdout validation fit",
    "raw DHS model input",
    "SSA population weights",
    "women 15-49 population weights for all modeled countries",
    "country investment phase summary",
    "alternate headroom-based investment table",
    "latest observed versus model projection check",
    "time to reach 99 percent of ceiling",
    "model-implied country growth trajectories over time",
    "2026 acceleration status summary",
    "S-curve stage at latest observed point",
    "S-curve stage at 2026 projection",
    "SSA country target-40 summary",
    "SSA planning summary table",
    "observed SSA interval growth series",
    "weighted SSA regional goal summary",
    "SSA country goal-tracking table",
    "SSA regional influence table",
    "fixed-s phase stability summary",
    "region-fixed-K sensitivity summary"
  ),
  priority = c(
    "high", "high", "medium", "medium",
    "high", "high", "high",
    "high", "medium", "high", "medium", "high", "medium", "high", "high",
    "high", "high", "medium", "high", "high", "high",
    "medium", "medium"
  ),
  notes = c(
    "Primary fit from dhs_analysis_production.Rmd",
    "Preserved for backward compatibility with the existing app",
    "Useful while comparing app behavior against the current dashboard",
    "Useful for QA or methods views",
    "Needed for observed trajectories and country-year context",
    "Needed for weighted SSA regional aggregation",
    "Needed for weighted regional trajectories across the full app, including global region views",
    "Core planning table",
    "Useful for comparing p(2026)-based versus latest-observed headroom",
    "Useful for diagnostics and dashboard trust-building",
    "Good for saturation views",
    "Supports trend and phase visualizations",
    "Supports growth dynamics views",
    "Useful for stage-based filtering",
    "Useful for planning-state filtering",
    "Contains projected year to 40 percent and growth needed",
    "Compact table for SSA dashboard views",
    "Useful when comparing observed versus model-implied growth",
    "Primary regional target table",
    "Best single country-level SSA app table",
    "Referenced in dhs_analysis_production.Rmd export section but not present in the repo",
    "Useful for sensitivity or robustness pages",
    "Useful for model-structure sensitivity views"
  ),
  stringsAsFactors = FALSE
)

dir.create(asset_root, recursive = TRUE, showWarnings = FALSE)

copy_status <- lapply(seq_len(nrow(asset_map)), function(i) {
  dest_path <- file.path(asset_root, asset_map$destination[i])
  source_candidates <- c(
    file.path(project_root, asset_map$source_path[i]),
    file.path(asset_root, asset_map$destination[i])
  )
  existing_sources <- source_candidates[file.exists(source_candidates)]
  src_path <- if (length(existing_sources) > 0) existing_sources[1] else source_candidates[1]

  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)

  exists_now <- file.exists(src_path)
  copied <- FALSE

  if (exists_now) {
    copied <- if (
      normalizePath(src_path, winslash = "/", mustWork = TRUE) ==
      normalizePath(dest_path, winslash = "/", mustWork = FALSE)
    ) {
      TRUE
    } else {
      file.copy(src_path, dest_path, overwrite = TRUE)
    }
  }

  data.frame(
    category = asset_map$category[i],
    subcategory = asset_map$subcategory[i],
    file_name = asset_map$file_name[i],
    source_path = asset_map$source_path[i],
    role = asset_map$role[i],
    priority = asset_map$priority[i],
    status = if (!exists_now) "missing" else if (copied) "available" else "copy_failed",
    notes = asset_map$notes[i],
    stringsAsFactors = FALSE
  )
})

manifest <- do.call(rbind, copy_status)
write.csv(manifest, file.path(asset_root, "manifest.csv"), row.names = FALSE)

cat("Dashboard asset sync complete.\n")
cat("Asset root:", asset_root, "\n")
cat("Available:", sum(manifest$status == "available"), "\n")
cat("Missing:", sum(manifest$status == "missing"), "\n")
