# Dashboard Assets

This folder contains the minimum saved data objects needed to run the Shiny dashboard without re-running the full analysis pipeline.

## Structure

- `inputs/`
  - country-year input tables used for observed trajectories and regional weighting
- `models/production/`
  - the saved production Bayesian fit used by the app
- `outputs/core/`
  - core model summary tables used across the app
- `outputs/ssa/`
  - SSA-specific target, growth, and regional summary tables

## Purpose

These files are the deployable asset bundle for the app. They let the dashboard:

- display observed DHS trajectories
- reconstruct fitted country curves from the saved posterior fit
- power SSA goal-tracking views
- show S-curve stage summaries and validation tables

The app uses these saved assets only and does not fit the model during runtime.
