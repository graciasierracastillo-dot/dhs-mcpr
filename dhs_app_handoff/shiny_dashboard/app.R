suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(brms)
  library(posterior)
  library(readr)
  library(htmltools)
  library(scales)
  library(stringr)
  library(purrr)
})

get_app_dir <- function() {
  frame_files <- vapply(
    sys.frames(),
    function(x) if (!is.null(x$ofile)) x$ofile else "",
    character(1)
  )
  frame_files <- frame_files[nzchar(frame_files)]

  if (length(frame_files) > 0) {
    normalized <- vapply(
      frame_files,
      function(path) {
        if (file.exists(path)) {
          normalizePath(path, winslash = "/", mustWork = TRUE)
        } else {
          ""
        }
      },
      character(1)
    )

    app_match <- normalized[nzchar(normalized) & basename(normalized) == "app.R"]
    if (length(app_match) > 0) {
      return(dirname(app_match[1]))
    }

    existing <- normalized[nzchar(normalized)]
    if (length(existing) > 0) {
      return(dirname(existing[1]))
    }
  }

  normalizePath(getwd(), mustWork = TRUE)
}

resolve_first_existing <- function(label, paths) {
  existing <- paths[file.exists(paths)]

  if (length(existing) == 0) {
    stop("Dashboard is missing required ", label, ". Tried: ", paste(paths, collapse = ", "))
  }

  normalizePath(existing[1], winslash = "/", mustWork = TRUE)
}

format_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "NA", sprintf(paste0("%.", digits, "f%%"), 100 * x))
}

format_pp <- function(x, digits = 2) {
  ifelse(is.na(x), "NA", paste0(sprintf(paste0("%.", digits, "f"), x), " pp/year"))
}

format_year <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.0f", x))
}

clamp_prob <- function(x) {
  pmin(pmax(x, 1e-6), 1 - 1e-6)
}

interactive_plot <- function(plot_obj, tooltip = "text", x_year = FALSE) {
  p <- suppressWarnings(ggplotly(plot_obj, tooltip = tooltip, dynamicTicks = FALSE)) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "lasso2d",
        "select2d",
        "autoScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    ) %>%
    layout(
      legend = list(orientation = "h", x = 0, y = -0.18),
      margin = list(l = 70, r = 30, b = 70, t = 60),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )

  if (x_year) {
    current_xaxis <- if (is.null(p$x$layout$xaxis)) list() else p$x$layout$xaxis
    p$x$layout$xaxis <- modifyList(
      current_xaxis,
      list(
        tickformat = ".0f",
        hoverformat = ".0f",
        separatethousands = FALSE
      )
    )
  }

  p
}

metric_card <- function(label, value, sub = NULL, accent = "blue") {
  div(
    class = paste("metric-card", paste0("accent-", accent)),
    div(class = "metric-label", label),
    div(class = "metric-value", value),
    if (!is.null(sub)) div(class = "metric-sub", sub)
  )
}

story_note <- function(...) {
  div(class = "story-note", ...)
}

region_lookup <- c(
  "Cambodia" = "East Asia and Pacific",
  "Indonesia" = "East Asia and Pacific",
  "Philippines" = "East Asia and Pacific",
  "Timor-Leste" = "East Asia and Pacific",
  "Vietnam" = "East Asia and Pacific",
  "Albania" = "Europe and Central Asia",
  "Armenia" = "Europe and Central Asia",
  "Kazakhstan" = "Europe and Central Asia",
  "Kyrgyz Republic" = "Europe and Central Asia",
  "Tajikistan" = "Europe and Central Asia",
  "Turkey" = "Europe and Central Asia",
  "Bolivia" = "Latin America and Carribbean",
  "Brazil" = "Latin America and Carribbean",
  "Colombia" = "Latin America and Carribbean",
  "Dominican Republic" = "Latin America and Carribbean",
  "Guatemala" = "Latin America and Carribbean",
  "Haiti" = "Latin America and Carribbean",
  "Honduras" = "Latin America and Carribbean",
  "Nicaragua" = "Latin America and Carribbean",
  "Peru" = "Latin America and Carribbean",
  "Egypt" = "Middle East and North Africa",
  "Jordan" = "Middle East and North Africa",
  "Morocco" = "Middle East and North Africa",
  "Yemen" = "Middle East and North Africa",
  "Bangladesh" = "South Asia",
  "India" = "South Asia",
  "Maldives" = "South Asia",
  "Nepal" = "South Asia",
  "Pakistan" = "South Asia",
  "Benin" = "West Africa",
  "Burkina Faso" = "West Africa",
  "Cote d'Ivoire" = "West Africa",
  "Gambia" = "West Africa",
  "Ghana" = "West Africa",
  "Guinea" = "West Africa",
  "Liberia" = "West Africa",
  "Mali" = "West Africa",
  "Niger" = "West Africa",
  "Nigeria" = "West Africa",
  "Senegal" = "West Africa",
  "Sierra Leone" = "West Africa",
  "Togo" = "West Africa",
  "Cameroon" = "Central Africa",
  "Chad" = "Central Africa",
  "Congo" = "Central Africa",
  "Congo Democratic Republic" = "Central Africa",
  "Gabon" = "Central Africa",
  "Angola" = "East/Southern Africa",
  "Burundi" = "East/Southern Africa",
  "Comoros" = "East/Southern Africa",
  "Ethiopia" = "East/Southern Africa",
  "Kenya" = "East/Southern Africa",
  "Lesotho" = "East/Southern Africa",
  "Madagascar" = "East/Southern Africa",
  "Malawi" = "East/Southern Africa",
  "Mozambique" = "East/Southern Africa",
  "Namibia" = "East/Southern Africa",
  "Rwanda" = "East/Southern Africa",
  "South Africa" = "East/Southern Africa",
  "Tanzania" = "East/Southern Africa",
  "Uganda" = "East/Southern Africa",
  "Zambia" = "East/Southern Africa",
  "Zimbabwe" = "East/Southern Africa"
)

region_code_lookup <- c(
  "East Asia and Pacific" = "EAP",
  "Europe and Central Asia" = "ECA",
  "Latin America and Carribbean" = "LAC",
  "Middle East and North Africa" = "MENA",
  "South Asia" = "SA",
  "West Africa" = "WAF",
  "Central Africa" = "CAF",
  "East/Southern Africa" = "ESA"
)

region_levels <- c(
  "Central Africa",
  "East Asia and Pacific",
  "East/Southern Africa",
  "Europe and Central Asia",
  "Latin America and Carribbean",
  "Middle East and North Africa",
  "South Asia",
  "West Africa"
)

ssa_regions <- c("West Africa", "Central Africa", "East/Southern Africa")

region_palette <- c(
  "Central Africa" = "#8C564B",
  "East Asia and Pacific" = "#1B9E77",
  "East/Southern Africa" = "#4C78A8",
  "Europe and Central Asia" = "#7570B3",
  "Latin America and Carribbean" = "#D95F02",
  "Middle East and North Africa" = "#E7298A",
  "South Asia" = "#66A61E",
  "West Africa" = "#E6AB02",
  "Global weighted" = "#111827",
  "Sub-Saharan Africa (weighted)" = "#0F172A"
)

stage_palette <- c(
  "Stage 1: initial" = "#D55E00",
  "Stage 2: diffusion" = "#0072B2",
  "Stage 3: plateau" = "#009E73"
)

crossing_palette <- c(
  "Already crossed" = "#047857",
  "Projected to cross by 2035" = "#2563EB",
  "Projected to cross after 2035" = "#D97706",
  "Not projected to cross" = "#B91C1C"
)

gap_palette <- c(
  "Population-weighted region" = "#111827",
  "Latest survey 2015 or later" = "#2563EB",
  "Latest survey before 2015" = "#F59E0B"
)

feasibility_palette <- c(
  "Current annualized growth rate above required" = "#0F766E",
  "Below required annualized growth rate, K above 40%" = "#2563EB",
  "Below required annualized growth rate, K spans 40%" = "#F59E0B",
  "K below 40%" = "#B91C1C"
)

target_mcpr <- 0.40
decision_year <- 2026
goal_year <- 2035
ci_lower <- 0.025
ci_upper <- 0.975
t0_lower <- -34
t0_span <- 69

app_dir <- get_app_dir()
project_root <- normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = TRUE)
dashboard_asset_root <- file.path(project_root, "dashboard_assets")

fit_path <- resolve_first_existing(
  "fit object",
  c(
    file.path(project_root, "fit_logistic_final_model_regionFixedSpeed_countryK_boundedT0_dev.rds"),
    file.path(dashboard_asset_root, "models", "production", "fit_logistic_final_model_regionFixedSpeed_countryK_boundedT0_dev.rds")
  )
)
mcpr_path <- resolve_first_existing(
  "mCPR input data",
  c(
    file.path(project_root, "mcpr_data.csv"),
    file.path(dashboard_asset_root, "inputs", "mcpr_data.csv")
  )
)
country_pop_path <- resolve_first_existing(
  "country women 15-49 population weights",
  c(
    file.path(project_root, "country_pop_weights_15_49.csv"),
    file.path(dashboard_asset_root, "inputs", "country_pop_weights_15_49.csv")
  )
)
investment_path <- resolve_first_existing(
  "investment table",
  c(
    file.path(project_root, "mcpr_investment_table_final_model.csv"),
    file.path(dashboard_asset_root, "outputs", "core", "mcpr_investment_table_final_model.csv")
  )
)
latest_projection_path <- resolve_first_existing(
  "latest observed versus projection table",
  c(
    file.path(project_root, "mcpr_latest_observed_vs_projection.csv"),
    file.path(dashboard_asset_root, "outputs", "core", "mcpr_latest_observed_vs_projection.csv")
  )
)
growth_over_time_path <- resolve_first_existing(
  "country growth over time table",
  c(
    file.path(project_root, "mcpr_country_growth_rates_over_time.csv"),
    file.path(dashboard_asset_root, "outputs", "core", "mcpr_country_growth_rates_over_time.csv")
  )
)
stage_lastobs_path <- resolve_first_existing(
  "stage classification table",
  c(
    file.path(project_root, "mcpr_s_curve_stage_classification.csv"),
    file.path(dashboard_asset_root, "outputs", "core", "mcpr_s_curve_stage_classification.csv")
  )
)
stage_2026_path <- resolve_first_existing(
  "2026 stage classification table",
  c(
    file.path(project_root, "mcpr_s_curve_stage_classification_2026.csv"),
    file.path(dashboard_asset_root, "outputs", "core", "mcpr_s_curve_stage_classification_2026.csv")
  )
)
time_to_k_path <- resolve_first_existing(
  "time-to-K table",
  c(
    file.path(project_root, "mcpr_time_to_99pct_k.csv"),
    file.path(dashboard_asset_root, "outputs", "core", "mcpr_time_to_99pct_k.csv")
  )
)
ssa_country_goals_path <- resolve_first_existing(
  "SSA country goals summary",
  c(
    file.path(project_root, "mcpr_ssa_country_goals_summary.csv"),
    file.path(dashboard_asset_root, "outputs", "ssa", "mcpr_ssa_country_goals_summary.csv")
  )
)
ssa_region_goals_path <- resolve_first_existing(
  "SSA region goals summary",
  c(
    file.path(project_root, "mcpr_ssa_region_goals_summary.csv"),
    file.path(dashboard_asset_root, "outputs", "ssa", "mcpr_ssa_region_goals_summary.csv")
  )
)
ssa_interval_growth_path <- resolve_first_existing(
  "SSA observed interval growth table",
  c(
    file.path(project_root, "mcpr_ssa_observed_interval_growth.csv"),
    file.path(dashboard_asset_root, "outputs", "ssa", "mcpr_ssa_observed_interval_growth.csv")
  )
)
ssa_validation_path <- resolve_first_existing(
  "SSA validation audit table",
  c(file.path(project_root, "ssa_validation_audit.csv"))
)

obs_data <- read_csv(mcpr_path, show_col_types = FALSE) %>%
  transmute(
    country,
    survey_id,
    year = as.integer(year),
    mCPR = as.numeric(mCPR),
    n_women = as.numeric(n_women),
    se_mCPR = as.numeric(se_mCPR),
    region = unname(region_lookup[country]),
    region_code = unname(region_code_lookup[region])
  ) %>%
  filter(!is.na(country), !is.na(year), !is.na(mCPR), !is.na(region_code)) %>%
  arrange(region, country, year)

country_latest_obs <- obs_data %>%
  group_by(country, region) %>%
  summarise(
    last_obs_year = max(year, na.rm = TRUE),
    last_obs_mcpr = mCPR[which.max(year)],
    n_surveys = n(),
    .groups = "drop"
  )

survey_interval_growth <- obs_data %>%
  group_by(country, region) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    prev_year = lag(year),
    prev_mcpr = lag(mCPR),
    interval_years = year - prev_year,
    obs_growth_pp_per_year = if_else(
      !is.na(prev_year) & interval_years > 0,
      100 * (mCPR - prev_mcpr) / interval_years,
      NA_real_
    ),
    interval_label = if_else(!is.na(prev_year), paste0(prev_year, "-", year), NA_character_),
    interval_mid_year = (prev_year + year) / 2
  ) %>%
  ungroup()

investment_table <- read_csv(investment_path, show_col_types = FALSE)
latest_vs_projection <- read_csv(latest_projection_path, show_col_types = FALSE)
growth_over_time <- read_csv(growth_over_time_path, show_col_types = FALSE)
stage_lastobs <- read_csv(stage_lastobs_path, show_col_types = FALSE)
stage_2026 <- read_csv(stage_2026_path, show_col_types = FALSE)
time_to_k <- read_csv(time_to_k_path, show_col_types = FALSE)
ssa_country_goals <- read_csv(ssa_country_goals_path, show_col_types = FALSE)
ssa_region_goals <- read_csv(ssa_region_goals_path, show_col_types = FALSE)
ssa_interval_growth <- read_csv(ssa_interval_growth_path, show_col_types = FALSE)
ssa_validation_audit <- read_csv(ssa_validation_path, show_col_types = FALSE)
country_pop_weights <- read_csv(country_pop_path, show_col_types = FALSE) %>%
  mutate(Year = as.integer(Year))

latest_weight_year <- max(country_pop_weights$Year, na.rm = TRUE)

country_catalog <- investment_table %>%
  left_join(stage_2026 %>% select(country, region, s_curve_stage_2026, p_2026_mean), by = "country") %>%
  left_join(stage_lastobs %>% select(country, latest_obs_year, latest_obs_mcpr, s_curve_stage), by = "country") %>%
  left_join(country_latest_obs, by = c("country", "region")) %>%
  left_join(
    latest_vs_projection %>%
      rename(
        projection_latest_mean = projected_latest_mean,
        projection_latest_p05 = projected_latest_p05,
        projection_latest_p95 = projected_latest_p95,
        projection_gap_latest = projection_gap,
        projection_gap_abs_latest = abs_projection_gap,
        projection_status_latest = status_latest_projection
      ) %>%
      select(
        country, region,
        projection_latest_mean, projection_latest_p05, projection_latest_p95,
        projection_gap_latest, projection_gap_abs_latest, projection_status_latest
      ),
    by = c("country", "region")
  ) %>%
  left_join(time_to_k %>% select(country, year_reach_99pctK_mean, year_reach_99pctK_p05, year_reach_99pctK_p95), by = "country") %>%
  mutate(
    region = factor(region, levels = region_levels),
    region_code = unname(region_code_lookup[as.character(region)]),
    s_fixed = s_mean,
    latest_obs_year = coalesce(latest_obs_year, last_obs_year),
    latest_obs_mcpr = coalesce(latest_obs_mcpr, last_obs_mcpr),
    is_ssa = as.character(region) %in% ssa_regions
  ) %>%
  arrange(region, country)

country_names <- country_catalog$country
country_names_ssa <- country_catalog %>%
  filter(is_ssa) %>%
  pull(country)

fit <- readRDS(fit_path)
nd_country_param <- country_catalog %>%
  transmute(
    country = as.character(country),
    region = as.character(region),
    region_code = as.character(region_code),
    s_fixed = as.numeric(s_fixed),
    region_prior = as.character(region_code),
    year_center = 0,
    n = 1,
    y = 0
  ) %>%
  distinct(country, region, region_code, s_fixed, region_prior, year_center, n, y) %>%
  arrange(country)

etaK_draws <- posterior_linpred(
  fit,
  nlpar = "Kraw",
  newdata = nd_country_param %>% select(country, region_prior, s_fixed, year_center, n, y) %>% rename(s_region_fixed = s_fixed),
  re_formula = NULL,
  transform = FALSE,
  summary = FALSE
)
K_draws <- plogis(etaK_draws)

t0_raw_draws <- posterior_linpred(
  fit,
  nlpar = "t0raw",
  newdata = nd_country_param %>% select(country, region_prior, s_fixed, year_center, n, y) %>% rename(s_region_fixed = s_fixed),
  re_formula = NULL,
  transform = FALSE,
  summary = FALSE
)
t0_center_draws <- t0_lower + t0_span * plogis(t0_raw_draws)

s_draws_country <- matrix(
  rep(nd_country_param$s_fixed, each = nrow(K_draws)),
  nrow = nrow(K_draws),
  ncol = ncol(K_draws)
)

country_draw_cache <- setNames(
  lapply(seq_len(nrow(nd_country_param)), function(i) {
    list(
      country = nd_country_param$country[[i]],
      region = nd_country_param$region[[i]],
      region_code = nd_country_param$region_code[[i]],
      s_fixed = nd_country_param$s_fixed[[i]],
      K = K_draws[, i],
      t0_center = t0_center_draws[, i],
      t0_year = 2000 + t0_center_draws[, i]
    )
  }),
  nd_country_param$country
)

projection_year_grid <- seq(min(obs_data$year, na.rm = TRUE), 2060, by = 1)

country_projection_matrix <- function(country_draws, years) {
  centered <- outer(
    country_draws$t0_center,
    years - 2000,
    function(t0, yy) (yy - t0) / country_draws$s_fixed
  )
  gate <- plogis(centered)
  sweep(gate, 1, country_draws$K, "*")
}

country_derivative_matrix <- function(country_draws, years) {
  centered <- outer(
    country_draws$t0_center,
    years - 2000,
    function(t0, yy) (yy - t0) / country_draws$s_fixed
  )
  gate <- plogis(centered)
  sweep(gate * (1 - gate), 1, country_draws$K / country_draws$s_fixed, "*")
}

summarise_draw_matrix <- function(mat) {
  tibble(
    mean = colMeans(mat),
    lo = apply(mat, 2, quantile, probs = ci_lower, na.rm = TRUE),
    hi = apply(mat, 2, quantile, probs = ci_upper, na.rm = TRUE)
  )
}

country_curve_summary <- bind_rows(
  lapply(country_names, function(ctry) {
    country_draws <- country_draw_cache[[ctry]]
    p_mat <- country_projection_matrix(country_draws, projection_year_grid)
    dpdt_mat <- country_derivative_matrix(country_draws, projection_year_grid)
    p_sum <- summarise_draw_matrix(p_mat)
    dpdt_sum <- summarise_draw_matrix(100 * dpdt_mat)

    tibble(
      country = ctry,
      region = country_draws$region,
      year = projection_year_grid,
      p_mean = p_sum$mean,
      p_lo = p_sum$lo,
      p_hi = p_sum$hi,
      dpdt_mean = dpdt_sum$mean,
      dpdt_lo = dpdt_sum$lo,
      dpdt_hi = dpdt_sum$hi
    )
  })
) %>%
  arrange(region, country, year)

country_pop_lookup <- split(country_pop_weights, country_pop_weights$country)

build_group_weight_matrix <- function(countries, years, dynamic = FALSE) {
  weight_years <- if (dynamic) pmin(years, latest_weight_year) else rep(latest_weight_year, length(years))

  raw_mat <- vapply(
    countries,
    function(ctry) {
      pop_tbl <- country_pop_lookup[[ctry]]
      pop_tbl$women_15_49[match(weight_years, pop_tbl$Year)]
    },
    FUN.VALUE = numeric(length(years))
  )

  if (!is.matrix(raw_mat)) {
    raw_mat <- matrix(raw_mat, ncol = length(countries))
  }

  raw_mat <- t(raw_mat)
  denom <- colSums(raw_mat, na.rm = TRUE)
  sweep(raw_mat, 2, denom, "/")
}

aggregate_weighted_draws <- function(countries, years, dynamic = FALSE, derivative = FALSE) {
  n_draws <- length(country_draw_cache[[countries[1]]]$K)
  weight_mat <- build_group_weight_matrix(countries, years, dynamic = dynamic)
  out <- matrix(0, nrow = n_draws, ncol = length(years))

  for (i in seq_along(countries)) {
    country_draws <- country_draw_cache[[countries[i]]]
    country_mat <- if (derivative) {
      country_derivative_matrix(country_draws, years)
    } else {
      country_projection_matrix(country_draws, years)
    }

    out <- out + sweep(country_mat, 2, weight_mat[i, ], "*")
  }

  out
}

weighted_group_list <- c(
  setNames(lapply(region_levels, function(reg) {
    country_catalog %>%
      filter(as.character(region) == reg) %>%
      pull(country)
  }), region_levels),
  list(
    "Sub-Saharan Africa (weighted)" = country_names_ssa,
    "Global weighted" = country_names
  )
)

weighted_group_meta <- lapply(names(weighted_group_list), function(group_name) {
  countries <- weighted_group_list[[group_name]]
  total_pop <- build_group_weight_matrix(countries, latest_weight_year, dynamic = FALSE)
  latest_weights <- as.numeric(total_pop[, 1])
  names(latest_weights) <- countries

  projection_draws <- aggregate_weighted_draws(countries, projection_year_grid, dynamic = FALSE, derivative = FALSE)
  derivative_draws <- aggregate_weighted_draws(countries, projection_year_grid, dynamic = FALSE, derivative = TRUE)

  curve_summary <- summarise_draw_matrix(projection_draws)
  derivative_summary <- summarise_draw_matrix(100 * derivative_draws)

  k_draws <- Reduce(
    `+`,
    lapply(seq_along(countries), function(i) country_draw_cache[[countries[i]]]$K * latest_weights[i])
  )
  p2026_draws <- projection_draws[, match(decision_year, projection_year_grid)]
  p2025_draws <- projection_draws[, match(2025, projection_year_grid)]
  p2035_draws <- projection_draws[, match(goal_year, projection_year_grid)]
  rel_sat_draws <- clamp_prob(p2026_draws / pmax(k_draws, 1e-6))
  stage_position_draws <- qlogis(rel_sat_draws)
  stage1 <- stage_position_draws < -1.5
  stage3 <- stage_position_draws > 1.5
  stage2 <- !(stage1 | stage3)

  stage_label <- case_when(
    mean(stage1) >= mean(stage2) & mean(stage1) >= mean(stage3) ~ "Stage 1: initial",
    mean(stage3) >= mean(stage1) & mean(stage3) >= mean(stage2) ~ "Stage 3: plateau",
    TRUE ~ "Stage 2: diffusion"
  )

  inflection_draws <- apply(
    projection_draws >= matrix(0.5 * k_draws, nrow = nrow(projection_draws), ncol = ncol(projection_draws)),
    1,
    function(hit_vec) {
      idx <- which(hit_vec)
      if (length(idx) == 0) NA_real_ else projection_year_grid[min(idx)]
    }
  )

  crossing_draws <- apply(
    projection_draws >= target_mcpr,
    1,
    function(hit_vec) {
      idx <- which(hit_vec)
      if (length(idx) == 0) NA_real_ else projection_year_grid[min(idx)]
    }
  )

  observed_weighted <- obs_data %>%
    filter(country %in% countries) %>%
    inner_join(
      country_pop_weights %>% select(country, Year, women_15_49),
      by = c("country", "year" = "Year")
    ) %>%
    group_by(year) %>%
    summarise(
      observed_weighted_mcpr = weighted.mean(mCPR, women_15_49, na.rm = TRUE),
      n_countries_obs = n_distinct(country),
      .groups = "drop"
    ) %>%
    mutate(geography = group_name)

  list(
    geography = group_name,
    countries = countries,
    curve = tibble(
      geography = group_name,
      year = projection_year_grid,
      p_mean = curve_summary$mean,
      p_lo = curve_summary$lo,
      p_hi = curve_summary$hi,
      dpdt_mean = derivative_summary$mean,
      dpdt_lo = derivative_summary$lo,
      dpdt_hi = derivative_summary$hi
    ),
    observed = observed_weighted,
    summary = tibble(
      geography = group_name,
      women_15_49 = sum(country_pop_weights %>% filter(country %in% countries, Year == latest_weight_year) %>% pull(women_15_49), na.rm = TRUE),
      K_mean = mean(k_draws, na.rm = TRUE),
      K_lo = quantile(k_draws, ci_lower, na.rm = TRUE),
      K_hi = quantile(k_draws, ci_upper, na.rm = TRUE),
      p2025_mean = mean(p2025_draws, na.rm = TRUE),
      p2025_lo = quantile(p2025_draws, ci_lower, na.rm = TRUE),
      p2025_hi = quantile(p2025_draws, ci_upper, na.rm = TRUE),
      p2026_mean = mean(p2026_draws, na.rm = TRUE),
      p2026_lo = quantile(p2026_draws, ci_lower, na.rm = TRUE),
      p2026_hi = quantile(p2026_draws, ci_upper, na.rm = TRUE),
      p2035_mean = mean(p2035_draws, na.rm = TRUE),
      dpdt_2025_mean = mean(100 * derivative_draws[, match(2025, projection_year_grid)], na.rm = TRUE),
      dpdt_2026_mean = mean(100 * derivative_draws[, match(decision_year, projection_year_grid)], na.rm = TRUE),
      rel_sat_mean = mean(rel_sat_draws, na.rm = TRUE),
      stage_position_mean = mean(stage_position_draws, na.rm = TRUE),
      prob_stage1 = mean(stage1, na.rm = TRUE),
      prob_stage2 = mean(stage2, na.rm = TRUE),
      prob_stage3 = mean(stage3, na.rm = TRUE),
      stage_label = stage_label,
      inflection_p50 = median(inflection_draws, na.rm = TRUE),
      inflection_lo = quantile(inflection_draws, ci_lower, na.rm = TRUE),
      inflection_hi = quantile(inflection_draws, ci_upper, na.rm = TRUE),
      crossing_p50 = suppressWarnings(median(crossing_draws, na.rm = TRUE)),
      crossing_lo = suppressWarnings(quantile(crossing_draws, ci_lower, na.rm = TRUE)),
      crossing_hi = suppressWarnings(quantile(crossing_draws, ci_upper, na.rm = TRUE)),
      prob_reach_40_by_2035 = mean(!is.na(crossing_draws) & crossing_draws <= goal_year, na.rm = TRUE)
    )
  )
})
names(weighted_group_meta) <- names(weighted_group_list)

weighted_curve_summary <- bind_rows(lapply(weighted_group_meta, `[[`, "curve"))
weighted_observed_summary <- bind_rows(lapply(weighted_group_meta, `[[`, "observed"))
weighted_stage_summary <- bind_rows(lapply(weighted_group_meta, `[[`, "summary")) %>%
  mutate(
    region_group = case_when(
      geography == "Sub-Saharan Africa (weighted)" ~ "Sub-Saharan Africa",
      geography == "Global weighted" ~ "Global",
      TRUE ~ geography
    )
  )

ssa_current_summary <- tibble(
  baseline = c(
    "Population-weighted latest observed mCPR across each country's last survey year",
    "Population-weighted model-estimated mCPR in 2025"
  ),
  current_mcpr_pct = c(
    100 * ssa_region_goals$last_obs_weighted_mean[[1]],
    100 * ssa_region_goals$p2025_mean[[1]]
  ),
  gap_to_40_pp = c(
    100 * pmax(target_mcpr - ssa_region_goals$last_obs_weighted_mean[[1]], 0),
    100 * pmax(target_mcpr - ssa_region_goals$p2025_mean[[1]], 0)
  ),
  required_growth_pp_per_year = c(
    ssa_region_goals$required_growth_from_lastobs_pp_per_year[[1]],
    ssa_region_goals$required_growth_from_p2025_pp_per_year[[1]]
  )
)

ssa_country_plot_base <- ssa_country_goals %>%
  mutate(
    survey_recency_group = if_else(last_obs_year >= 2015, "Latest survey 2015 or later", "Latest survey before 2015"),
    current_latest_obs_pct = 100 * last_obs_mcpr,
    current_p2025_pct = 100 * p2025_mean,
    gap_from_lastobs_pp = pmax(100 * (target_mcpr - last_obs_mcpr), 0),
    gap_from_p2025_pp = pmax(100 * (target_mcpr - p2025_mean), 0),
    women_15_49_millions = women_15_49 / 1e6,
    crossing_status = case_when(
      last_obs_mcpr >= target_mcpr ~ "Already crossed",
      is.na(projected_year_reach_40_p50) ~ "Not projected to cross",
      projected_year_reach_40_p50 <= goal_year ~ "Projected to cross by 2035",
      TRUE ~ "Projected to cross after 2035"
    ),
    feasibility_status = case_when(
      K_p95 < target_mcpr ~ "K below 40%",
      model_dpdt_2025_pp_per_year >= required_growth_from_p2025_pp_per_year ~ "Current annualized growth rate above required",
      K_p05 >= target_mcpr ~ "Below required annualized growth rate, K above 40%",
      TRUE ~ "Below required annualized growth rate, K spans 40%"
    ),
    latest_obs_status = if_else(current_latest_obs_pct >= 40, "Already at or above 40%", "Below 40% at latest observed")
  )

ssa_region_plot_base <- tibble(
  geography = "Sub-Saharan Africa (weighted)",
  region = "Sub-Saharan Africa",
  survey_recency_group = "Population-weighted region",
  current_latest_obs_pct = 100 * ssa_region_goals$last_obs_weighted_mean[[1]],
  current_p2025_pct = 100 * ssa_region_goals$p2025_mean[[1]],
  gap_from_lastobs_pp = 100 * pmax(target_mcpr - ssa_region_goals$last_obs_weighted_mean[[1]], 0),
  gap_from_p2025_pp = 100 * pmax(target_mcpr - ssa_region_goals$p2025_mean[[1]], 0),
  required_growth_from_lastobs_pp_per_year = ssa_region_goals$required_growth_from_lastobs_pp_per_year[[1]],
  required_growth_from_p2025_pp_per_year = ssa_region_goals$required_growth_from_p2025_pp_per_year[[1]],
  latest_obs_interval_growth_pp_per_year = ssa_region_goals$observed_latest_interval_growth_pp_per_year[[1]],
  model_dpdt_2025_pp_per_year = ssa_region_goals$model_dpdt_2025_pp_per_year[[1]],
  women_15_49 = weighted_stage_summary %>% filter(geography == "Sub-Saharan Africa (weighted)") %>% pull(women_15_49) %>% .[1],
  women_15_49_millions = women_15_49 / 1e6,
  K_mean = weighted_stage_summary %>% filter(geography == "Sub-Saharan Africa (weighted)") %>% pull(K_mean) %>% .[1],
  K_lo = weighted_stage_summary %>% filter(geography == "Sub-Saharan Africa (weighted)") %>% pull(K_lo) %>% .[1],
  K_hi = weighted_stage_summary %>% filter(geography == "Sub-Saharan Africa (weighted)") %>% pull(K_hi) %>% .[1],
  crossing_status = case_when(
    current_latest_obs_pct >= 40 ~ "Already crossed",
    is.na(ssa_region_goals$projected_year_reach_40_p50[[1]]) ~ "Not projected to cross",
    ssa_region_goals$projected_year_reach_40_p50[[1]] <= goal_year ~ "Projected to cross by 2035",
    TRUE ~ "Projected to cross after 2035"
  ),
  feasibility_status = case_when(
    K_hi < target_mcpr ~ "K below 40%",
    model_dpdt_2025_pp_per_year >= required_growth_from_p2025_pp_per_year ~ "Current annualized growth rate above required",
    K_lo >= target_mcpr ~ "Below required annualized growth rate, K above 40%",
    TRUE ~ "Below required annualized growth rate, K spans 40%"
  ),
  latest_obs_status = if_else(current_latest_obs_pct >= 40, "Already at or above 40%", "Below 40% at latest observed"),
  projected_year_reach_40_p50 = ssa_region_goals$projected_year_reach_40_p50[[1]],
  prob_reach_40_by_2035 = ssa_region_goals$prob_reach_40_by_2035[[1]]
)

ssa_growth_history_table <- bind_rows(
  weighted_curve_summary %>%
    filter(geography == "Sub-Saharan Africa (weighted)", year <= 2025) %>%
    transmute(
      geography = geography,
      region = "Sub-Saharan Africa",
      year,
      model_annualized_growth_pp_per_year = dpdt_mean,
      cri = paste0(sprintf("%.2f", dpdt_lo), " to ", sprintf("%.2f", dpdt_hi))
    ),
  country_curve_summary %>%
    filter(country %in% country_names_ssa, year <= 2025) %>%
    transmute(
      geography = country,
      region,
      year,
      model_annualized_growth_pp_per_year = dpdt_mean,
      cri = paste0(sprintf("%.2f", dpdt_lo), " to ", sprintf("%.2f", dpdt_hi))
    )
)

default_region <- "West Africa"
default_country <- country_catalog %>%
  filter(as.character(region) == default_region) %>%
  arrange(country) %>%
  slice(1) %>%
  pull(country)

default_weighted_region <- "Sub-Saharan Africa (weighted)"

ui <- fluidPage(
  tags$head(
    tags$title("mCPR Decision Dashboard"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(
    class = "hero-panel",
    div(class = "hero-eyebrow", "DHS mCPR Growth, Goals, And S-Curve Decision Support"),
    h1("Interactive dashboard for country trajectories, SSA goal feasibility, and weighted regional stage positioning"),
    p(
      class = "hero-subtitle",
      "This app reads the current fit and export tables, then turns the model into an interactive story: observed trajectories first, country and region interpretation next, and a focused Sub-Saharan Africa 40% goal workflow after that."
    )
  ),
  fluidRow(
    column(
      width = 3,
      div(
        class = "control-panel",
        h3("Controls"),
        conditionalPanel(
          condition = "input.main_tab == 'explorer' || input.main_tab == 'methods'",
          selectInput("region", "Region", choices = region_levels, selected = default_region),
          selectInput(
            "country",
            "Country",
            choices = country_catalog %>% filter(as.character(region) == default_region) %>% arrange(country) %>% pull(country),
            selected = default_country
          ),
          sliderInput(
            "projection_end_year",
            "Projection horizon",
            min = min(projection_year_grid),
            max = max(projection_year_grid),
            value = 2045,
            step = 1,
            sep = ""
          )
        ),
        conditionalPanel(
          condition = "input.main_tab == 'ssa'",
          radioButtons(
            "ssa_gap_baseline",
            "Gap baseline",
            choices = c(
              "Latest observed mCPR" = "last_obs",
              "Model-estimated p(2025)" = "p2025"
            ),
            selected = "p2025"
          ),
          checkboxGroupInput(
            "ssa_region_filter",
            "SSA regions",
            choices = ssa_regions,
            selected = ssa_regions
          ),
          checkboxGroupInput(
            "ssa_cross_filter",
            "Crossing status",
            choices = names(crossing_palette),
            selected = names(crossing_palette)
          ),
          selectizeInput(
            "ssa_highlight_countries",
            "Highlight countries",
            choices = sort(country_names_ssa),
            selected = c("Nigeria", "Congo Democratic Republic"),
            multiple = TRUE
          ),
          sliderInput(
            "ssa_plot_end_year",
            "SSA trajectory horizon",
            min = 2035,
            max = 2060,
            value = 2050,
            step = 1,
            sep = ""
          ),
          selectInput(
            "stage_panel_region",
            "Focused country S-curve region",
            choices = c("All SSA", ssa_regions),
            selected = "All SSA"
          )
        ),
        conditionalPanel(
          condition = "input.main_tab == 'global'",
          selectInput(
            "weighted_region_choice",
            "Weighted region view",
            choices = c("Sub-Saharan Africa (weighted)", region_levels, "Global weighted"),
            selected = default_weighted_region
          ),
          sliderInput(
            "weighted_region_end_year",
            "Weighted region horizon",
            min = 2035,
            max = 2060,
            value = 2045,
            step = 1,
            sep = ""
          )
        ),
        div(
          class = "control-footnote",
          "Observed values are DHS survey points. Weighted regional summaries use women ages 15-49 population weights, and values like 0.5 in growth tables mean 0.5 percentage points per year."
        )
      )
    ),
    column(
      width = 9,
      tabsetPanel(
        id = "main_tab",
        type = "tabs",
        tabPanel(
          title = "Trajectory Explorer",
          value = "explorer",
          story_note(
            HTML(
              "<strong>How to read the explorer:</strong> observed DHS points anchor the country history, the fitted line and ribbon show the model-implied trajectory with a 95% credible interval, the ceiling <code>K</code> marks the trajectory-implied upper level, and the inflection year marks the midpoint of the S curve. In this model, <code>s</code> is fixed by analytic region to reduce the country-level <code>K</code> versus <code>s</code> identifiability problem."
            )
          ),
          uiOutput("country_metric_cards"),
          fluidRow(
            column(width = 7, div(class = "section-card", plotlyOutput("country_trajectory_plot", height = "620px"))),
            column(width = 5, div(class = "section-card", plotlyOutput("country_growth_plot", height = "520px")))
          ),
          fluidRow(
            column(width = 7, div(class = "section-card", plotlyOutput("region_observed_plot", height = "420px"))),
            column(width = 5, div(class = "section-card", plotlyOutput("country_validation_plot", height = "420px")))
          ),
          div(class = "section-card", DTOutput("region_country_table"))
        ),
        tabPanel(
          title = "SSA Goal Story",
          value = "ssa",
          uiOutput("ssa_metric_cards"),
          tabsetPanel(
            id = "ssa_story_tab",
            type = "pills",
            tabPanel(
              title = "Overview",
              story_note(
                "The SSA goal workflow starts with two current baselines, both population-weighted: the latest observed country-by-country baseline and the model-estimated p(2025) baseline. The app then turns each gap to 40% into an annualized growth rate requirement and compares that requirement to observed and model-based growth."
              ),
              div(class = "section-card", DTOutput("ssa_current_summary_table")),
              div(class = "section-card", plotlyOutput("ssa_gap_plot", height = "1100px"))
            ),
            tabPanel(
              title = "Annualized Growth",
              story_note(
                "This is the core feasibility view. Points above the diagonal are already on a faster model-implied annualized growth rate than the annualized growth rate needed to reach 40% by 2035. Hover any point to see the exact benchmark mix and projected crossing status."
              ),
              div(class = "section-card", plotlyOutput("ssa_feasibility_plot", height = "920px")),
              div(class = "section-card", DTOutput("ssa_growth_benchmark_table")),
              div(class = "section-card", DTOutput("ssa_survey_interval_growth_table")),
              div(class = "section-card", DTOutput("ssa_model_growth_history_table"))
            ),
            tabPanel(
              title = "Trajectories",
              story_note(
                "The weighted SSA region is drawn as the thick black line. Country trajectories are filterable by SSA subregion and crossing status, with hover text for p(2025), fitted K, and crossing timing."
              ),
              div(class = "section-card", plotlyOutput("ssa_all_trajectories_plot", height = "620px")),
              div(class = "section-card", plotlyOutput("ssa_notyet_plot", height = "620px")),
              div(class = "section-card", plotlyOutput("ssa_already_crossed_plot", height = "480px"))
            ),
            tabPanel(
              title = "S-Curve And Ceiling",
              story_note(
                HTML(
                  "<strong>Stage classification at 2026:</strong> Stage 1 if <code>(2026 - t0) / s &lt; -1.5</code>, Stage 2 if that standardized position falls between <code>-1.5</code> and <code>1.5</code>, and Stage 3 if it is greater than <code>1.5</code>. Equivalently, the cutpoints correspond to roughly <code>p(2026)/K &lt; 0.18</code>, <code>0.18-0.82</code>, and <code>&gt; 0.82</code>. The country viewer below uses the same posterior mean and 95% credible interval summaries as the report."
                )
              ),
              div(class = "section-card", plotlyOutput("ssa_asymptote_plot", height = "720px")),
              div(class = "section-card", plotlyOutput("ssa_region_stage_plot", height = "980px")),
              div(
                class = "section-card",
                uiOutput("ssa_country_stage_selector_ui"),
                plotlyOutput("ssa_country_stage_focus_plot", height = "820px")
              )
            )
          )
        ),
        tabPanel(
          title = "Weighted Region Stage Views",
          value = "global",
          story_note(
            "This page uses the same weighted-region S-curve logic beyond SSA. You can inspect the weighted curve for any analytic region or the global weighted curve, then compare stage position, K, and p(2026) across all weighted regions."
          ),
          div(class = "section-card", plotlyOutput("weighted_region_stage_plot", height = "760px")),
          div(class = "section-card", DTOutput("weighted_region_stage_table"))
        ),
        tabPanel(
          title = "Methods And Validation",
          value = "methods",
          fluidRow(
            column(
              width = 6,
              div(
                class = "section-card prose-card",
                h3("Model structure"),
                p("The model is a hierarchical Bayesian logistic growth model with country-specific K and t0, but region-fixed s. That keeps the country ceiling and timing interpretation while reducing the K versus s confounding that appeared when both were allowed to vary freely by country."),
                p(HTML("The fitted mean trajectory is <code>p_i(t) = K_i / (1 + exp(-((t - 2000) - t0_i^*) / s_r))</code>, where <code>t0_i^*</code> is the bounded inflection-year transform used in the report.")),
                p("Observed DHS survey points are used directly for country histories. Weighted regional summaries are built afterward by aggregating country posterior trajectories with women ages 15-49 population weights.")
              )
            ),
            column(
              width = 6,
              div(
                class = "section-card prose-card",
                h3("Validation lens"),
                p("For countries with recent surveys, the first thing to check is whether the fitted same-year value tracks the latest observed DHS point closely. For SSA countries, the app also surfaces p(last observed year) and p(2025) from the audit file so we can catch mismatches quickly."),
                p("The Explorer tab is intended to be the first stop: look at the observed survey history, compare it to the fitted trajectory, then move into the SSA goal pages only after the country-level fit looks credible.")
              )
            )
          ),
          div(class = "section-card", DTOutput("validation_table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$region, {
    region_countries <- country_catalog %>%
      filter(as.character(region) == input$region) %>%
      arrange(country) %>%
      pull(country)

    selected_country <- if (!is.null(input$country) && input$country %in% region_countries) {
      input$country
    } else {
      region_countries[[1]]
    }

    updateSelectInput(session, "country", choices = region_countries, selected = selected_country)
  }, ignoreInit = FALSE)

  selected_country_row <- reactive({
    req(input$country)
    country_catalog %>%
      filter(country == input$country) %>%
      slice(1)
  })

  selected_country_curve <- reactive({
    req(input$country)
    country_curve_summary %>%
      filter(country == input$country, year <= input$projection_end_year) %>%
      arrange(year)
  })

  selected_country_obs <- reactive({
    req(input$country)
    obs_data %>%
      filter(country == input$country) %>%
      arrange(year)
  })

  output$country_metric_cards <- renderUI({
    row <- selected_country_row()
    p2025_row <- country_curve_summary %>% filter(country == row$country[[1]], year == 2025) %>% slice(1)
    latest_gap <- row$latest_obs_mcpr[[1]] - row$projection_latest_mean[[1]]

    div(
      class = "metric-grid",
      metric_card(
        "Latest observed survey",
        paste0(format_year(row$latest_obs_year[[1]]), " | ", format_pct(row$latest_obs_mcpr[[1]], 1)),
        paste0("n = ", row$n_surveys[[1]], " DHS surveys"),
        "blue"
      ),
      metric_card(
        "Model p(last observed year)",
        format_pct(row$projection_latest_mean[[1]], 1),
        paste0("Gap versus observed: ", sprintf("%.1f pp", 100 * latest_gap)),
        "amber"
      ),
      metric_card(
        "Model p(2025)",
        format_pct(p2025_row$p_mean[[1]], 1),
        paste0("95% CrI ", format_pct(p2025_row$p_lo[[1]], 1), " to ", format_pct(p2025_row$p_hi[[1]], 1)),
        "green"
      ),
      metric_card(
        "Fitted K",
        format_pct(row$K_mean[[1]], 1),
        paste0("95% CrI ", format_pct(row$K_p05[[1]], 1), " to ", format_pct(row$K_p95[[1]], 1)),
        "gold"
      ),
      metric_card(
        "Stage at 2026",
        row$s_curve_stage_2026[[1]],
        paste0("Inflection year ~", round(row$t0_year_mean[[1]])),
        "purple"
      ),
      metric_card(
        "Region-fixed s",
        sprintf("%.2f years", row$s_fixed[[1]]),
        "Smaller s implies a faster transition through the S curve",
        "slate"
      )
    )
  })

  output$country_trajectory_plot <- renderPlotly({
    row <- selected_country_row()
    curve_df <- selected_country_curve() %>%
      mutate(
        p_mean_pct = 100 * p_mean,
        p_lo_pct = 100 * p_lo,
        p_hi_pct = 100 * p_hi,
        hover = paste0(
          "<b>", country, "</b><br>",
          "Year: ", year, "<br>",
          "Model mCPR: ", format_pct(p_mean, 1), "<br>",
          "95% CrI: ", format_pct(p_lo, 1), " to ", format_pct(p_hi, 1), "<br>",
          "Region: ", region
        )
      ) %>%
      arrange(year)
    obs_df <- selected_country_obs() %>%
      mutate(
        mCPR_pct = 100 * mCPR,
        hover = paste0(
          "<b>", country, "</b><br>",
          "Survey: ", survey_id, "<br>",
          "Year: ", year, "<br>",
          "Observed mCPR: ", format_pct(mCPR, 1), "<br>",
          "Women in survey: ", comma(round(n_women))
        )
      ) %>%
      arrange(year)

    y_max <- min(
      100,
      ceiling(max(c(100 * row$K_p95[[1]], curve_df$p_hi_pct, obs_df$mCPR_pct), na.rm = TRUE) / 5) * 5 + 10
    )

    plot_ly(source = "country_trajectory_plot") %>%
      add_ribbons(
        data = curve_df,
        x = ~year,
        ymin = ~p_lo_pct,
        ymax = ~p_hi_pct,
        hoverinfo = "skip",
        line = list(color = "transparent"),
        fillcolor = "rgba(76,120,168,0.20)",
        name = "95% credible interval",
        showlegend = FALSE
      ) %>%
      add_lines(
        data = curve_df,
        x = ~year,
        y = ~p_mean_pct,
        text = ~hover,
        hoverinfo = "text",
        line = list(color = "#0F4C81", width = 4),
        name = "Fitted model",
        showlegend = FALSE
      ) %>%
      add_markers(
        data = obs_df,
        x = ~year,
        y = ~mCPR_pct,
        text = ~hover,
        hoverinfo = "text",
        marker = list(color = "#B91C1C", size = 8, line = list(color = "white", width = 0.8)),
        name = "Observed DHS",
        showlegend = FALSE
      ) %>%
      layout(
        title = list(
          text = paste0(
            row$country[[1]],
            ": observed DHS trajectory and fitted model",
            "<br><sup>Red points are observed DHS mCPR. The blue curve and band show the fitted model with a 95% credible interval. Ochre marks K, blue marks the inflection year, and the dark dashed guide marks 2035.</sup>"
          ),
          x = 0
        ),
        xaxis = list(
          title = "Year",
          tickformat = ".0f",
          hoverformat = ".0f",
          separatethousands = FALSE,
          showgrid = TRUE,
          gridcolor = "rgba(229,231,235,0.9)",
          zeroline = FALSE
        ),
        yaxis = list(
          title = "mCPR (%)",
          range = c(0, y_max),
          showgrid = TRUE,
          gridcolor = "rgba(229,231,235,0.9)",
          zeroline = FALSE
        ),
        shapes = list(
          list(
            type = "line",
            x0 = min(curve_df$year, na.rm = TRUE),
            x1 = max(curve_df$year, na.rm = TRUE),
            y0 = 100 * row$K_mean[[1]],
            y1 = 100 * row$K_mean[[1]],
            line = list(color = "#8A5A00", dash = "dot", width = 2.2)
          ),
          list(
            type = "line",
            x0 = row$t0_year_mean[[1]],
            x1 = row$t0_year_mean[[1]],
            y0 = 0,
            y1 = y_max,
            line = list(color = "#1D4ED8", dash = "dot", width = 2)
          ),
          list(
            type = "line",
            x0 = goal_year,
            x1 = goal_year,
            y0 = 0,
            y1 = y_max,
            line = list(color = "#374151", dash = "dash", width = 1.8)
          )
        ),
        annotations = list(
          list(
            x = max(curve_df$year, na.rm = TRUE) - 0.5,
            y = 100 * row$K_mean[[1]],
            text = paste0("K: ", sprintf("%.1f%%", 100 * row$K_mean[[1]])),
            xanchor = "right",
            yanchor = "bottom",
            showarrow = FALSE,
            font = list(color = "#8A5A00", size = 13)
          ),
          list(
            x = row$t0_year_mean[[1]],
            y = y_max - 2,
            text = paste0("t0 ~ ", round(row$t0_year_mean[[1]])),
            textangle = -90,
            xanchor = "left",
            yanchor = "top",
            showarrow = FALSE,
            font = list(color = "#1D4ED8", size = 12)
          ),
          list(
            x = goal_year,
            y = y_max - 2,
            text = "2035",
            textangle = -90,
            xanchor = "left",
            yanchor = "top",
            showarrow = FALSE,
            font = list(color = "#374151", size = 12)
          )
        ),
        margin = list(l = 70, r = 35, b = 70, t = 88),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "lasso2d",
          "select2d",
          "autoScale2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian"
        )
      )
  })

  output$country_growth_plot <- renderPlotly({
    country_name <- input$country
    growth_curve <- country_curve_summary %>%
      filter(country == country_name, year <= input$projection_end_year) %>%
      mutate(
        dpdt_mean_pp = dpdt_mean,
        dpdt_lo_pp = dpdt_lo,
        dpdt_hi_pp = dpdt_hi,
        hover = paste0(
          "<b>", country, "</b><br>",
          "Year: ", year, "<br>",
          "Model annualized growth rate: ", sprintf("%.2f pp/year", dpdt_mean), "<br>",
          "95% CrI: ", sprintf("%.2f", dpdt_lo), " to ", sprintf("%.2f", dpdt_hi), " pp/year"
        )
      ) %>%
      arrange(year)

    growth_points <- survey_interval_growth %>%
      filter(country == country_name, !is.na(obs_growth_pp_per_year)) %>%
      mutate(
        hover = paste0(
          "<b>", country, "</b><br>",
          "Survey interval: ", interval_label, "<br>",
          "Observed annualized growth rate: ", sprintf("%.2f pp/year", obs_growth_pp_per_year), "<br>",
          "Start mCPR: ", format_pct(prev_mcpr, 1), "<br>",
          "End mCPR: ", format_pct(mCPR, 1)
        )
      ) %>%
      arrange(interval_mid_year)

    y_pad <- 0.20
    y_limits <- range(c(growth_curve$dpdt_lo_pp, growth_curve$dpdt_hi_pp, growth_points$obs_growth_pp_per_year), na.rm = TRUE)
    if (!all(is.finite(y_limits))) {
      y_limits <- c(0, 1)
    }
    y_limits <- c(y_limits[1] - y_pad, y_limits[2] + y_pad)

    plot_ly(source = "country_growth_plot") %>%
      add_ribbons(
        data = growth_curve,
        x = ~year,
        ymin = ~dpdt_lo_pp,
        ymax = ~dpdt_hi_pp,
        hoverinfo = "skip",
        line = list(color = "transparent"),
        fillcolor = "rgba(34,197,94,0.18)",
        showlegend = FALSE,
        name = "95% credible interval"
      ) %>%
      add_lines(
        data = growth_curve,
        x = ~year,
        y = ~dpdt_mean_pp,
        text = ~hover,
        hoverinfo = "text",
        line = list(color = "#15803D", width = 3.2),
        showlegend = FALSE,
        name = "Model annualized growth"
      ) %>%
      add_markers(
        data = growth_points,
        x = ~interval_mid_year,
        y = ~obs_growth_pp_per_year,
        text = ~hover,
        hoverinfo = "text",
        marker = list(color = "#7C3AED", size = 9, line = list(color = "white", width = 1)),
        showlegend = FALSE,
        name = "Observed annualized growth"
      ) %>%
      layout(
        title = list(
          text = "Observed and model-based annualized growth<br><sup>Purple points are survey-to-survey annualized growth rates. The green curve and band show the model-implied instantaneous annualized growth rate over time.</sup>",
          x = 0
        ),
        xaxis = list(
          title = "Year",
          tickformat = ".0f",
          hoverformat = ".0f",
          separatethousands = FALSE,
          showgrid = TRUE,
          gridcolor = "rgba(229,231,235,0.9)",
          zeroline = FALSE
        ),
        yaxis = list(
          title = "Annualized growth rate (pp/year)",
          range = y_limits,
          showgrid = TRUE,
          gridcolor = "rgba(229,231,235,0.9)",
          zeroline = FALSE
        ),
        shapes = list(
          list(
            type = "line",
            x0 = 2025,
            x1 = 2025,
            y0 = y_limits[1],
            y1 = y_limits[2],
            line = list(color = "#111827", dash = "dash", width = 1.8)
          )
        ),
        annotations = list(
          list(
            x = 2025,
            y = y_limits[2],
            text = "2025",
            textangle = -90,
            xanchor = "left",
            yanchor = "top",
            showarrow = FALSE,
            font = list(color = "#111827", size = 12)
          )
        ),
        margin = list(l = 70, r = 35, b = 70, t = 82),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "lasso2d",
          "select2d",
          "autoScale2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian"
        )
      )
  })

  output$region_observed_plot <- renderPlotly({
    region_df <- obs_data %>%
      filter(as.character(region) == input$region) %>%
      mutate(
        highlight_flag = if_else(country == input$country, "Selected country", "Other countries"),
        mcpr_pct = 100 * mCPR,
        hover = paste0(
          "<b>", country, "</b><br>",
          "Survey: ", survey_id, "<br>",
          "Year: ", year, "<br>",
          "Observed mCPR: ", format_pct(mCPR, 1)
        )
      ) %>%
      arrange(country, year)

    region_curve_df <- country_curve_summary %>%
      filter(as.character(region) == input$region, year <= input$projection_end_year) %>%
      mutate(
        highlight_flag = if_else(country == input$country, "Selected country", "Other countries"),
        p_mean_pct = 100 * p_mean,
        hover = paste0(
          "<b>", country, "</b><br>",
          "Year: ", year, "<br>",
          "Model mCPR: ", format_pct(p_mean, 1), "<br>",
          "95% CrI: ", format_pct(p_lo, 1), " to ", format_pct(p_hi, 1), "<br>",
          "Region: ", region
        )
      ) %>%
      arrange(country, year)

    other_curve_df <- region_curve_df %>% filter(highlight_flag == "Other countries")
    selected_curve_df <- region_curve_df %>% filter(highlight_flag == "Selected country")
    other_obs_df <- region_df %>% filter(highlight_flag == "Other countries")
    selected_obs_df <- region_df %>% filter(highlight_flag == "Selected country")

    y_max <- max(c(region_curve_df$p_mean_pct, region_df$mcpr_pct), na.rm = TRUE)
    if (!is.finite(y_max)) y_max <- 50
    y_max <- min(100, ceiling(y_max / 5) * 5 + 8)

    plot_ly(source = "region_observed_plot") %>%
      add_lines(
        data = other_curve_df,
        x = ~year,
        y = ~p_mean_pct,
        split = ~country,
        text = ~hover,
        hoverinfo = "text",
        line = list(color = "rgba(203,213,225,0.95)", width = 1.1),
        showlegend = FALSE
      ) %>%
      add_lines(
        data = selected_curve_df,
        x = ~year,
        y = ~p_mean_pct,
        split = ~country,
        text = ~hover,
        hoverinfo = "text",
        line = list(color = "#0F4C81", width = 3.2),
        showlegend = FALSE
      ) %>%
      add_lines(
        data = other_obs_df,
        x = ~year,
        y = ~mcpr_pct,
        split = ~country,
        text = ~hover,
        hoverinfo = "text",
        line = list(color = "rgba(148,163,184,0.75)", width = 1.1),
        showlegend = FALSE
      ) %>%
      add_markers(
        data = other_obs_df,
        x = ~year,
        y = ~mcpr_pct,
        text = ~hover,
        hoverinfo = "text",
        marker = list(color = "rgba(100,116,139,0.85)", size = 5),
        showlegend = FALSE
      ) %>%
      add_lines(
        data = selected_obs_df,
        x = ~year,
        y = ~mcpr_pct,
        split = ~country,
        text = ~hover,
        hoverinfo = "text",
        line = list(color = "#1D4ED8", width = 2.4),
        showlegend = FALSE
      ) %>%
      add_markers(
        data = selected_obs_df,
        x = ~year,
        y = ~mcpr_pct,
        text = ~hover,
        hoverinfo = "text",
        marker = list(color = "#B91C1C", size = 8, line = list(color = "white", width = 0.8)),
        showlegend = FALSE
      ) %>%
      layout(
        title = list(
          text = paste0(
            input$region,
            ": observed DHS and fitted model trajectories",
            "<br><sup>Grey and blue curves are fitted model trajectories; points and thinner line segments show observed DHS values. The selected country is emphasized in darker blue and red.</sup>"
          ),
          x = 0
        ),
        xaxis = list(
          title = "Year",
          tickformat = ".0f",
          hoverformat = ".0f",
          separatethousands = FALSE,
          showgrid = TRUE,
          gridcolor = "rgba(229,231,235,0.9)",
          zeroline = FALSE
        ),
        yaxis = list(
          title = "mCPR (%)",
          range = c(0, y_max),
          showgrid = TRUE,
          gridcolor = "rgba(229,231,235,0.9)",
          zeroline = FALSE
        ),
        margin = list(l = 70, r = 35, b = 70, t = 82),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "lasso2d",
          "select2d",
          "autoScale2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian"
        )
      )
  })

  output$country_validation_plot <- renderPlotly({
    row <- selected_country_row()
    p2025_row <- country_curve_summary %>% filter(country == row$country[[1]], year == 2025) %>% slice(1)
    validation_df <- tibble(
      measure = c("Observed latest survey", "Model p(last observed year)", "Model p(2025)"),
      value = c(
        100 * row$latest_obs_mcpr[[1]],
        100 * row$projection_latest_mean[[1]],
        100 * p2025_row$p_mean[[1]]
      ),
      hover = c(
        paste0("Observed latest survey: ", format_pct(row$latest_obs_mcpr[[1]], 1), " in ", format_year(row$latest_obs_year[[1]])),
        paste0("Model p(last observed year): ", format_pct(row$projection_latest_mean[[1]], 1)),
        paste0("Model p(2025): ", format_pct(p2025_row$p_mean[[1]], 1))
      )
    )

    p <- ggplot(validation_df, aes(x = measure, y = value, fill = measure, text = hover)) +
      geom_col(width = 0.65, alpha = 0.92, show.legend = FALSE) +
      scale_fill_manual(values = c("#B91C1C", "#0F4C81", "#15803D")) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 10, hjust = 1)) +
      labs(
        title = "Quick validation snapshot",
        subtitle = "This is the simplest same-country check before moving into goal or stage interpretation.",
        x = NULL,
        y = "mCPR (%)"
      )

    interactive_plot(p)
  })

  output$region_country_table <- renderDT({
    region_table <- country_catalog %>%
      filter(as.character(region) == input$region) %>%
      left_join(
        country_curve_summary %>%
          filter(year == 2025) %>%
          select(country, p2025_mean = p_mean),
        by = "country"
      ) %>%
      transmute(
        Country = country,
        `Latest survey year` = latest_obs_year,
        `Observed latest mCPR (%)` = round(100 * latest_obs_mcpr, 1),
        `Model p(last obs) (%)` = round(100 * projection_latest_mean, 1),
        `Model p(2025) (%)` = round(100 * p2025_mean, 1),
        `K (%)` = round(100 * K_mean, 1),
        `Stage 2026` = s_curve_stage_2026,
        `Growth 2026 (pp/year)` = round(100 * dpdt_mean, 2),
        Phase = phase
      ) %>%
      arrange(desc(`Growth 2026 (pp/year)`))

    datatable(region_table, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE, autoWidth = TRUE))
  })

  output$ssa_metric_cards <- renderUI({
    region_row <- ssa_region_goals %>% slice(1)
    stage_row <- weighted_stage_summary %>% filter(geography == "Sub-Saharan Africa (weighted)") %>% slice(1)

    div(
      class = "metric-grid",
      metric_card(
        "Weighted latest observed SSA mCPR",
        paste0(sprintf("%.1f", 100 * region_row$last_obs_weighted_mean[[1]]), "%"),
        "Population-weighted over each country's latest observed DHS year",
        "blue"
      ),
      metric_card(
        "Weighted SSA p(2025)",
        paste0(sprintf("%.1f", 100 * region_row$p2025_mean[[1]]), "%"),
        paste0("95% CrI ", sprintf("%.1f", 100 * region_row$p2025_p05[[1]]), " to ", sprintf("%.1f", 100 * region_row$p2025_p95[[1]]), "%"),
        "green"
      ),
      metric_card(
        "Required annualized growth to reach 40%",
        format_pp(region_row$required_growth_from_p2025_pp_per_year[[1]], 2),
        "Calculated from p(2025) to 2035",
        "amber"
      ),
      metric_card(
        "Model annualized growth at 2025",
        format_pp(region_row$model_dpdt_2025_pp_per_year[[1]], 2),
        paste0("Gap versus required: ", sprintf("%.2f pp/year", region_row$model_dpdt_2025_pp_per_year[[1]] - region_row$required_growth_from_p2025_pp_per_year[[1]])),
        "purple"
      ),
      metric_card(
        "Projected weighted SSA crossing year",
        ifelse(is.na(region_row$projected_year_reach_40_p50[[1]]), "No crossing projected", paste0("~", round(region_row$projected_year_reach_40_p50[[1]]))),
        paste0("P(reach 40% by 2035) = ", percent(region_row$prob_reach_40_by_2035[[1]], accuracy = 0.1)),
        "gold"
      ),
      metric_card(
        "Weighted SSA stage at 2026",
        stage_row$stage_label[[1]],
        paste0("Weighted K ", sprintf("%.1f", 100 * stage_row$K_mean[[1]]), "% | p(2026) ", sprintf("%.1f", 100 * stage_row$p2026_mean[[1]]), "%"),
        "slate"
      )
    )
  })

  output$ssa_current_summary_table <- renderDT({
    tab <- ssa_current_summary %>%
      transmute(
        Baseline = baseline,
        `Current mCPR (%)` = round(current_mcpr_pct, 1),
        `Gap to 40% (pp)` = round(gap_to_40_pp, 1),
        `Required annualized growth rate (pp/year)` = round(required_growth_pp_per_year, 2)
      )

    datatable(tab, rownames = FALSE, options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE))
  })

  ssa_filtered_goals <- reactive({
    ssa_country_plot_base %>%
      filter(region %in% input$ssa_region_filter, crossing_status %in% input$ssa_cross_filter)
  })

  ssa_gap_plot_data <- reactive({
    countries <- ssa_filtered_goals()
    baseline <- req(input$ssa_gap_baseline)

    if (baseline == "last_obs") {
      countries %>%
        transmute(
          geography = country,
          region,
          survey_group = survey_recency_group,
          gap_pp = gap_from_lastobs_pp,
          growth_required = required_growth_from_lastobs_pp_per_year,
          women_15_49_millions,
          latest_year = last_obs_year,
          current_value = current_latest_obs_pct
        )
    } else {
      countries %>%
        transmute(
          geography = country,
          region,
          survey_group = survey_recency_group,
          gap_pp = gap_from_p2025_pp,
          growth_required = required_growth_from_p2025_pp_per_year,
          women_15_49_millions,
          latest_year = last_obs_year,
          current_value = current_p2025_pct
        )
    } %>%
      bind_rows(
        tibble(
          geography = "Sub-Saharan Africa (weighted)",
          region = "Sub-Saharan Africa",
          survey_group = "Population-weighted region",
          gap_pp = if (baseline == "last_obs") ssa_region_plot_base$gap_from_lastobs_pp else ssa_region_plot_base$gap_from_p2025_pp,
          growth_required = if (baseline == "last_obs") ssa_region_plot_base$required_growth_from_lastobs_pp_per_year else ssa_region_plot_base$required_growth_from_p2025_pp_per_year,
          women_15_49_millions = ssa_region_plot_base$women_15_49_millions,
          latest_year = NA_real_,
          current_value = if (baseline == "last_obs") ssa_region_plot_base$current_latest_obs_pct else ssa_region_plot_base$current_p2025_pct
        )
      )
  })

  output$ssa_gap_plot <- renderPlotly({
    gap_df <- ssa_gap_plot_data() %>%
      mutate(is_region_row = geography == "Sub-Saharan Africa (weighted)") %>%
      arrange(desc(is_region_row), desc(gap_pp), geography) %>%
      mutate(
        geography_plot = geography,
        hover = paste0(
          "<b>", geography, "</b><br>",
          "Region: ", region, "<br>",
          "Current mCPR: ", sprintf("%.1f%%", current_value), "<br>",
          "Gap to 40%%: ", sprintf("%.1f pp", gap_pp), "<br>",
          "Required annualized growth rate: ", sprintf("%.2f pp/year", growth_required), "<br>",
          "Women 15-49: ", ifelse(is.na(women_15_49_millions), "NA", sprintf("%.1fm", women_15_49_millions))
        )
      )

    title_txt <- if (input$ssa_gap_baseline == "last_obs") {
      "Gap to 40% from the latest observed baseline"
    } else {
      "Gap to 40% from model-estimated p(2025)"
    }

    y_levels <- rev(gap_df$geography_plot)
    x_max <- max(gap_df$gap_pp, na.rm = TRUE)
    x_right <- max(1, x_max * 1.18)

    p <- plot_ly(source = "ssa_gap_plot")

    for (group_name in names(gap_palette)) {
      group_df <- gap_df %>% filter(survey_group == group_name)

      if (nrow(group_df) > 0) {
        p <- p %>%
          add_bars(
            data = group_df,
            x = ~gap_pp,
            y = ~geography_plot,
            orientation = "h",
            hoverinfo = "text",
            text = ~hover,
            marker = list(color = gap_palette[[group_name]]),
            name = group_name
          )
      }
    }

    p %>%
      layout(
        title = list(
          text = paste0(
            "SSA goals: ", title_txt,
            "<br><sup>Bars show the gap to 40%. Hover any bar to see the annualized growth rate required to close that gap in 10 years. The weighted SSA region is always the first reference row.</sup>"
          )
        ),
        barmode = "overlay",
        xaxis = list(
          title = "Gap to 40% (percentage points)",
          range = c(0, x_right),
          zeroline = FALSE
        ),
        yaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = y_levels,
          automargin = TRUE
        ),
        legend = list(orientation = "h", x = 0, y = -0.12),
        margin = list(l = 170, r = 40, b = 70, t = 90),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian")
      )
  })

  output$ssa_feasibility_plot <- renderPlotly({
    country_df <- ssa_filtered_goals() %>%
      mutate(
        hover = paste0(
          "<b>", country, "</b><br>",
          "Region: ", region, "<br>",
          "Required annualized growth to reach 40%: ", sprintf("%.2f pp/year", required_growth_from_p2025_pp_per_year), "<br>",
          "Model annualized growth at 2025: ", sprintf("%.2f pp/year", model_dpdt_2025_pp_per_year), "<br>",
          "Observed latest-interval growth: ", sprintf("%.2f pp/year", latest_obs_interval_growth_pp_per_year), "<br>",
          "K: ", sprintf("%.1f%%", 100 * K_mean), "<br>",
          "Crossing status: ", crossing_status
        ),
        highlight = country %in% input$ssa_highlight_countries
      )

    region_df <- ssa_region_plot_base %>%
      mutate(
        hover = paste0(
          "<b>Sub-Saharan Africa (weighted)</b><br>",
          "Required annualized growth to reach 40%: ", sprintf("%.2f pp/year", required_growth_from_p2025_pp_per_year), "<br>",
          "Model annualized growth at 2025: ", sprintf("%.2f pp/year", model_dpdt_2025_pp_per_year), "<br>",
          "Observed annualized growth: ", sprintf("%.2f pp/year", latest_obs_interval_growth_pp_per_year), "<br>",
          "Weighted K: ", sprintf("%.1f%%", 100 * K_mean), "<br>",
          "P(reach 40% by 2035): ", percent(prob_reach_40_by_2035, accuracy = 0.1)
        )
      )

    limits <- range(
      c(
        country_df$required_growth_from_p2025_pp_per_year,
        country_df$model_dpdt_2025_pp_per_year,
        region_df$required_growth_from_p2025_pp_per_year,
        region_df$model_dpdt_2025_pp_per_year
      ),
      na.rm = TRUE
    )
    pad <- 0.30
    limits <- c(max(0, limits[1] - pad), limits[2] + pad)

    p <- ggplot() +
      geom_polygon(
        data = tibble(x = c(limits[1], limits[1], limits[2]), y = c(limits[1], limits[2], limits[2])),
        aes(x = x, y = y),
        inherit.aes = FALSE,
        fill = "#ECFDF5",
        alpha = 0.42
      ) +
      geom_polygon(
        data = tibble(x = c(limits[1], limits[2], limits[2]), y = c(limits[1], limits[1], limits[2])),
        aes(x = x, y = y),
        inherit.aes = FALSE,
        fill = "#FFF7ED",
        alpha = 0.40
      ) +
      geom_abline(intercept = 0, slope = 1, color = "#111827", linewidth = 0.9, linetype = "dashed") +
      geom_point(
        data = country_df,
        aes(
          x = required_growth_from_p2025_pp_per_year,
          y = model_dpdt_2025_pp_per_year,
          fill = feasibility_status,
          shape = latest_obs_status,
          size = women_15_49_millions,
          text = hover
        ),
        color = "white",
        stroke = 0.45,
        alpha = 0.95
      ) +
      geom_point(
        data = region_df,
        aes(x = required_growth_from_p2025_pp_per_year, y = model_dpdt_2025_pp_per_year, text = hover),
        shape = 23,
        size = 5,
        fill = "white",
        color = "grey15",
        stroke = 0.5
      ) +
      ggrepel::geom_text_repel(
        data = country_df,
        aes(
          x = required_growth_from_p2025_pp_per_year,
          y = model_dpdt_2025_pp_per_year,
          label = country
        ),
        seed = 2025,
        size = 3.0,
        color = "#111827",
        fontface = "bold",
        segment.color = "grey55",
        segment.alpha = 0.65,
        segment.size = 0.28,
        min.segment.length = 0,
        box.padding = 0.38,
        point.padding = 0.24,
        force = 1.35,
        force_pull = 0.12,
        max.overlaps = Inf,
        show.legend = FALSE
      ) +
      ggrepel::geom_label_repel(
        data = region_df,
        aes(
          x = required_growth_from_p2025_pp_per_year,
          y = model_dpdt_2025_pp_per_year,
          label = "SSA weighted"
        ),
        seed = 2025,
        size = 3.6,
        color = "#111827",
        fontface = "bold",
        segment.color = "grey40",
        segment.alpha = 0.75,
        segment.size = 0.34,
        min.segment.length = 0,
        box.padding = 0.55,
        point.padding = 0.32,
        force = 1.6,
        force_pull = 0.14,
        max.overlaps = Inf,
        fill = "white",
        label.size = 0.22,
        label.r = grid::unit(0.14, "lines"),
        show.legend = FALSE
      ) +
      annotate(
        "label",
        x = limits[1] + 0.08,
        y = limits[2] - 0.04,
        label = "Above diagonal:\ncurrent model annualized growth rate exceeds required",
        hjust = 0,
        vjust = 1,
        size = 3.2,
        fill = "#ECFDF5",
        label.size = 0.2,
        color = "#14532D"
      ) +
      annotate(
        "label",
        x = limits[2] - 0.04,
        y = limits[1] + 0.32,
        label = "Below diagonal:\nacceleration is needed",
        hjust = 1,
        vjust = 0,
        size = 3.2,
        fill = "#FFF7ED",
        label.size = 0.2,
        color = "#9A3412"
      ) +
      scale_fill_manual(
        values = feasibility_palette,
        breaks = c(
          "Current annualized growth rate above required",
          "Below required annualized growth rate, K above 40%",
          "Below required annualized growth rate, K spans 40%",
          "K below 40%"
        ),
        name = "Current annualized growth rate / K status"
      ) +
      scale_shape_manual(
        values = c("Already at or above 40%" = 24, "Below 40% at latest observed" = 21),
        name = "Latest observed status"
      ) +
      scale_size_area(max_size = 14, guide = "none") +
      scale_x_continuous(limits = limits, expand = expansion(mult = 0)) +
      scale_y_continuous(limits = limits, expand = expansion(mult = 0)) +
      coord_cartesian(xlim = limits, ylim = limits, clip = "off") +
      theme_minimal(base_size = 11.4) +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = margin(0, 0, 0, 0),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
        plot.title.position = "plot",
        plot.margin = margin(16, 130, 22, 18)
      ) +
      guides(
        fill = guide_legend(
          order = 1,
          override.aes = list(shape = 21, size = 4.5, color = "grey15", alpha = 1)
        ),
        shape = guide_legend(
          order = 2,
          override.aes = list(size = 4.5, fill = "grey55", color = "grey15", alpha = 1)
        )
      ) +
      labs(
        title = "Goals - final: required annualized growth rate versus model-implied current annualized growth rate",
        subtitle = "Every country is labeled; the weighted SSA region is the diamond. Points above the diagonal are on a faster model-implied annualized growth rate than the annualized growth rate needed to reach 40% by 2035. Triangle markers indicate countries already at or above 40% at their latest observed survey.",
        x = "Required annual growth from p(2025) to reach 40% by 2035 (pp/year)",
        y = "Model-implied annual growth at 2025 (pp/year)"
      )

    interactive_plot(p) %>%
      layout(
        margin = list(l = 95, r = 220, b = 95, t = 90),
        legend = list(orientation = "h", x = 0, y = -0.18)
      )
  })

  output$ssa_growth_benchmark_table <- renderDT({
    benchmark_df <- ssa_filtered_goals() %>%
      left_join(
        country_curve_summary %>%
          filter(year == goal_year) %>%
          select(country, p2035_mean = p_mean),
        by = "country"
      ) %>%
      transmute(
        Geography = country,
        Region = region,
        `Observed annualized growth rate (pp/year)` = round(latest_obs_interval_growth_pp_per_year, 2),
        `Model annualized growth at 2025 (pp/year)` = round(model_dpdt_2025_pp_per_year, 2),
        `Projected annualized growth, 2025-2035 (pp/year)` = round(100 * (p2035_mean - p2025_mean) / 10, 2),
        `Required annualized growth to reach 40% (pp/year)` = round(required_growth_from_p2025_pp_per_year, 2),
        `Crossing status` = crossing_status
      ) %>%
      arrange(factor(Region, levels = ssa_regions), desc(`Required annualized growth to reach 40% (pp/year)`))

    datatable(benchmark_df, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })

  output$ssa_survey_interval_growth_table <- renderDT({
    interval_df <- survey_interval_growth %>%
      filter(country %in% country_names_ssa, region %in% input$ssa_region_filter, !is.na(obs_growth_pp_per_year)) %>%
      transmute(
        Country = country,
        Region = region,
        `Survey interval` = interval_label,
        `Interval length (years)` = interval_years,
        `Start mCPR (%)` = round(100 * prev_mcpr, 1),
        `End mCPR (%)` = round(100 * mCPR, 1),
        `Observed annualized growth rate (pp/year)` = round(obs_growth_pp_per_year, 2)
      ) %>%
      arrange(Country, `Survey interval`)

    datatable(interval_df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$ssa_model_growth_history_table <- renderDT({
    hist_df <- ssa_growth_history_table %>%
      filter(region %in% c(input$ssa_region_filter, "Sub-Saharan Africa")) %>%
      transmute(
        Geography = geography,
        Region = region,
        Year = year,
        `Model annualized growth rate (pp/year)` = round(model_annualized_growth_pp_per_year, 2),
        `95% CrI (pp/year)` = cri
      )

    datatable(hist_df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  ssa_trajectory_filter <- reactive({
    ssa_filtered_goals() %>%
      select(country, region, crossing_status, women_15_49_millions, K_mean, p2025_mean, projected_year_reach_40_p50)
  })

  output$ssa_all_trajectories_plot <- renderPlotly({
    filtered_meta <- ssa_trajectory_filter()
    curve_df <- country_curve_summary %>%
      filter(country %in% filtered_meta$country, year <= input$ssa_plot_end_year) %>%
      left_join(filtered_meta, by = c("country", "region")) %>%
      mutate(
        highlight = country %in% input$ssa_highlight_countries,
        hover = paste0(
          "<b>", country, "</b><br>",
          "Region: ", region, "<br>",
          "Year: ", year, "<br>",
          "Model mCPR: ", sprintf("%.1f%%", 100 * p_mean), "<br>",
          "K: ", sprintf("%.1f%%", 100 * K_mean), "<br>",
          "p(2025): ", sprintf("%.1f%%", 100 * p2025_mean), "<br>",
          "Crossing status: ", crossing_status
        )
      )

    country_obs <- obs_data %>%
      filter(country %in% filtered_meta$country) %>%
      transmute(country, region, year, mcp_pct = 100 * mCPR)

    region_curve <- weighted_curve_summary %>%
      filter(geography == "Sub-Saharan Africa (weighted)", year <= input$ssa_plot_end_year) %>%
      arrange(year) %>%
      mutate(
        hover = paste0(
          "<b>Sub-Saharan Africa (weighted)</b><br>",
          "Year: ", year, "<br>",
          "Weighted model mCPR: ", sprintf("%.1f%%", 100 * p_mean)
        )
      )

    region_obs <- weighted_observed_summary %>%
      filter(geography == "Sub-Saharan Africa (weighted)", year <= input$ssa_plot_end_year) %>%
      transmute(year, mcp_pct = 100 * observed_weighted_mcpr)

    p <- ggplot() +
      geom_hline(yintercept = 40, color = "#7F1D1D", linetype = "dashed", linewidth = 0.75) +
      geom_vline(xintercept = goal_year, color = "grey25", linetype = "longdash", linewidth = 0.75) +
      geom_point(
        data = country_obs,
        aes(x = year, y = mcp_pct),
        inherit.aes = FALSE,
        color = "black",
        size = 0.95,
        alpha = 0.35
      ) +
      geom_line(
        data = curve_df %>% filter(!highlight),
        aes(x = year, y = 100 * p_mean, group = country, color = crossing_status, text = hover),
        linewidth = 0.55,
        alpha = 0.45
      ) +
      geom_line(
        data = curve_df %>% filter(highlight),
        aes(x = year, y = 100 * p_mean, group = country, color = crossing_status, text = hover),
        linewidth = 1.15,
        alpha = 0.95
      ) +
      geom_line(
        data = region_curve,
        aes(x = year, y = 100 * p_mean, text = hover),
        color = "#111827",
        linewidth = 1.8
      ) +
      geom_point(
        data = region_obs,
        aes(x = year, y = mcp_pct),
        inherit.aes = FALSE,
        color = "#111827",
        size = 1.55,
        alpha = 0.82
      ) +
      scale_color_manual(values = crossing_palette, name = NULL) +
      theme_minimal(base_size = 11.4) +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom", plot.title.position = "plot") +
      labs(
        title = "Goals - final: weighted SSA region first, then every country trajectory relative to 40%",
        subtitle = "The thick black line is the weighted SSA regional trajectory and black points are observed values. Country colors encode whether the geography already crossed, is projected to cross by 2035, is projected to cross later, or is not projected to cross.",
        x = "Year",
        y = "Model mCPR (%)"
      )

    interactive_plot(p, x_year = TRUE)
  })

  output$ssa_notyet_plot <- renderPlotly({
    filtered_meta <- ssa_trajectory_filter() %>%
      filter(crossing_status != "Already crossed")

    curve_df <- country_curve_summary %>%
      filter(country %in% filtered_meta$country, year >= 2025, year <= input$ssa_plot_end_year) %>%
      left_join(filtered_meta, by = c("country", "region")) %>%
      mutate(
        hover = paste0(
          "<b>", country, "</b><br>",
          "Region: ", region, "<br>",
          "Year: ", year, "<br>",
          "Model mCPR: ", sprintf("%.1f%%", 100 * p_mean), "<br>",
          "Crossing status: ", crossing_status, "<br>",
          "Projected crossing year: ", ifelse(is.na(projected_year_reach_40_p50), "Not projected", round(projected_year_reach_40_p50))
        )
      )

    label_df <- curve_df %>%
      group_by(country, region, crossing_status, projected_year_reach_40_p50) %>%
      slice_max(year, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(
        projection_label = case_when(
          crossing_status == "Not projected to cross" ~ paste0(country, " (Not projected to cross)"),
          crossing_status == "Projected to cross by 2035" ~ paste0(country, " (by 2035: ~", round(projected_year_reach_40_p50), ")"),
          TRUE ~ paste0(country, " (after 2035: ~", round(projected_year_reach_40_p50), ")")
        )
      )

    x_right <- max(2060, input$ssa_plot_end_year + 6)

    p <- ggplot(curve_df, aes(x = year, y = 100 * p_mean, group = country, color = crossing_status, text = hover)) +
      geom_line(linewidth = 0.9, alpha = 0.8) +
      geom_hline(yintercept = 40, color = "#7F1D1D", linetype = "dashed", linewidth = 0.75) +
      geom_vline(xintercept = goal_year, color = "grey25", linetype = "longdash", linewidth = 0.75) +
      ggrepel::geom_label_repel(
        data = label_df,
        aes(x = year, y = 100 * p_mean, label = projection_label, color = crossing_status),
        inherit.aes = FALSE,
        direction = "y",
        hjust = 0,
        nudge_x = 2.8,
        size = 2.7,
        segment.alpha = 0.38,
        segment.size = 0.28,
        min.segment.length = 0,
        box.padding = 0.28,
        point.padding = 0.2,
        label.size = 0.16,
        label.padding = grid::unit(0.1, "lines"),
        label.r = grid::unit(0.12, "lines"),
        fill = "white",
        fontface = "bold",
        max.overlaps = Inf,
        show.legend = FALSE
      ) +
      scale_color_manual(values = crossing_palette[c("Projected to cross by 2035", "Projected to cross after 2035", "Not projected to cross")], name = NULL) +
      theme_minimal(base_size = 11.4) +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title.position = "plot",
        plot.margin = margin(5.5, 80, 5.5, 5.5)
      ) +
      coord_cartesian(xlim = c(2025, x_right), ylim = c(0, 100), clip = "off") +
      labs(
        title = "Goals - final: projected trajectories for countries still below 40% at last observation",
        subtitle = "Boxed labels distinguish countries projected to cross by 2035, projected to cross later, and countries not projected to cross under the fitted trajectory.",
        x = "Year",
        y = "Model mCPR (%)"
      )

    interactive_plot(p, x_year = TRUE)
  })

  output$ssa_already_crossed_plot <- renderPlotly({
    already_df <- ssa_country_plot_base %>%
      filter(crossing_status == "Already crossed", region %in% input$ssa_region_filter)

    already_colors <- setNames(
      grDevices::hcl.colors(max(nrow(already_df), 1), palette = "Dark 3")[seq_len(max(nrow(already_df), 1))],
      if (nrow(already_df) > 0) already_df$country else "placeholder"
    )
    if (nrow(already_df) == 0) {
      already_colors <- c()
    }

    curve_df <- country_curve_summary %>%
      filter(country %in% already_df$country, year <= min(2045, input$ssa_plot_end_year)) %>%
      left_join(already_df %>% select(country, region), by = c("country", "region")) %>%
      mutate(
        hover = paste0(
          "<b>", country, "</b><br>",
          "Region: ", region, "<br>",
          "Year: ", year, "<br>",
          "Model mCPR: ", sprintf("%.1f%%", 100 * p_mean)
        )
      )

    obs_df <- obs_data %>%
      filter(country %in% already_df$country) %>%
      transmute(country, year, mcp_pct = 100 * mCPR)

    cross_points <- obs_data %>%
      filter(country %in% already_df$country, mCPR >= target_mcpr) %>%
      group_by(country) %>%
      slice_min(year, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(
        country,
        year,
        mcp_pct = 100 * mCPR,
        cross_label = paste0(country, " crossed ", year)
      )

    p <- ggplot(curve_df, aes(x = year, y = 100 * p_mean, group = country, color = country, text = hover)) +
      geom_line(linewidth = 1.0, alpha = 0.9, show.legend = FALSE) +
      geom_point(
        data = obs_df,
        aes(x = year, y = mcp_pct, color = country),
        inherit.aes = FALSE,
        size = 2,
        alpha = 0.8,
        show.legend = FALSE
      ) +
      geom_hline(yintercept = 40, color = "#7F1D1D", linetype = "dashed", linewidth = 0.75) +
      geom_point(
        data = cross_points,
        aes(x = year, y = mcp_pct, fill = country),
        inherit.aes = FALSE,
        shape = 21,
        size = 3,
        stroke = 0.5,
        color = "white",
        show.legend = FALSE
      ) +
      ggrepel::geom_text_repel(
        data = cross_points,
        aes(x = year, y = mcp_pct, label = cross_label, color = country),
        inherit.aes = FALSE,
        size = 3,
        segment.alpha = 0.35,
        min.segment.length = 0,
        max.overlaps = Inf,
        show.legend = FALSE
      ) +
      scale_color_manual(values = already_colors) +
      scale_fill_manual(values = already_colors) +
      coord_cartesian(ylim = c(0, 70)) +
      theme_minimal(base_size = 11.2) +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
      labs(
        title = "Goals - final: countries already above 40% at their latest observed survey",
        subtitle = "Crossing labels use the first observed DHS survey year at or above 40%; fitted lines show the broader trajectory around that threshold.",
        x = "Year",
        y = "Model mCPR (%)"
      )

    interactive_plot(p, x_year = TRUE)
  })

  output$ssa_asymptote_plot <- renderPlotly({
    plot_df <- ssa_filtered_goals() %>%
      arrange(desc(K_mean), country) %>%
      mutate(
        y = rev(seq_len(n())),
        hover_current = paste0(
          "<b>", country, "</b><br>",
          "Region: ", region, "<br>",
          "p(2025): ", sprintf("%.1f%%", 100 * p2025_mean), "<br>",
          "Fitted K: ", sprintf("%.1f%%", 100 * K_mean)
        ),
        hover_k = paste0(
          "<b>", country, "</b><br>",
          "Region: ", region, "<br>",
          "Fitted K: ", sprintf("%.1f%%", 100 * K_mean), "<br>",
          "95% CrI: ", sprintf("%.1f", 100 * K_p05), " to ", sprintf("%.1f", 100 * K_p95), "%"
        )
      )

    plot_ly(source = "ssa_asymptote") %>%
      add_segments(
        data = plot_df,
        x = ~100 * K_p05,
        xend = ~100 * K_p95,
        y = ~y,
        yend = ~y,
        hoverinfo = "text",
        text = ~hover_k,
        line = list(color = "#8A5A00", width = 2),
        showlegend = FALSE
      ) %>%
      add_markers(
        data = plot_df,
        x = ~100 * K_mean,
        y = ~y,
        hoverinfo = "text",
        text = ~hover_k,
        marker = list(symbol = "diamond", size = 11, color = "#FCD34D", line = list(color = "#8A5A00", width = 1.2)),
        name = "K"
      ) %>%
      add_markers(
        data = plot_df,
        x = ~100 * p2025_mean,
        y = ~y,
        hoverinfo = "text",
        text = ~hover_current,
        marker = list(symbol = "circle", size = 8, color = "#0F4C81"),
        name = "p(2025)"
      ) %>%
      layout(
        title = list(
          text = "Model-estimated p(2025) versus fitted K<br><sup>Countries are ordered from highest fitted K to lowest. Solid dots are p(2025), diamonds are fitted K, and the horizontal spans show the 95% credible interval around K. The vertical dashed line marks the 40% goal.</sup>"
        ),
        xaxis = list(title = "mCPR (%)", zeroline = FALSE),
        yaxis = list(
          title = "",
          tickmode = "array",
          tickvals = plot_df$y,
          ticktext = plot_df$country,
          automargin = TRUE
        ),
        shapes = list(
          list(
            type = "line",
            x0 = 40, x1 = 40,
            y0 = min(plot_df$y) - 1, y1 = max(plot_df$y) + 1,
            line = list(color = "#7F1D1D", dash = "dash", width = 2)
          )
        ),
        legend = list(orientation = "h", x = 0, y = -0.12),
        margin = list(l = 140, r = 30, b = 60, t = 80),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian")
      )
  })

  output$ssa_region_stage_plot <- renderPlotly({
    region_curve <- weighted_curve_summary %>%
      filter(geography == "Sub-Saharan Africa (weighted)", year <= input$ssa_plot_end_year) %>%
      mutate(
        hover = paste0(
          "<b>Sub-Saharan Africa (weighted)</b><br>",
          "Year: ", year, "<br>",
          "Model mCPR: ", sprintf("%.1f%%", 100 * p_mean), "<br>",
          "95% CrI: ", sprintf("%.1f", 100 * p_lo), " to ", sprintf("%.1f", 100 * p_hi), "%"
        )
      )
    region_obs <- weighted_observed_summary %>%
      filter(geography == "Sub-Saharan Africa (weighted)") %>%
      mutate(
        hover = paste0(
          "<b>Observed weighted SSA</b><br>",
          "Year: ", year, "<br>",
          "Observed weighted mCPR: ", sprintf("%.1f%%", 100 * observed_weighted_mcpr), "<br>",
          "Countries observed: ", n_countries_obs
        )
      )
    region_stage <- weighted_stage_summary %>%
      filter(geography == "Sub-Saharan Africa (weighted)") %>%
      slice(1)

    stage_note <- paste0(
      "Stage at 2026: ", region_stage$stage_label[[1]],
      "\nWeighted K: ", round(100 * region_stage$K_mean[[1]], 1), "%",
      "\nWeighted p(2026): ", round(100 * region_stage$p2026_mean[[1]], 1), "%",
      "\nRelative saturation p/K: ", round(100 * region_stage$rel_sat_mean[[1]], 1), "%",
      "\nInflection year: ", round(region_stage$inflection_p50[[1]], 1),
      " (95% CrI ", round(region_stage$inflection_lo[[1]], 1),
      "-", round(region_stage$inflection_hi[[1]], 1), ")"
    )
    label_x <- max(region_curve$year, na.rm = TRUE) - 1.2
    note_x <- min(region_curve$year, na.rm = TRUE) + 3
    line_guide_x <- min(region_curve$year, na.rm = TRUE) + 2.2
    cross_x <- region_stage$crossing_p50[[1]]
    cross_label <- if (is.na(cross_x)) {
      "No weighted regional crossing projected"
    } else {
      paste0("Weighted crossing year: ~", round(cross_x, 1))
    }

    p <- ggplot(region_curve, aes(x = year, y = 100 * p_mean, text = hover, group = 1)) +
      geom_ribbon(aes(ymin = 100 * p_lo, ymax = 100 * p_hi, group = 1), fill = stage_palette[[region_stage$stage_label[[1]]]], alpha = 0.12) +
      geom_line(aes(group = 1), color = stage_palette[[region_stage$stage_label[[1]]]], linewidth = 1.9) +
      geom_point(
        data = region_obs,
        aes(x = year, y = 100 * observed_weighted_mcpr, text = hover),
        inherit.aes = FALSE,
        color = "#111827",
        fill = "#111827",
        shape = 21,
        size = 1.9,
        stroke = 0.3
      ) +
      geom_point(
        aes(x = decision_year, y = 100 * region_stage$p2026_mean[[1]]),
        inherit.aes = FALSE,
        shape = 21,
        size = 3.5,
        fill = "white",
        color = stage_palette[[region_stage$stage_label[[1]]]],
        stroke = 0.8
      ) +
      geom_hline(yintercept = 40, color = "#C2410C", linetype = "longdash", linewidth = 0.9) +
      geom_hline(yintercept = 100 * region_stage$K_mean[[1]], color = "#8A5A00", linetype = "dotted", linewidth = 0.95) +
      geom_vline(xintercept = region_stage$inflection_p50[[1]], color = "#1D4ED8", linetype = "dotted", linewidth = 0.85) +
      geom_vline(xintercept = goal_year, color = "grey25", linetype = "longdash", linewidth = 0.85) +
      {
        if (!is.na(region_stage$crossing_p50[[1]]) && is.finite(region_stage$crossing_p50[[1]])) {
          geom_vline(xintercept = region_stage$crossing_p50[[1]], color = "#7C3AED", linetype = "dotdash", linewidth = 0.95)
        }
      } +
      annotate("label", x = label_x, y = 41.5, label = "40% goal", hjust = 1, size = 4.1, fontface = "bold", fill = "#FFF7ED", label.size = 0.25, color = "#C2410C") +
      annotate("label", x = label_x, y = 100 * region_stage$K_mean[[1]] + 1.6, label = paste0("Weighted K: ", sprintf("%.1f%%", 100 * region_stage$K_mean[[1]])), hjust = 1, size = 4.1, fontface = "bold", fill = "#FFFBEB", label.size = 0.25, color = "#8A5A00") +
      annotate(
        "label",
        x = line_guide_x,
        y = 6.5,
        label = "Horizontal lines:\nOrange dashed = 40% goal\nOchre dotted = weighted K",
        hjust = 0,
        vjust = 0,
        size = 3.3,
        label.size = 0.2,
        fill = "white",
        color = "grey15"
      ) +
      {
        if (!is.na(cross_x) && is.finite(cross_x)) {
          annotate(
            "label",
            x = cross_x,
            y = 8,
            label = cross_label,
            angle = 90,
            hjust = 0,
            vjust = -0.15,
            size = 3.6,
            fontface = "bold",
            label.size = 0.25,
            fill = "#F5F3FF",
            color = "#7C3AED"
          )
        }
      } +
      annotate(
        "label",
        x = note_x,
        y = min(98, 100 * region_stage$K_mean[[1]] + 8),
        label = stage_note,
        hjust = 0,
        vjust = 1,
        size = 3.6,
        label.size = 0.2,
        fill = "white",
        color = "grey15"
      ) +
      scale_x_continuous(
        breaks = sort(unique(c(
          seq(
            floor(min(region_curve$year, na.rm = TRUE) / 5) * 5,
            ceiling(max(region_curve$year, na.rm = TRUE) / 5) * 5,
            by = 5
          ),
          2035
        )))
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.02, 0.10))) +
      coord_cartesian(clip = "off") +
      theme_minimal(base_size = 12.3) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey88", linewidth = 0.35),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.35),
        axis.line.x = element_line(color = "grey20", linewidth = 0.6),
        axis.line.y = element_line(color = "grey20", linewidth = 0.6),
        axis.ticks.x = element_line(color = "grey20", linewidth = 0.5),
        axis.ticks.y = element_line(color = "grey20", linewidth = 0.5),
        axis.title = element_text(size = 13.5, face = "bold"),
        axis.text = element_text(size = 11.5, color = "grey15"),
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 11.4, lineheight = 1.08),
        plot.title.position = "plot"
      ) +
      labs(
        title = "Weighted SSA region on the S curve at 2026",
        subtitle = "This is the population-weighted SSA regional mCPR implied by the fitted country model. Black dots are population-weighted observed SSA regional mCPR values; the colored horizontal lines mark the 40% goal and weighted K, and the vertical lines mark the inflection year, 2035 horizon, and weighted regional crossing year.",
        x = "Year",
        y = "Population-weighted model mCPR (%)"
      )

    interactive_plot(p, x_year = TRUE)
  })

  stage_focus_countries <- reactive({
    req(input$stage_panel_region)

    if (identical(input$stage_panel_region, "All SSA")) {
      country_names_ssa
    } else {
      country_catalog %>%
        filter(is_ssa, as.character(region) == input$stage_panel_region) %>%
        pull(country) %>%
        sort()
    }
  })

  output$ssa_country_stage_selector_ui <- renderUI({
    choices <- stage_focus_countries()
    req(length(choices) > 0)

    default_country <- if ("Nigeria" %in% choices) "Nigeria" else choices[[1]]

    selectizeInput(
      "stage_focus_country",
      "Focused country S-curve",
      choices = choices,
      selected = default_country,
      multiple = FALSE,
      options = list(placeholder = "Choose one SSA country")
    )
  })

  output$ssa_country_stage_focus_plot <- renderPlotly({
    req(input$stage_focus_country, input$ssa_plot_end_year)

    meta_row <- ssa_country_plot_base %>%
      filter(country == input$stage_focus_country) %>%
      slice(1)
    req(nrow(meta_row) == 1)

    curve_df <- country_curve_summary %>%
      filter(country == input$stage_focus_country, year <= input$ssa_plot_end_year) %>%
      arrange(year) %>%
      mutate(
        p_mean_pct = 100 * p_mean,
        p_lo_pct = 100 * p_lo,
        p_hi_pct = 100 * p_hi,
        hover = paste0(
          "<b>", country, "</b><br>",
          "Region: ", region, "<br>",
          "Year: ", year, "<br>",
          "Model mCPR: ", sprintf("%.1f%%", 100 * p_mean), "<br>",
          "95% CrI: ", sprintf("%.1f", 100 * p_lo), " to ", sprintf("%.1f", 100 * p_hi), "%<br>",
          "Stage at 2026: ", meta_row$s_curve_stage_2026[[1]]
        )
      )

    obs_df <- obs_data %>%
      filter(country == input$stage_focus_country) %>%
      arrange(year) %>%
      mutate(
        mCPR_pct = 100 * mCPR,
        hover = paste0(
          "<b>", country, "</b><br>",
          "Survey: ", survey_id, "<br>",
          "Year: ", year, "<br>",
          "Observed mCPR: ", sprintf("%.1f%%", 100 * mCPR)
        )
      )

    stage_color <- stage_palette[[meta_row$s_curve_stage_2026[[1]]]]
    y_max <- min(
      100,
      ceiling(max(c(
        40,
        100 * meta_row$K_p95[[1]],
        curve_df$p_hi_pct,
        obs_df$mCPR_pct
      ), na.rm = TRUE) / 5) * 5 + 8
    )

    crossing_year <- meta_row$projected_year_reach_40_p50[[1]]
    crossing_text <- if (is.na(crossing_year)) {
      "No crossing projected"
    } else if (meta_row$last_obs_mcpr[[1]] >= target_mcpr) {
      paste0("Already crossed before ", round(crossing_year))
    } else {
      paste0("Crossing year ~", round(crossing_year))
    }

    stage_note <- paste0(
      meta_row$s_curve_stage_2026[[1]], "\n",
      "K: ", sprintf("%.1f%%", 100 * meta_row$K_mean[[1]]), " (95% CrI ",
      sprintf("%.1f", 100 * meta_row$K_p05[[1]]), "-", sprintf("%.1f", 100 * meta_row$K_p95[[1]]), "%)\n",
      "p(2026): ", sprintf("%.1f%%", 100 * meta_row$p2026_mean[[1]]), "\n",
      "Inflection year ~", round(meta_row$t0_year_mean[[1]]), "\n",
      crossing_text
    )

    p <- plot_ly(source = "ssa_country_stage_focus_plot") %>%
      add_ribbons(
        data = curve_df,
        x = ~year,
        ymin = ~p_lo_pct,
        ymax = ~p_hi_pct,
        hoverinfo = "skip",
        line = list(color = "transparent"),
        fillcolor = paste0(grDevices::adjustcolor(stage_color, alpha.f = 0.16)),
        showlegend = FALSE,
        name = "95% credible interval"
      ) %>%
      add_lines(
        data = curve_df,
        x = ~year,
        y = ~p_mean_pct,
        text = ~hover,
        hoverinfo = "text",
        line = list(color = stage_color, width = 4),
        showlegend = FALSE,
        name = "Fitted model"
      ) %>%
      add_markers(
        data = obs_df,
        x = ~year,
        y = ~mCPR_pct,
        text = ~hover,
        hoverinfo = "text",
        marker = list(color = "#111827", size = 8, line = list(color = "white", width = 0.8)),
        showlegend = FALSE,
        name = "Observed DHS"
      )

    shape_list <- list(
      list(
        type = "line",
        x0 = min(curve_df$year, na.rm = TRUE),
        x1 = max(curve_df$year, na.rm = TRUE),
        y0 = 40,
        y1 = 40,
        line = list(color = "#C2410C", dash = "dash", width = 2.2)
      ),
      list(
        type = "line",
        x0 = min(curve_df$year, na.rm = TRUE),
        x1 = max(curve_df$year, na.rm = TRUE),
        y0 = 100 * meta_row$K_mean[[1]],
        y1 = 100 * meta_row$K_mean[[1]],
        line = list(color = "#8A5A00", dash = "dot", width = 2.2)
      ),
      list(
        type = "line",
        x0 = meta_row$t0_year_mean[[1]],
        x1 = meta_row$t0_year_mean[[1]],
        y0 = 0,
        y1 = y_max,
        line = list(color = "#1D4ED8", dash = "dot", width = 2)
      ),
      list(
        type = "line",
        x0 = goal_year,
        x1 = goal_year,
        y0 = 0,
        y1 = y_max,
        line = list(color = "#374151", dash = "dash", width = 1.8)
      )
    )

    annotation_list <- list(
      list(
        x = max(curve_df$year, na.rm = TRUE) - 0.5,
        y = 40,
        text = "40% goal",
        xanchor = "right",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(color = "#C2410C", size = 13)
      ),
      list(
        x = max(curve_df$year, na.rm = TRUE) - 0.5,
        y = 100 * meta_row$K_mean[[1]],
        text = paste0("K: ", sprintf("%.1f%%", 100 * meta_row$K_mean[[1]])),
        xanchor = "right",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(color = "#8A5A00", size = 13)
      ),
      list(
        x = meta_row$t0_year_mean[[1]],
        y = y_max - 1.5,
        text = paste0("Inflection ~", round(meta_row$t0_year_mean[[1]])),
        textangle = -90,
        xanchor = "left",
        yanchor = "top",
        showarrow = FALSE,
        font = list(color = "#1D4ED8", size = 12)
      ),
      list(
        x = goal_year,
        y = y_max - 1.5,
        text = "2035",
        textangle = -90,
        xanchor = "left",
        yanchor = "top",
        showarrow = FALSE,
        font = list(color = "#374151", size = 12)
      ),
      list(
        x = min(curve_df$year, na.rm = TRUE) + 3,
        y = y_max - 2,
        text = gsub("\n", "<br>", stage_note),
        xanchor = "left",
        yanchor = "top",
        align = "left",
        showarrow = FALSE,
        bgcolor = "rgba(255,255,255,0.92)",
        bordercolor = "rgba(148,163,184,0.7)",
        borderwidth = 1,
        font = list(color = "#111827", size = 12)
      ),
      list(
        x = min(curve_df$year, na.rm = TRUE) + 3,
        y = 6.5,
        text = "Horizontal lines:<br>Orange dashed = 40% goal<br>Ochre dotted = fitted K",
        xanchor = "left",
        yanchor = "bottom",
        align = "left",
        showarrow = FALSE,
        bgcolor = "rgba(255,255,255,0.92)",
        bordercolor = "rgba(148,163,184,0.7)",
        borderwidth = 1,
        font = list(color = "#111827", size = 12)
      )
    )

    if (!is.na(crossing_year) && is.finite(crossing_year) && meta_row$last_obs_mcpr[[1]] < target_mcpr) {
      shape_list <- append(
        shape_list,
        list(
          list(
            type = "line",
            x0 = crossing_year,
            x1 = crossing_year,
            y0 = 0,
            y1 = y_max,
            line = list(color = "#7C3AED", dash = "dotdash", width = 2)
          )
        )
      )
      annotation_list <- append(
        annotation_list,
        list(
          list(
            x = crossing_year,
            y = 8,
            text = paste0("Crossing ~", round(crossing_year)),
            textangle = -90,
            xanchor = "left",
            yanchor = "bottom",
            showarrow = FALSE,
            font = list(color = "#7C3AED", size = 12)
          )
        )
      )
    }

    p %>%
      layout(
        title = list(
          text = paste0(
            input$stage_focus_country,
            " on the S curve",
            "<br><sup>The fitted line and ribbon use the same posterior summaries as the report. Observed DHS points are overlaid for direct fit checking.</sup>"
          ),
          x = 0
        ),
        xaxis = list(
          title = "Year",
          tickformat = ".0f",
          hoverformat = ".0f",
          separatethousands = FALSE,
          showline = TRUE,
          linecolor = "rgba(31,41,55,1)",
          linewidth = 1.2,
          ticks = "outside",
          tickcolor = "rgba(31,41,55,1)",
          showgrid = TRUE,
          gridcolor = "rgba(229,231,235,0.9)",
          zeroline = FALSE
        ),
        yaxis = list(
          title = "Model mCPR (%)",
          range = c(0, y_max),
          showgrid = TRUE,
          gridcolor = "rgba(229,231,235,0.9)",
          zeroline = FALSE
        ),
        shapes = shape_list,
        annotations = annotation_list,
        margin = list(l = 80, r = 40, b = 70, t = 90),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "lasso2d",
          "select2d",
          "autoScale2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian"
        )
      )
  })

  output$ssa_country_stage_plot_ui <- renderUI({
    req(input$stage_panel_region)

    stage_country_count <- if (identical(input$stage_panel_region, "All SSA")) {
      length(country_names_ssa)
    } else {
      country_catalog %>%
        filter(is_ssa, as.character(region) == input$stage_panel_region) %>%
        nrow()
    }

    facet_cols <- 2
    facet_rows <- max(1, ceiling(stage_country_count / facet_cols))
    plot_height_px <- max(4200, 380 + facet_rows * 330)

    plotlyOutput("ssa_country_stage_plot", height = paste0(plot_height_px, "px"))
  })

  output$ssa_country_stage_plot <- renderPlotly({
    req(input$stage_panel_region, input$ssa_plot_end_year)

    stage_countries <- if (input$stage_panel_region == "All SSA") {
      country_names_ssa
    } else {
      country_catalog %>%
        filter(is_ssa, as.character(region) == input$stage_panel_region) %>%
        pull(country)
    }
    req(length(stage_countries) > 0)

    meta_df <- ssa_country_plot_base %>%
      filter(country %in% stage_countries) %>%
      transmute(
        country,
        region,
        K_mean,
        t0_year_mean,
        p2026_mean = p2026_mean,
        s_curve_stage_2026,
        stage_panel_label = paste0(country, "\n", s_curve_stage_2026, " | t0~", round(t0_year_mean), " | K ", round(100 * K_mean), "%")
      ) %>%
      arrange(
        factor(s_curve_stage_2026, levels = c("Stage 1: initial", "Stage 2: diffusion", "Stage 3: plateau")),
        region,
        country
      ) %>%
      mutate(stage_panel_label = factor(stage_panel_label, levels = unique(stage_panel_label), ordered = TRUE))

    curve_df <- country_curve_summary %>%
      filter(country %in% stage_countries, year <= input$ssa_plot_end_year) %>%
      left_join(meta_df, by = c("country", "region")) %>%
      arrange(country, year) %>%
      mutate(
        hover = paste0(
          "<b>", country, "</b><br>",
          "Region: ", region, "<br>",
          "Year: ", year, "<br>",
          "Model mCPR: ", sprintf("%.1f%%", 100 * p_mean), "<br>",
          "Stage at 2026: ", s_curve_stage_2026
        )
      )
    obs_df <- obs_data %>%
      filter(country %in% stage_countries) %>%
      left_join(meta_df %>% select(country, stage_panel_label), by = "country") %>%
      mutate(
        hover = paste0(
          "<b>", country, "</b><br>",
          "Survey: ", survey_id, "<br>",
          "Year: ", year, "<br>",
          "Observed mCPR: ", format_pct(mCPR, 1)
        )
      )

    facet_cols <- 2

    p <- ggplot(curve_df, aes(x = year, y = 100 * p_mean, text = hover, group = country)) +
      geom_ribbon(aes(ymin = 100 * p_lo, ymax = 100 * p_hi, fill = s_curve_stage_2026, group = country), alpha = 0.08) +
      geom_line(aes(color = s_curve_stage_2026, group = country), linewidth = 1.45, alpha = 0.98) +
      geom_point(
        data = obs_df,
        aes(x = year, y = 100 * mCPR, text = hover),
        inherit.aes = FALSE,
        color = "black",
        size = 0.45,
        alpha = 0.24
      ) +
      geom_hline(data = meta_df, aes(yintercept = 100 * K_mean), color = "#8A5A00", linetype = "dotted", linewidth = 0.72) +
      geom_vline(data = meta_df, aes(xintercept = t0_year_mean), color = "#1D4ED8", linetype = "dotted", linewidth = 0.72) +
      geom_vline(xintercept = goal_year, color = "grey15", linetype = "longdash", linewidth = 0.78) +
      facet_wrap(~ stage_panel_label, ncol = facet_cols, scales = "free_x") +
      scale_color_manual(values = stage_palette, name = "Stage at 2026") +
      scale_fill_manual(values = stage_palette, name = "Stage at 2026") +
      theme_minimal(base_size = 10.9) +
      theme(
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 9.5, lineheight = 1.06),
        axis.line.x = element_line(color = "grey20", linewidth = 0.6),
        axis.ticks.x = element_line(color = "grey20", linewidth = 0.5),
        axis.text.x = element_text(size = 8.8),
        axis.text.y = element_text(size = 8.8),
        panel.spacing = grid::unit(1.15, "lines"),
        legend.position = "bottom",
        plot.title.position = "plot"
      ) +
      labs(
        title = "Every SSA country on the S curve",
        subtitle = "The line and ribbon show the fitted trajectory with a 95% credible interval. The ochre dotted line marks fitted K, the blue dotted line marks the inflection year, and the dark dashed line marks 2035. The layout expands automatically so the country panels stay readable.",
        x = "Year",
        y = "Model mCPR (%)"
      )

    interactive_plot(p, x_year = TRUE)
  })

  output$weighted_region_stage_plot <- renderPlotly({
    group_name <- req(input$weighted_region_choice)
    curve_df <- weighted_curve_summary %>%
      filter(geography == group_name, year <= input$weighted_region_end_year) %>%
      arrange(year) %>%
      mutate(
        hover = paste0(
          "<b>", geography, "</b><br>",
          "Year: ", year, "<br>",
          "Model mCPR: ", sprintf("%.1f%%", 100 * p_mean), "<br>",
          "95% CrI: ", sprintf("%.1f", 100 * p_lo), " to ", sprintf("%.1f", 100 * p_hi), "%"
        )
      )
    obs_df <- weighted_observed_summary %>%
      filter(geography == group_name) %>%
      mutate(
        hover = paste0(
          "<b>", geography, "</b><br>",
          "Observed year: ", year, "<br>",
          "Observed weighted mCPR: ", sprintf("%.1f%%", 100 * observed_weighted_mcpr), "<br>",
          "Countries observed: ", n_countries_obs
        )
      )
    meta_df <- weighted_stage_summary %>%
      filter(geography == group_name) %>%
      slice(1)
    curve_color <- if (group_name %in% names(region_palette)) region_palette[[group_name]] else "#0F4C81"
    stage_note <- paste0(
      "Stage at 2026: ", meta_df$stage_label[[1]],
      "\nWeighted K: ", round(100 * meta_df$K_mean[[1]], 1), "%",
      "\nWeighted p(2026): ", round(100 * meta_df$p2026_mean[[1]], 1), "%",
      "\nRelative saturation p/K: ", round(100 * meta_df$rel_sat_mean[[1]], 1), "%",
      "\nInflection year: ", round(meta_df$inflection_p50[[1]], 1),
      " (95% CrI ", round(meta_df$inflection_lo[[1]], 1),
      "-", round(meta_df$inflection_hi[[1]], 1), ")"
    )
    label_x <- max(curve_df$year, na.rm = TRUE) - 1.2
    note_x <- min(curve_df$year, na.rm = TRUE) + 3
    cross_x <- meta_df$crossing_p50[[1]]
    cross_label <- if (is.na(cross_x)) {
      "No weighted crossing projected"
    } else {
      paste0("Weighted crossing year: ~", round(cross_x, 1))
    }

    p <- ggplot(curve_df, aes(x = year, y = 100 * p_mean, text = hover, group = 1)) +
      geom_ribbon(aes(ymin = 100 * p_lo, ymax = 100 * p_hi, group = 1), fill = curve_color, alpha = 0.12) +
      geom_line(aes(group = 1), color = curve_color, linewidth = 1.85) +
      geom_point(
        data = obs_df,
        aes(x = year, y = 100 * observed_weighted_mcpr, text = hover),
        inherit.aes = FALSE,
        color = "#111827",
        fill = "white",
        shape = 21,
        size = 1.8,
        stroke = 0.4
      ) +
      geom_hline(yintercept = 40, color = "#C2410C", linetype = "longdash", linewidth = 0.9) +
      geom_hline(yintercept = 100 * meta_df$K_mean[[1]], color = "#8A5A00", linetype = "dotted", linewidth = 0.9) +
      geom_vline(xintercept = meta_df$inflection_p50[[1]], color = "#1D4ED8", linetype = "dotted", linewidth = 0.85) +
      geom_vline(xintercept = goal_year, color = "grey25", linetype = "longdash", linewidth = 0.8) +
      {
        if (!is.na(cross_x) && is.finite(cross_x)) {
          geom_vline(xintercept = cross_x, color = "#7C3AED", linetype = "dotdash", linewidth = 0.95)
        }
      } +
      annotate("label", x = label_x, y = 41.5, label = "40% goal", hjust = 1, size = 4.0, fontface = "bold", fill = "#FFF7ED", label.size = 0.22, color = "#C2410C") +
      annotate("label", x = label_x, y = 100 * meta_df$K_mean[[1]] + 1.5, label = paste0("Weighted K: ", sprintf("%.1f%%", 100 * meta_df$K_mean[[1]])), hjust = 1, size = 4.0, fontface = "bold", fill = "#FFFBEB", label.size = 0.22, color = "#8A5A00") +
      {
        if (!is.na(cross_x) && is.finite(cross_x)) {
          annotate(
            "label",
            x = cross_x,
            y = 8,
            label = cross_label,
            angle = 90,
            hjust = 0,
            vjust = -0.15,
            size = 3.4,
            fontface = "bold",
            label.size = 0.22,
            fill = "#F5F3FF",
            color = "#7C3AED"
          )
        }
      } +
      annotate(
        "label",
        x = note_x,
        y = min(98, 100 * meta_df$K_mean[[1]] + 8),
        label = stage_note,
        hjust = 0,
        vjust = 1,
        size = 3.4,
        label.size = 0.18,
        fill = "white",
        color = "grey15"
      ) +
      scale_x_continuous(
        breaks = sort(unique(c(
          seq(
            floor(min(curve_df$year, na.rm = TRUE) / 5) * 5,
            ceiling(max(curve_df$year, na.rm = TRUE) / 5) * 5,
            by = 5
          ),
          2035
        )))
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.02, 0.10))) +
      coord_cartesian(clip = "off") +
      theme_minimal(base_size = 12.2) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey88", linewidth = 0.35),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.35),
        axis.line.y = element_line(color = "grey20", linewidth = 0.6),
        axis.ticks.y = element_line(color = "grey20", linewidth = 0.5),
        axis.title = element_text(size = 13.2, face = "bold"),
        axis.text = element_text(size = 11.2, color = "grey15"),
        plot.title.position = "plot"
      ) +
      labs(
        title = paste0(group_name, " on the S curve at 2026"),
        subtitle = "This uses the same weighted-region stage logic as the SSA view. The horizontal reference lines mark the 40% goal and weighted K, and the vertical guides mark the inflection year, 2035 horizon, and the weighted crossing year when projected.",
        x = "Year",
        y = "Population-weighted model mCPR (%)"
      )

    interactive_plot(p, x_year = TRUE)
  })

  output$weighted_region_stage_table <- renderDT({
    stage_table <- weighted_stage_summary %>%
      transmute(
        Geography = geography,
        `Women 15-49 (latest)` = round(women_15_49 / 1e6, 1),
        `Stage at 2026` = stage_label,
        `Weighted p(2026) (%)` = round(100 * p2026_mean, 1),
        `Weighted K (%)` = round(100 * K_mean, 1),
        `Relative saturation p/K (%)` = round(100 * rel_sat_mean, 1),
        `Inflection year (p50)` = round(inflection_p50),
        `Crossing year (p50)` = ifelse(is.finite(crossing_p50), round(crossing_p50), NA_real_),
        `P(reach 40% by 2035)` = percent(prob_reach_40_by_2035, accuracy = 0.1)
      ) %>%
      arrange(match(Geography, c("Sub-Saharan Africa (weighted)", region_levels, "Global weighted")))

    datatable(stage_table, rownames = FALSE, options = list(pageLength = nrow(stage_table), dom = "t", scrollX = TRUE))
  })

  output$validation_table <- renderDT({
    validation_df <- country_catalog %>%
      left_join(
        country_curve_summary %>%
          filter(year == 2025) %>%
          select(country, p2025_mean = p_mean),
        by = "country"
      ) %>%
      left_join(
        ssa_validation_audit %>% select(country, p_last_observed_year_model, p2025_model),
        by = "country"
      ) %>%
      mutate(
        p_last_observed_year_model = coalesce(p_last_observed_year_model, projection_latest_mean),
        p2025_model = coalesce(p2025_model, p2025_mean)
      ) %>%
      filter(as.character(region) == input$region) %>%
      transmute(
        Country = country,
        `Latest survey year` = latest_obs_year,
        `Observed latest mCPR (%)` = round(100 * latest_obs_mcpr, 1),
        `Model p(last observed year) (%)` = round(100 * p_last_observed_year_model, 1),
        `Model p(2025) (%)` = round(100 * p2025_model, 1),
        `Absolute fit gap at latest survey (pp)` = round(100 * abs(latest_obs_mcpr - p_last_observed_year_model), 1),
        `Stage at 2026` = s_curve_stage_2026,
        Phase = phase
      ) %>%
      arrange(desc(`Absolute fit gap at latest survey (pp)`))

    datatable(validation_df, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })
}

shinyApp(ui = ui, server = server)
