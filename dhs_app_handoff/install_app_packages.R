get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) == 0) {
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  } else {
    script_path <- sub("^--file=", "", file_arg[1])
    if (!file.exists(script_path)) {
      normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    } else {
      dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
    }
  }
}

project_root <- get_script_dir()
project_lib <- file.path(project_root, "r_libs")
dir.create(project_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(project_lib, .libPaths()))

packages <- c(
  "shiny", "dplyr", "tidyr", "ggplot2", "plotly", "DT", "brms",
  "posterior", "readr", "htmltools", "scales", "stringr", "purrr"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages(packages, lib = project_lib, dependencies = TRUE)

cat("Package install complete.\n")
cat("Local library:\n", project_lib, "\n", sep = "")
