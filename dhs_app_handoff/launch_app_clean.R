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
app_dir <- file.path(project_root, "shiny_dashboard")

dir.create(project_lib, recursive = TRUE, showWarnings = FALSE)
setwd(project_root)
.libPaths(c(project_lib, .libPaths()))

required_packages <- c(
  "shiny", "dplyr", "tidyr", "ggplot2", "plotly", "DT", "brms",
  "posterior", "readr", "htmltools", "scales", "stringr", "purrr"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("Missing required packages:\n")
  cat(paste0(" - ", missing_packages), sep = "\n")
  cat("\n\nRun '1_Install_DHS_Dashboard_Packages.bat' first, then try again.\n")
  quit(status = 1)
}

cat("Launching DHS dashboard from:\n", app_dir, "\n", sep = "")
cat("Using local library:\n", project_lib, "\n", sep = "")

shiny::runApp(app_dir, launch.browser = TRUE)
