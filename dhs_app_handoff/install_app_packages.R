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

plot_stack <- c(
  "rlang", "cli", "withr", "glue", "lifecycle", "vctrs",
  "gtable", "isoband", "munsell", "scales", "tibble",
  "ggplot2", "ggrepel"
)

packages <- c(
  plot_stack,
  "shiny", "dplyr", "tidyr", "plotly", "DT", "brms",
  "posterior", "readr", "htmltools", "stringr", "purrr"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))

install_type <- getOption("pkgType")
if (.Platform$OS.type == "windows") {
  install_type <- "win.binary"
  options(pkgType = install_type)
  options(install.packages.compile.from.source = "never")
}

install.packages(plot_stack, lib = project_lib, dependencies = TRUE, type = install_type)
install.packages(setdiff(packages, plot_stack), lib = project_lib, dependencies = TRUE, type = install_type)

missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("The following packages are still missing after install:\n")
  cat(paste0(" - ", missing_packages), sep = "\n")
  quit(status = 1)
}

cat("Package install complete.\n")
cat("Local library:\n", project_lib, "\n", sep = "")
