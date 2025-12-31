# ==============================================================================
# Package Dependencies
# Central location for all required packages
# ==============================================================================

# Required packages for the Shiny app (CRAN)
REQUIRED_PACKAGES <- c(

  # Shiny core
  "shiny",
  "shinyjs",
  "shinydashboard",

  # Data manipulation
  "dplyr",
  "tidyr",
  "data.table",

  # File I/O
  "readxl",

  # Plotting
  "mSigPlot"
)

# GitHub packages (not on CRAN)
GITHUB_PACKAGES <- list(
  mSigPlot = "steverozen/mSigPlot"
)

# Optional packages (for development/testing)
OPTIONAL_PACKAGES <- c(
  "testthat",   # Unit testing
  "ggplot2",    # Plotting (used by Koh89.476.plotting.code.functions.R)
  "remotes"     # For installing GitHub packages
)

#' Load all required packages
#'
#' @param quiet Logical. Suppress package startup messages (default: TRUE)
#' @return Invisible NULL
load_dependencies <- function(quiet = TRUE) {
  for (pkg in REQUIRED_PACKAGES) {
    if (quiet) {
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    } else {
      library(pkg, character.only = TRUE)
    }
  }
  invisible(NULL)
}

#' Check if all required packages are installed
#'
#' @return List with 'ok' (logical) and 'missing' (character vector)
check_dependencies <- function() {
  installed <- rownames(installed.packages())
  missing <- REQUIRED_PACKAGES[!REQUIRED_PACKAGES %in% installed]

  list(
    ok = length(missing) == 0,
    missing = missing
  )
}

#' Install missing packages
#'
#' @param include_optional Logical. Also install optional packages (default: FALSE)
#' @return Invisible NULL
install_dependencies <- function(include_optional = FALSE) {
  installed <- rownames(installed.packages())

  # Separate CRAN and GitHub packages
  cran_packages <- setdiff(REQUIRED_PACKAGES, names(GITHUB_PACKAGES))
  if (include_optional) {
    cran_packages <- c(cran_packages, OPTIONAL_PACKAGES)
  }

  # Install CRAN packages
  cran_to_install <- cran_packages[!cran_packages %in% installed]
  if (length(cran_to_install) > 0) {
    message("Installing CRAN packages: ", paste(cran_to_install, collapse = ", "))
    install.packages(cran_to_install)
  }

 # Install GitHub packages
  github_to_install <- names(GITHUB_PACKAGES)[!names(GITHUB_PACKAGES) %in% installed]
  if (length(github_to_install) > 0) {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      message("Installing 'remotes' package for GitHub installations...")
      install.packages("remotes")
    }
    for (pkg in github_to_install) {
      repo <- GITHUB_PACKAGES[[pkg]]
      message("Installing from GitHub: ", repo)
      remotes::install_github(repo)
    }
  }

  if (length(cran_to_install) == 0 && length(github_to_install) == 0) {
    message("All packages already installed")
  }

  invisible(NULL)
}

#' Print dependency status
#'
#' @return Invisible NULL
print_dependency_status <- function() {
  installed <- rownames(installed.packages())

  # CRAN packages
  cran_packages <- setdiff(REQUIRED_PACKAGES, names(GITHUB_PACKAGES))
  message("Required packages (CRAN):")
  for (pkg in cran_packages) {
    status <- if (pkg %in% installed) "\u2713" else "\u2717 (missing)"
    message(sprintf("  %s %s", status, pkg))
  }

  # GitHub packages
  message("\nRequired packages (GitHub):")
  for (pkg in names(GITHUB_PACKAGES)) {
    repo <- GITHUB_PACKAGES[[pkg]]
    status <- if (pkg %in% installed) "\u2713" else "\u2717 (missing)"
    message(sprintf("  %s %s [%s]", status, pkg, repo))
  }

  message("\nOptional packages:")
  for (pkg in OPTIONAL_PACKAGES) {
    status <- if (pkg %in% installed) "\u2713" else "\u2717 (not installed)"
    message(sprintf("  %s %s", status, pkg))
  }

  invisible(NULL)
}
