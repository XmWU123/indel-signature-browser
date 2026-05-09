# ==============================================================================
# Validation Functions
# Input validation and data integrity checks
# ==============================================================================

#' Validate that required data files exist
#'
#' @param config List. Configuration object with data_file and signatures_csv
#' @return TRUE if valid, stops with error otherwise
validate_data_files <- function(config = CONFIG) {
  required_files <- c(
    config$data_file,
    config$signatures_csv
  )

  missing <- required_files[!file.exists(required_files)]

  if (length(missing) > 0) {
    stop(
      "Missing required data files:\n",
      paste("  -", missing, collapse = "\n"),
      "\n\nPlease ensure all data files are in place before starting the app."
    )
  }

  TRUE
}

#' Validate that the www directory exists and contains images
#'
#' @param images_dir Character. Path to images directory (default: "www")
#' @return List with validation results
validate_images_directory <- function(images_dir = "www") {
  result <- list(
    valid = TRUE,
    warnings = character(),
    errors = character(),
    png_count = 0
  )

  if (!dir.exists(images_dir)) {
    result$valid <- FALSE
    result$errors <- c(result$errors, paste("Images directory not found:", images_dir))
    return(result)
  }

  png_files <- list.files(images_dir, pattern = "\\.png$", ignore.case = TRUE)
  result$png_count <- length(png_files)

  if (result$png_count == 0) {
    result$warnings <- c(result$warnings, "No PNG files found in images directory")
  }

  # Check for expected image types
  has_thumbnails <- any(grepl("Thumbnail", png_files))
  has_89spectrum <- any(grepl("89spectrum", png_files))
  has_83all <- any(grepl("_83all", png_files))

  if (!has_thumbnails) {
    result$warnings <- c(result$warnings, "No thumbnail images found")
  }
  if (!has_89spectrum) {
    result$warnings <- c(result$warnings, "No 89-spectrum images found")
  }
  if (!has_83all) {
    result$warnings <- c(result$warnings, "No 83-type signature images found")
  }

  result
}

#' Validate Excel data structure
#'
#' @param data Data frame. Raw data from Excel file
#' @return List with validation results
validate_excel_data <- function(data) {
  result <- list(
    valid = TRUE,
    warnings = character(),
    errors = character()
  )

  required_columns <- c(
    "83-type signature ID",
    "89-type signature ID (this study)",
    "Proposed Etiology"
  )

  # Check for required columns
  missing_cols <- required_columns[!required_columns %in% names(data)]
  if (length(missing_cols) > 0) {
    result$valid <- FALSE
    result$errors <- c(
      result$errors,
      paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    )
  }

  # Check for empty data
  if (nrow(data) == 0) {
    result$valid <- FALSE
    result$errors <- c(result$errors, "Excel file contains no data rows")
  }

  result
}

#' Validate signature groups data structure
#'
#' @param groups List. signature_groups data structure
#' @return List with validation results
validate_signature_groups <- function(groups) {
  result <- list(
    valid = TRUE,
    warnings = character(),
    errors = character(),
    count = length(groups),
    missing_thumbnails = character(),
    missing_images = character()
  )

  if (length(groups) == 0) {
    result$valid <- FALSE
    result$errors <- c(result$errors, "No signature groups loaded")
    return(result)
  }

  for (name in names(groups)) {
    sig <- groups[[name]]

    # Check thumbnail
    if (!is.null(sig$thumbnail) && nzchar(sig$thumbnail)) {
      if (!file.exists(file.path("www", sig$thumbnail))) {
        result$missing_thumbnails <- c(result$missing_thumbnails, name)
      }
    }

    # Check main image
    if (length(sig$imgs) > 0 && nzchar(sig$imgs[1])) {
      if (!file.exists(file.path("www", sig$imgs[1]))) {
        result$missing_images <- c(result$missing_images, name)
      }
    }
  }

  if (length(result$missing_thumbnails) > 0) {
    result$warnings <- c(
      result$warnings,
      sprintf("%d signatures missing thumbnails", length(result$missing_thumbnails))
    )
  }

  if (length(result$missing_images) > 0) {
    result$warnings <- c(
      result$warnings,
      sprintf("%d signatures missing main images", length(result$missing_images))
    )
  }

  result
}

#' Validate ID83 groups data structure
#'
#' @param groups List. id83_groups data structure
#' @return List with validation results
validate_id83_groups <- function(groups) {
  result <- list(
    valid = TRUE,
    warnings = character(),
    errors = character(),
    count = length(groups),
    empty_members = character()
  )

  if (length(groups) == 0) {
    result$warnings <- c(result$warnings, "No ID83 groups loaded")
    return(result)
  }

  for (name in names(groups)) {
    info <- groups[[name]]

    # Check for empty members
    if (length(info$members) == 0) {
      result$empty_members <- c(result$empty_members, name)
    }
  }

  if (length(result$empty_members) > 0) {
    result$warnings <- c(
      result$warnings,
      sprintf("%d ID83 groups have no members", length(result$empty_members))
    )
  }

  result
}

#' Run all validations and return summary
#'
#' @param config List. Configuration object
#' @param signature_groups List. Signature groups (optional, skipped if NULL)
#' @param id83_groups List. ID83 groups (optional, skipped if NULL)
#' @return List with overall validation results
validate_all <- function(config = CONFIG,
                         signature_groups = NULL,
                         id83_groups = NULL) {

  results <- list(
    valid = TRUE,
    errors = character(),
    warnings = character()
  )

  # Validate data files
  tryCatch({
    validate_data_files(config)
  }, error = function(e) {
    results$valid <<- FALSE
    results$errors <<- c(results$errors, e$message)
  })

  # Validate images directory
  img_result <- validate_images_directory(config$images_dir)
  if (!img_result$valid) {
    results$valid <- FALSE
    results$errors <- c(results$errors, img_result$errors)
  }
  results$warnings <- c(results$warnings, img_result$warnings)

  # Validate signature groups if provided
if (!is.null(signature_groups)) {
    sig_result <- validate_signature_groups(signature_groups)
    if (!sig_result$valid) {
      results$valid <- FALSE
      results$errors <- c(results$errors, sig_result$errors)
    }
    results$warnings <- c(results$warnings, sig_result$warnings)
  }

  # Validate ID83 groups if provided
  if (!is.null(id83_groups)) {
    id83_result <- validate_id83_groups(id83_groups)
    if (!id83_result$valid) {
      results$valid <- FALSE
      results$errors <- c(results$errors, id83_result$errors)
    }
    results$warnings <- c(results$warnings, id83_result$warnings)
  }

  results
}

#' Print validation results to console
#'
#' @param results List. Results from validate_all()
print_validation_results <- function(results) {
  if (results$valid) {
    message("\u2714 All validations passed")
  } else {
    message("\u2718 Validation failed")
  }

  if (length(results$errors) > 0) {
    message("\nErrors:")
    for (err in results$errors) {
      message("  \u2718 ", err)
    }
  }

  if (length(results$warnings) > 0) {
    message("\nWarnings:")
    for (warn in results$warnings) {
      message("  \u26A0 ", warn)
    }
  }
}
