# ==============================================================================
# Logging Module
# Centralized logging for the Indel Signature Browser
# ==============================================================================

# Log levels
LOG_LEVELS <- list(
  DEBUG = 1,
  INFO = 2,
  WARN = 3,
  ERROR = 4
)

# Current log level (set from config)
.log_level <- LOG_LEVELS$INFO
.log_enabled <- TRUE
.log_file <- NULL

#' Initialize logging
#'
#' @param config List. Configuration with logging settings
init_logging <- function(config = NULL) {
  if (!is.null(config) && !is.null(config$logging)) {
    .log_enabled <<- config$logging$enabled %||% TRUE

    level_name <- config$logging$level %||% "INFO"
    .log_level <<- LOG_LEVELS[[level_name]] %||% LOG_LEVELS$INFO

    if (!is.null(config$logging$file)) {
      .log_file <<- config$logging$file
      # Ensure log directory exists
      log_dir <- dirname(.log_file)
      if (!dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE)
      }
    }
  }
}

#' Format a log message
#'
#' @param level Character. Log level
#' @param message Character. Log message
#' @param ... Additional arguments for sprintf
#' @return Formatted log string
format_log_message <- function(level, message, ...) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- tryCatch(
    sprintf(message, ...),
    error = function(e) message
  )
  sprintf("[%s] [%s] %s", timestamp, level, msg)
}

#' Write a log message
#'
#' @param level Character. Log level name
#' @param level_num Numeric. Log level number
#' @param message Character. Log message
#' @param ... Additional arguments for sprintf
write_log <- function(level, level_num, message, ...) {
  if (!.log_enabled) return(invisible(NULL))
  if (level_num < .log_level) return(invisible(NULL))

  formatted <- format_log_message(level, message, ...)

  # Write to console
  if (level == "ERROR") {
    message(formatted)
  } else if (level == "WARN") {
    message(formatted)
  } else {
    message(formatted)
  }

  # Write to file if configured
  if (!is.null(.log_file)) {
    tryCatch({
      cat(formatted, "\n", file = .log_file, append = TRUE)
    }, error = function(e) {
      # Silently fail file logging
    })
  }

  invisible(formatted)
}

#' Log a debug message
#'
#' @param message Character. Log message (can use sprintf format)
#' @param ... Additional arguments for sprintf
log_debug <- function(message, ...) {
  write_log("DEBUG", LOG_LEVELS$DEBUG, message, ...)
}

#' Log an info message
#'
#' @param message Character. Log message (can use sprintf format)
#' @param ... Additional arguments for sprintf
log_info <- function(message, ...) {
  write_log("INFO", LOG_LEVELS$INFO, message, ...)
}

#' Log a warning message
#'
#' @param message Character. Log message (can use sprintf format)
#' @param ... Additional arguments for sprintf
log_warn <- function(message, ...) {
  write_log("WARN", LOG_LEVELS$WARN, message, ...)
}

#' Log an error message
#'
#' @param message Character. Log message (can use sprintf format)
#' @param ... Additional arguments for sprintf
log_error <- function(message, ...) {
  write_log("ERROR", LOG_LEVELS$ERROR, message, ...)
}

#' Log application startup
#'
#' @param signature_count Numeric. Number of signatures loaded
#' @param id83_count Numeric. Number of ID83 groups loaded
log_app_start <- function(signature_count = 0, id83_count = 0) {
  log_info("========================================")
  log_info("Indel Signature Browser starting")
  log_info("========================================")
  log_info("Environment: %s", APP_ENV)
  log_info("Signatures loaded: %d", signature_count)
  log_info("ID83 groups loaded: %d", id83_count)
}

#' Log a user action
#'
#' @param action Character. Action name
#' @param details Character. Additional details (optional)
log_user_action <- function(action, details = NULL) {
  if (is.null(details)) {
    log_debug("User action: %s", action)
  } else {
    log_debug("User action: %s - %s", action, details)
  }
}

#' Log a search action
#'
#' @param term Character. Search term
#' @param results_89 Numeric. Number of 89-type matches
#' @param results_83 Numeric. Number of 83-type matches
log_search <- function(term, results_89 = 0, results_83 = 0) {
  log_debug("Search: '%s' -> %d 89-type, %d 83-type matches",
            term, results_89, results_83)
}

#' Log a navigation event
#'
#' @param from Character. Source tab/page
#' @param to Character. Destination tab/page
log_navigation <- function(from, to) {
  log_debug("Navigation: %s -> %s", from, to)
}

#' Log data loading
#'
#' @param file Character. File being loaded
#' @param success Logical. Whether load succeeded
#' @param rows Numeric. Number of rows loaded (optional)
log_data_load <- function(file, success, rows = NULL) {
  if (success) {
    if (!is.null(rows)) {
      log_info("Loaded %s: %d rows", file, rows)
    } else {
      log_info("Loaded %s", file)
    }
  } else {
    log_error("Failed to load %s", file)
  }
}

#' Log validation results
#'
#' @param results List. Validation results from validate_all()
log_validation_results <- function(results) {
  if (results$valid) {
    log_info("Validation passed")
  } else {
    log_error("Validation failed")
    for (err in results$errors) {
      log_error("  - %s", err)
    }
  }

  for (warn in results$warnings) {
    log_warn("  - %s", warn)
  }
}

#' Log an error with stack trace
#'
#' @param error Condition. Error object
#' @param context Character. Context where error occurred
log_error_with_trace <- function(error, context = "Unknown") {
  log_error("Error in %s: %s", context, conditionMessage(error))

  # Log stack trace if available
  trace <- sys.calls()
  if (length(trace) > 0) {
    log_debug("Stack trace:")
    for (i in seq_along(trace)) {
      log_debug("  %d: %s", i, deparse(trace[[i]])[1])
    }
  }
}
