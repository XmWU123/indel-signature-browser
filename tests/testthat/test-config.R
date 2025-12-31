# ==============================================================================
# Tests for Configuration
# ==============================================================================

library(testthat)

# Source the config file
source("../../config.R")

# ==============================================================================
# Tests for CONFIG structure
# ==============================================================================

test_that("CONFIG is a list", {
  expect_true(is.list(CONFIG))
})

test_that("CONFIG contains data_file", {
  expect_true("data_file" %in% names(CONFIG))
  expect_true(is.character(CONFIG$data_file))
})

test_that("CONFIG contains signatures_csv", {
  expect_true("signatures_csv" %in% names(CONFIG))
  expect_true(is.character(CONFIG$signatures_csv))
})

test_that("CONFIG contains tabs list", {
  expect_true("tabs" %in% names(CONFIG))
  expect_true(is.list(CONFIG$tabs))
})

test_that("CONFIG tabs contains required tab names", {
  expect_true("HOME" %in% names(CONFIG$tabs))
  expect_true("KOH89" %in% names(CONFIG$tabs))
  expect_true("COSMIC83" %in% names(CONFIG$tabs))
  expect_true("SEARCH" %in% names(CONFIG$tabs))
  expect_true("ABOUT" %in% names(CONFIG$tabs))
})

test_that("CONFIG contains colors list", {
  expect_true("colors" %in% names(CONFIG))
  expect_true(is.list(CONFIG$colors))
})

test_that("CONFIG colors are valid hex codes", {
  for (color in CONFIG$colors) {
    expect_match(color, "^#[0-9A-Fa-f]{6}$")
  }
})

test_that("CONFIG contains ui settings", {
  expect_true("ui" %in% names(CONFIG))
  expect_true(is.list(CONFIG$ui))
  expect_true("grid_columns" %in% names(CONFIG$ui))
})

test_that("CONFIG ui grid_columns is numeric", {
  expect_true(is.numeric(CONFIG$ui$grid_columns))
  expect_true(CONFIG$ui$grid_columns > 0)
})

# ==============================================================================
# Tests for get_config()
# ==============================================================================

test_that("get_config returns correct value for single key", {
  expect_equal(get_config("data_file"), CONFIG$data_file)
})

test_that("get_config returns correct value for nested keys", {
  expect_equal(get_config("tabs", "HOME"), CONFIG$tabs$HOME)
})

test_that("get_config returns NULL for non-existent key", {
  expect_null(get_config("nonexistent_key"))
})

test_that("get_config returns NULL for non-existent nested key", {
  expect_null(get_config("tabs", "NONEXISTENT"))
})

# ==============================================================================
# Tests for environment detection
# ==============================================================================

test_that("APP_ENV is defined", {
  expect_true(exists("APP_ENV"))
  expect_true(is.character(APP_ENV))
})

test_that("CONFIG$debug is logical", {
  expect_true(is.logical(CONFIG$debug))
})
