# ==============================================================================
# Tests for Validation Functions
# ==============================================================================

library(testthat)

# Source the validation file
source("../../config.R")
source("../../server/validation.R")

# ==============================================================================
# Tests for validate_images_directory()
# ==============================================================================

test_that("validate_images_directory returns valid=FALSE for non-existent directory", {
  result <- validate_images_directory("nonexistent_directory_12345")
  expect_false(result$valid)
  expect_true(length(result$errors) > 0)
})
test_that("validate_images_directory returns valid=TRUE for existing directory", {
  # Use current directory which should exist
  result <- validate_images_directory(".")
  expect_true(result$valid)
})

test_that("validate_images_directory counts PNG files", {
  result <- validate_images_directory("../../www")
  expect_true(is.numeric(result$png_count))
})

# ==============================================================================
# Tests for validate_excel_data()
# ==============================================================================

test_that("validate_excel_data returns error for empty data frame", {
  empty_df <- data.frame()
  result <- validate_excel_data(empty_df)
  expect_false(result$valid)
})

test_that("validate_excel_data returns error for missing columns", {
  bad_df <- data.frame(col1 = 1:3, col2 = 4:6)
  result <- validate_excel_data(bad_df)
  expect_false(result$valid)
  expect_true(any(grepl("Missing required columns", result$errors)))
})

test_that("validate_excel_data returns valid for correct structure", {
  good_df <- data.frame(
    `83-type signature ID` = c("ID1", "ID2"),
    `89-type signature ID (this study)` = c("InsDel1a", "InsDel1b"),
    `Proposed Etiology` = c("Cause 1", "Cause 2"),
    check.names = FALSE
  )
  result <- validate_excel_data(good_df)
  expect_true(result$valid)
})

# ==============================================================================
# Tests for validate_signature_groups()
# ==============================================================================

test_that("validate_signature_groups returns error for empty list", {
  result <- validate_signature_groups(list())
  expect_false(result$valid)
})

test_that("validate_signature_groups returns count", {
  groups <- list(
    sig1 = list(imgs = character(), thumbnail = "", aetiology = ""),
    sig2 = list(imgs = character(), thumbnail = "", aetiology = "")
  )
  result <- validate_signature_groups(groups)
  expect_equal(result$count, 2)
})

test_that("validate_signature_groups detects missing thumbnails", {
  groups <- list(
    sig1 = list(
      imgs = c("nonexistent.png"),
      thumbnail = "nonexistent_thumbnail.png",
      aetiology = "test"
    )
  )
  result <- validate_signature_groups(groups)
  expect_true(length(result$missing_thumbnails) > 0)
})

# ==============================================================================
# Tests for validate_id83_groups()
# ==============================================================================

test_that("validate_id83_groups returns warning for empty list", {
  result <- validate_id83_groups(list())
  expect_true(any(grepl("No ID83 groups", result$warnings)))
})

test_that("validate_id83_groups returns count", {
  groups <- list(
    id1 = list(members = c("a", "b"), id83_all = "", thumbnail = ""),
    id2 = list(members = c("c"), id83_all = "", thumbnail = "")
  )
  result <- validate_id83_groups(groups)
  expect_equal(result$count, 2)
})

test_that("validate_id83_groups detects empty members", {
  groups <- list(
    id1 = list(members = character(), id83_all = "", thumbnail = "")
  )
  result <- validate_id83_groups(groups)
  expect_true(length(result$empty_members) > 0)
})

# ==============================================================================
# Tests for validate_all()
# ==============================================================================

test_that("validate_all returns list with expected structure", {
  # Use a config that will fail (nonexistent files)
  test_config <- list(
    data_file = "nonexistent.xlsx",
    signatures_csv = "nonexistent.csv",
    images_dir = "nonexistent_dir"
  )

  result <- validate_all(config = test_config)

  expect_true(is.list(result))
  expect_true("valid" %in% names(result))
  expect_true("errors" %in% names(result))
  expect_true("warnings" %in% names(result))
})

test_that("validate_all returns valid=FALSE for missing files", {
  test_config <- list(
    data_file = "nonexistent.xlsx",
    signatures_csv = "nonexistent.csv",
    images_dir = "."
  )

  result <- validate_all(config = test_config)
  expect_false(result$valid)
})
