# ==============================================================================
# Tests for Helper Functions
# ==============================================================================

library(testthat)
library(shiny)

# Source the helpers file
source("../../config.R")
source("../../server/helpers.R")

# ==============================================================================
# Tests for image_exists()
# ==============================================================================

test_that("image_exists returns FALSE for NULL input", {
  expect_false(image_exists(NULL))
})

test_that("image_exists returns FALSE for NA input", {
  expect_false(image_exists(NA))
})

test_that("image_exists returns FALSE for empty string", {
  expect_false(image_exists(""))
})

test_that("image_exists returns FALSE for non-existent file", {
  expect_false(image_exists("nonexistent_file_12345.png"))
})

# ==============================================================================
# Tests for filter_existing_images()
# ==============================================================================

test_that("filter_existing_images removes NA values", {
  input <- c("file1.png", NA, "file2.png")
  result <- filter_existing_images(input)
  expect_false(any(is.na(result)))
})

test_that("filter_existing_images removes empty strings", {
  input <- c("file1.png", "", "file2.png")
  result <- filter_existing_images(input)
  expect_false(any(result == ""))
})

test_that("filter_existing_images returns empty vector for empty input", {
  result <- filter_existing_images(character(0))
  expect_length(result, 0)
})

# ==============================================================================
# Tests for get_image_display_title()
# ==============================================================================

test_that("get_image_display_title returns correct title for 89spectrum", {
  expect_equal(
    get_image_display_title("InsDel1a_signature.89spectrum.png"),
    "89-Type Signature"
  )
})

test_that("get_image_display_title returns correct title for sample A", {
  expect_equal(
    get_image_display_title("InsDel1a_89spectrumA.png"),
    "Sample A (Original Spectrum)"
  )
})

test_that("get_image_display_title returns correct title for sample B", {
  expect_equal(
    get_image_display_title("InsDel1a_89spectrumB.png"),
    "Sample B (Reconstructed)"
  )
})

test_that("get_image_display_title returns correct title for sample C", {
  expect_equal(
    get_image_display_title("InsDel1a_89spectrumC.png"),
    "Sample A-B (Residual)"
  )
})

test_that("get_image_display_title returns correct title for 83all", {
  expect_equal(
    get_image_display_title("InsDel1a_ID1_83all.png"),
    "83-Type Signature"
  )
})

test_that("get_image_display_title returns correct title for 83filtered", {
  expect_equal(
    get_image_display_title("InsDel1a_ID1_83filtered.png"),
    "Sample A in 83-Type Representation"
  )
})

test_that("get_image_display_title returns correct title for 476all", {
  expect_equal(
    get_image_display_title("InsDel1a_476all.png"),
    "476-Type Signature"
  )
})

test_that("get_image_display_title returns correct title for Thumbnail", {
  expect_equal(
    get_image_display_title("ID_A_Thumbnail.png"),
    "Group Thumbnail"
  )
})

test_that("get_image_display_title returns default for unknown pattern", {
  expect_equal(
    get_image_display_title("unknown_file.png"),
    "Signature View"
  )
})

# ==============================================================================
# Tests for render_aetiology()
# ==============================================================================

test_that("render_aetiology returns NULL for NULL input", {
  expect_null(render_aetiology(NULL))
})

test_that("render_aetiology returns NULL for NA input", {
  expect_null(render_aetiology(NA))
})

test_that("render_aetiology returns NULL for empty string", {
  expect_null(render_aetiology(""))
})

test_that("render_aetiology returns NULL for whitespace-only string", {
  expect_null(render_aetiology("   "))
})

test_that("render_aetiology returns shiny tag for valid input", {
  result <- render_aetiology("Test aetiology")
  expect_true(inherits(result, "shiny.tag"))
})

test_that("render_aetiology compact version returns shiny tag", {
  result <- render_aetiology("Test aetiology", compact = TRUE)
  expect_true(inherits(result, "shiny.tag"))
})

# ==============================================================================
# Tests for render_placeholder()
# ==============================================================================

test_that("render_placeholder returns shiny tag", {
  result <- render_placeholder()
  expect_true(inherits(result, "shiny.tag"))
})

test_that("render_placeholder accepts custom message", {
  result <- render_placeholder("Custom message")
  expect_true(inherits(result, "shiny.tag"))
})

# ==============================================================================
# Tests for render_section_title()
# ==============================================================================

test_that("render_section_title returns shiny tag", {
  result <- render_section_title("Test Title")
  expect_true(inherits(result, "shiny.tag"))
})

test_that("render_section_title uses default class", {
  result <- render_section_title("Test Title")
  expect_equal(result$attribs$class, "img-section-title")
})

test_that("render_section_title accepts custom class", {
  result <- render_section_title("Test Title", class = "custom-class")
  expect_equal(result$attribs$class, "custom-class")
})

# ==============================================================================
# Tests for render_label()
# ==============================================================================

test_that("render_label returns shiny tag", {
  result <- render_label("Test Label")
  expect_true(inherits(result, "shiny.tag"))
})

test_that("render_label uses default class", {
  result <- render_label("Test Label")
  expect_equal(result$attribs$class, "img-label")
})
