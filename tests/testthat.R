# ==============================================================================
# Test Runner for Indel Signature Browser
# Run with: testthat::test_dir("tests/testthat")
# ==============================================================================

library(testthat)
library(shiny)

# Source required files
source("../../config.R")
source("../../server/helpers.R")
source("../../server/validation.R")

test_check("indel-signature-browser")
