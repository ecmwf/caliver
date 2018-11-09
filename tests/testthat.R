Sys.setenv("R_TESTS" = "")

library(testthat)
library(caliver)
library(raster)

##  Load the proto library for accessing sub-components of the ggplot2
library(ggplot2)
library(proto)

##  Load the visualTest library to get fingerprint of basic plots
library(visualTest)

test_check("caliver")

# Static code analysis
# Integration with lintr: tests to fail if there are any lints in the project
if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}
