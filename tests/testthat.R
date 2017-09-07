library(testthat)
library(caliver)

##  Load the proto library for accessing sub-components of the ggplot2
library(ggplot2)
library(proto)

##  Load the visualTest library to get fingerprint of basic plots
library(visualTest)

# Test data
geff5tar <- system.file(file.path("testdata", "geff5.tar"), package = "caliver")
s <- import_geff_data_from_tar(geff5tar)

geff_pattern <- "^geff_reanalysis_an_fwis_fwi_1980010.*_0000_00.nc$"
geff5nc <- stack_netcdf_files(input_dir = tempdir(),
                              pattern = geff_pattern,
                              output_file = file.path(getwd(), "geff5.nc"))

test_check("caliver")

unlink(geff5nc)

# Static code analysis
# Integration with lintr: tests to fail if there are any lints in the project
if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}
