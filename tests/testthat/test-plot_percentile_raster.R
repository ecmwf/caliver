context("test-plot_percentile_raster.R")

test_that("plot_percentile_raster works", {
  x <- plot_percentile_raster(rstack1)
  expect_true(is.null(x))
})
