context("test-plot_percentiles.R")

test_that("plot_percentiles works", {
  x <- plot_percentiles(rstack1)
  expect_true(is.null(x))
})
