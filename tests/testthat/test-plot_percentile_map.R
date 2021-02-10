context("test-plot_percentile_map.R")

test_that("plot_percentile_map works", {
  x <- plot_percentile_map(rstack1)
  expect_true(is.null(x))
})
