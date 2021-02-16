context("test-plot_percentile_map.R")

test_that("plot_percentile_map works", {
  x <- plot_percentile_map(b)
  expect_true(is.null(x))
})
