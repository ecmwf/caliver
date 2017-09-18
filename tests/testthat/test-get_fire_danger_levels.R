context("get_fire_danger_levels")

test_that("get_fire_danger_levels works", {

  # Check whether the stop option works correctly
  s0 <- raster::setValues(x = s, values = matrix(NA, nrow = 131072, ncol = 5))
  x0 <- get_fire_danger_levels(s0)
  expect_equal(x0, c(NA, NA, NA, NA, NA))

  # Check whether the result is correct
  x <- get_fire_danger_levels(s)
  expect_equal(x, c(3, 8, 18, 38, 73))

})
