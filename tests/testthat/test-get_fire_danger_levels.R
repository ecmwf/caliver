context("get_fire_danger_levels")

test_that("get_fire_danger_levels works", {

  # Check whether the result is correct
  x <- get_fire_danger_levels(b)
  expect_equal(x, c(3, 9, 23, 49, 98))

})
