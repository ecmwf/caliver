context("get_fire_danger_levels")

test_that("get_fire_danger_levels works", {

  # Check whether the result is correct
  x <- get_fire_danger_levels(rstack1)
  expect_equal(x, c(1, 4, 8, 15, 25))

})
