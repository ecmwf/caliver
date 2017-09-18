context("validate_fire_danger_levels")

x <- raster::raster(ncols = 2, nrows = 2)
x[] <- c(180, 8, 1, 10)

y <- raster::raster(ncols = 2, nrows = 2)
y[] <- c(0, 0.8, 1, 0.3)

test_that("validate_fire_danger_levels works", {
  
  # Check whether the result is correct in case of GLOB
  tableTest <- validate_fire_danger_levels(fire_index = x,
                                           observation = y,
                                           fire_threshold = 10,
                                           obs_threshold = 0.5)

  expect_equal(as.vector(tableTest), c(0, 2, 2, 0))
  
})
