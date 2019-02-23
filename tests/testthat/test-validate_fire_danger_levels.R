context("validate_fire_danger_levels")

test_that("validate_fire_danger_levels works", {

  # Check whether the result is correct in case of GLOB
  test <- validate_fire_danger_levels(fire_index = r1,
                                      observation = r2,
                                      fire_threshold = 10,
                                      obs_threshold = 0.5)

  table_test <- table(test$pred, test$obs)
  expect_equal(as.vector(table_test), c(1705, 3295))

})

test_that("validate_fire_danger_levels works with different resolutions", {

  r1_resampled <- raster::aggregate(r1, fact = 2)

  # Check whether the result is correct in case of GLOB
  test <- validate_fire_danger_levels(fire_index = r1_resampled,
                                      observation = r2,
                                      fire_threshold = 10,
                                      obs_threshold = 0.5)

  table_test <- table(test$pred, test$obs)
  expect_equal(as.vector(table_test), c(213, 1037))

})
