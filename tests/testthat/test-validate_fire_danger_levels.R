context("validate_fire_danger_levels")

test_that("validate_fire_danger_levels works", {

  # Check whether the result is correct in case of GLOB
  test <- validate_fire_danger_levels(fire_index = r_risico,
                                      observation = r_risico * 2,
                                      fire_threshold = 0.5,
                                      obs_threshold = 0.5)

  table_test <- table(test$pred, test$obs)
  expect_equal(as.vector(table_test), c(651L, 0L, 0L, 291094L))

})

test_that("validate_fire_danger_levels works with different resolutions", {

  r1_resampled <- raster::aggregate(r_risico, fact = 2)

  # Check whether the result is correct in case of GLOB
  test <- validate_fire_danger_levels(fire_index = r1_resampled,
                                      observation = r_risico * 2,
                                      fire_threshold = 10,
                                      obs_threshold = 0.5)

  table_test <- table(test$pred, test$obs)
  expect_equal(as.vector(table_test), c(27L, 0L, 9772L, 64699L))

})
