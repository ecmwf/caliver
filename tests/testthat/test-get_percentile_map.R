context("get_percentile_map")

test_that("get_percentile_map should not work if probs are > 1", {
  
  probs_maps <- try(get_percentile_map(r = b, probs = 1.5), silent = TRUE)
  
  # Check output
  expect_true("try-error" %in% class(probs_maps))
  
})

test_that("get_percentile_map works with one date", {
  
  probs_maps <- get_percentile_map(r = b[[1]], probs = 0.5)
  
  # Check output
  expect_true(cellStats(b[[1]] - probs_maps, "sum") == 0)
  
})

test_that("get_percentile_map works with all non-null inputs", {

  probs_maps <- get_percentile_map(r = b, probs = 0.50)

  # Check output data type
  expect_true("RasterLayer" %in% class(probs_maps))

  # Check percentile value
  mean50 <- round(raster::cellStats(probs_maps, stat = "mean"), 0)
  expect_equal(mean50, 48)

})

test_that("get_percentile_map works with multiple probs", {

  probs_maps <- get_percentile_map(r = b, probs = c(0.75, 0.95))

  # Check whether the class is correct
  expect_equal("RasterBrick" %in% class(probs_maps), TRUE)

  mean75 <- round(raster::cellStats(probs_maps[[1]], stat = "mean"), 0)
  mean95 <- round(raster::cellStats(probs_maps[[2]], stat = "mean"), 0)
  expect_equal(mean75, 76)
  expect_equal(mean95, 95)

})

test_that("get_percentile_map works with clima", {

  probs_maps <- get_percentile_map(r = clima, probs = c(0.50, 0.75))

  # Check whether the class is correct
  expect_true("list" %in% class(probs_maps))
  expect_true("RasterBrick" %in% class(probs_maps[[1]]))

  mean50 <- round(raster::cellStats(probs_maps[[1]], stat = "mean"), 0)[1]
  mean75 <- round(raster::cellStats(probs_maps[[2]], stat = "mean"), 0)[2]
  expect_equal(as.numeric(mean50), 55)
  expect_equal(as.numeric(mean75), 64)

})

test_that("get_percentile_map works with clima, single date", {

  probs_maps <- get_percentile_map(r = clima[[1]], probs = c(0.50, 0.75))

  # Check whether the class is correct
  expect_true("RasterBrick" %in% class(probs_maps))

  mean50 <- round(raster::cellStats(probs_maps[[1]], stat = "mean"), 0)
  mean75 <- round(raster::cellStats(probs_maps[[2]], stat = "mean"), 0)
  expect_equal(as.numeric(mean50), 55)
  expect_equal(as.numeric(mean75), 70)

})

test_that("get_percentile_map works with clima, single date single prob", {

  probs_maps <- get_percentile_map(r = clima[[1]], probs = 0.50)

  # Check whether the class is correct
  expect_true("RasterLayer" %in% class(probs_maps))

  meanprobs <- round(raster::cellStats(probs_maps, stat = "mean"), 0)
  expect_equal(as.numeric(meanprobs), 55)

})
