context("get_percentile_raster")

test_that("get_percentile_raster works with null inputs", {

  probsMaps <- try(get_percentile_raster(probs = 50,
                                         input_raster = NULL,
                                         input_file_path = NULL), silent = TRUE)

  # Check whether the function exits correctly
  expect_equal(class(probsMaps), "try-error")

})

test_that("get_percentile_raster works with all non-null inputs", {

  probsMaps <- try(get_percentile_raster(probs = 50,
                                         input_raster = b,
                                         input_file_path = "x.nc"),
                   silent = TRUE)

  # Check whether the function exits correctly
  expect_equal(class(probsMaps), "try-error")

})

test_that("get_percentile_raster works with single prob from nc file", {

  probsMaps1A <- get_percentile_raster(probs = 50,
                                       input_file_path = geff5nc)

  # Check whether the class is correct
  expect_equal("RasterLayer" %in% class(probsMaps1A), TRUE)

  # Check whether layers are named correctly
  # expect_equal(names(probsMaps1A), "FWI50")

  mean50 <- round(raster::cellStats(probsMaps1A, stat = 'mean'), 0)
  expect_equal(mean50, 26)

})

test_that("get_percentile_raster works with multiple probs from nc file", {

  probsMaps2A <- get_percentile_raster(probs = c(50, 75),
                                       input_file_path = geff5nc)

  # Check whether the class is correct
  expect_equal("RasterBrick" %in% class(probsMaps2A), TRUE)

  # Check whether layers are named correctly
  # expect_equal(names(probsMaps2A), c("FWI50", "FWI75"))

  mean50 <- round(raster::cellStats(probsMaps2A[[1]], stat = 'mean'), 0)
  mean75 <- round(raster::cellStats(probsMaps2A[[2]], stat = 'mean'), 0)
  expect_equal(mean50, 26)
  expect_equal(mean75, 30)

})

test_that("get_percentile_raster works with single prob from raster file", {

  probsMaps1B <- get_percentile_raster(probs = 50, input_raster = b)

  # Check whether the class is correct
  expect_equal("RasterLayer" %in% class(probsMaps1B), TRUE)

  # Check whether layers are named correctly
  expect_equal(names(probsMaps1B), "FWI50")

  mean50 <- round(raster::cellStats(probsMaps1B, stat = 'mean'), 0)
  expect_equal(mean50, 7)

})

test_that("get_percentile_raster works with multiple probs from raster file", {

  probsMaps2B <- get_percentile_raster(probs = c(50, 75), input_raster = b)

  # Check whether the class is correct
  expect_equal("RasterBrick" %in% class(probsMaps2B), TRUE)

  # Check whether layers are named correctly
  expect_equal(names(probsMaps2B), c("FWI50", "FWI75"))

  mean50 <- round(raster::cellStats(probsMaps2B$FWI50, stat = 'mean'), 0)
  mean75 <- round(raster::cellStats(probsMaps2B$FWI75, stat = 'mean'), 0)
  expect_equal(mean50, 7)
  expect_equal(mean75, 9)

})
