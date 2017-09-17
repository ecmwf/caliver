context("get_percentile_raster")

test_that("get_percentile_raster works with null inputs", {

  probs_maps <- try(get_percentile_raster(probs = 50,
                                         input_raster = NULL,
                                         input_file_path = NULL), silent = TRUE)

  # Check whether the function exits correctly
  expect_equal(class(probs_maps), "try-error")

})

test_that("get_percentile_raster works with all non-null inputs", {

  probs_maps <- try(get_percentile_raster(probs = 50,
                                         input_raster = s,
                                         input_file_path = "x.nc"),
                   silent = TRUE)

  # Check whether the function exits correctly
  expect_equal(class(probs_maps), "try-error")

})

test_that("get_percentile_raster works with single prob from raster file", {

  probs_maps_1_b <- get_percentile_raster(probs = 50, input_raster = s)

  # Check whether the class is correct
  expect_equal("RasterLayer" %in% class(probs_maps_1_b), TRUE)

  # Check whether layers are named correctly
  expect_equal(names(probs_maps_1_b), "FWI50")

  mean50 <- round(raster::cellStats(probs_maps_1_b, stat = "mean"), 0)
  expect_equal(mean50, 7)

})

test_that("get_percentile_raster works with multiple probs from raster file", {

  probs_maps_2_b <- get_percentile_raster(probs = c(50, 75), input_raster = s)

  # Check whether the class is correct
  expect_equal("RasterBrick" %in% class(probs_maps_2_b), TRUE)

  # Check whether layers are named correctly
  expect_equal(names(probs_maps_2_b), c("FWI50", "FWI75"))

  mean50 <- round(raster::cellStats(probs_maps_2_b$FWI50, stat = "mean"), 0)
  mean75 <- round(raster::cellStats(probs_maps_2_b$FWI75, stat = "mean"), 0)
  expect_equal(mean50, 7)
  expect_equal(mean75, 9)

})

test_that("get_percentile_raster works with single prob from nc file", {

  skip("skip temporarily for testing")

  probs_maps_1_a <- get_percentile_raster(probs = 50,
                                       input_file_path = geff5nc)

  # Check whether the class is correct
  expect_equal("RasterLayer" %in% class(probs_maps_1_a), TRUE)

  # Check whether layers are named correctly
  expect_equal(names(probs_maps_1_a), "FWI50")

  mean50 <- round(raster::cellStats(probs_maps_1_a, stat = "mean"), 0)
  expect_equal(mean50, 26)

})

test_that("get_percentile_raster works with multiple probs from nc file", {

  skip("skip temporarily for testing")

  probs_maps_2_a <- get_percentile_raster(probs = c(50, 75),
                                       input_file_path = geff5nc)

  # Check whether the class is correct
  expect_equal("RasterBrick" %in% class(probs_maps_2_a), TRUE)

  # Check whether layers are named correctly
  expect_equal(names(probs_maps_2_a), c("FWI50", "FWI75"))

  mean50 <- round(raster::cellStats(probs_maps_2_a[[1]], stat = "mean"), 0)
  mean75 <- round(raster::cellStats(probs_maps_2_a[[2]], stat = "mean"), 0)
  expect_equal(mean50, 26)
  expect_equal(mean75, 30)

})
