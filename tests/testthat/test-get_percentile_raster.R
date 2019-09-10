context("get_percentile_raster")

test_that("get_percentile_raster works with null inputs", {

  probs_maps <- try(get_percentile_raster(probs = 50,
                                          r = NULL,
                                          input_file = NULL), silent = TRUE)

  # Check whether the function exits correctly
  expect_equal(class(probs_maps), "try-error")

})

test_that("get_percentile_raster works with all non-null inputs", {

  probs_maps <- try(get_percentile_raster(probs = 50,
                                          r = rstack1,
                                          input_file = "x.nc"),
                    silent = TRUE)

  # Check whether the function exits correctly
  expect_true("RasterLayer" %in% class(probs_maps))

})

test_that("get_percentile_raster works with single prob from raster", {

  probs_maps_1_b <- get_percentile_raster(probs = 50, r = rstack1)

  # Check whether the class is correct
  expect_true("RasterLayer" %in% class(probs_maps_1_b))

  mean50 <- round(raster::cellStats(probs_maps_1_b, stat = "mean"), 0)
  expect_equal(mean50, 13)

})

test_that("get_percentile_raster works with multiple probs from raster", {

  probs_maps_2_b <- get_percentile_raster(probs = c(50, 75), r = rstack1)

  # Check whether the class is correct
  expect_equal("RasterBrick" %in% class(probs_maps_2_b), TRUE)

  mean50 <- round(raster::cellStats(probs_maps_2_b[[1]], stat = "mean"), 0)
  mean75 <- round(raster::cellStats(probs_maps_2_b[[2]], stat = "mean"), 0)
  expect_equal(mean50, 13)
  expect_equal(mean75, 17)

})

test_that("get_percentile_raster works with input file", {

  skip("Skip")
  tempfilepath <- tempfile(fileext = ".nc")
  raster::writeRaster(rstack1,
                      filename = tempfilepath,
                      format = "CDF", overwrite = TRUE)

  probs_maps_3_b <- get_percentile_raster(probs = 50,
                                          input_file = tempfilepath)

  mean50 <- round(raster::cellStats(probs_maps_3_b, stat = "mean"), 0)
  expect_equal(mean50, 13)

})
