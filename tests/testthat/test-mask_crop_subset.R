context("mask_crop_subset")

test_that("mask_crop_subset works", {
  
  inFile <- tempfile()
  download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/outTest.nc", destfile = inFile)
  
  r <- raster::rotate(raster::brick(inFile))
  p <- raster::getData(name = "GADM", country = "Italy", level = 0)
  
  # Check whether the result is correct in case of TRUE TRUE
  x1 <- mask_crop_subset(r, p, mask = TRUE, crop = TRUE)
  expect_equal(dim(x1), c(16, 17, 304))
  y1 <- round(max(raster::cellStats(x1, sum, na.rm = TRUE)), 0)
  expect_equal(y1 == 1370, TRUE)
  
  # Check whether the result is correct in case of TRUE FALSE
  x2 <- mask_crop_subset(r, p, mask = TRUE, crop = FALSE)
  expect_equal(dim(x2), c(256, 512, 304))
  y2 <- round(max(raster::cellStats(x2, sum, na.rm = TRUE)), 0)
  expect_equal(y2 == 1370, TRUE)
  
  # Check whether the result is correct in case of FALSE FALSE
  x3 <- mask_crop_subset(r, p, mask = FALSE, crop = FALSE)
  expect_equal(dim(x3), c(256, 512, 304))
  y3 <- round(max(raster::cellStats(x3, sum, na.rm = TRUE)), 0)
  expect_equal(y3 == 473244, TRUE)
  
  # Check whether the result is correct in case of FALSE TRUE
  x4 <- mask_crop_subset(r, p, mask = FALSE, crop = TRUE)
  expect_equal(dim(x4), c(16, 17, 304))
  y4 <- round(max(raster::cellStats(x4, sum, na.rm = TRUE)), 0)
  expect_equal(y4 == 2559, TRUE)
  
  # Check whether subset works
  x5 <- mask_crop_subset(r, p, mask = TRUE, crop = TRUE, idx = 1:10)
  expect_equal(dim(x5), c(16, 17, 10))
  y5 <- round(max(raster::cellStats(x5, sum, na.rm = TRUE)), 0)
  expect_equal(y5 == 39, TRUE)
  
})
