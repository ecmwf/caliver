context("mask_crop_subset")

test_that("mask_crop_subset works", {

  # Check whether the stopping option works correctly
  x0 <- try(mask_crop_subset("s", shape), silent = TRUE)
  expect_equal(class(x0), "try-error")

  # Check whether the result is correct
  x1a <- mask_crop_subset(r1, shape)
  expect_equal(dim(x1a), c(3, 3, 1))
  x1b <- round(sum(raster::cellStats(x1a, sum, na.rm = TRUE)), 0)
  expect_equal(x1b, 112)
  x1c <- mask_crop_subset(rstack1, shape)
  expect_equal(dim(x1c), c(3, 3, 5))
  x1d <- round(sum(raster::cellStats(x1c, sum, na.rm = TRUE)), 0)
  expect_equal(x1d, 583)
  # Check whether the stack converts to brick
  expect_equal("RasterBrick" %in% class(x1c), TRUE)

  # Check whether subset works
  x2a <- mask_crop_subset(rstack1, shape, idx = 1:3)
  expect_equal(dim(x2a), c(3, 3, 3))
  expect_equal(round(sum(raster::cellStats(x2a, sum, na.rm = TRUE)), 0), 341)

})
