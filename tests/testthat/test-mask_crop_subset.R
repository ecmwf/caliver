context("mask_crop_subset")

test_that("mask_crop_subset works", {

  p <- raster::getData("GADM", country = "IT", level = 0)

  # Check whether the stopping option works correctly
  x0 <- try(mask_crop_subset("s", p, mask = TRUE, crop = TRUE), silent = TRUE)
  expect_equal(class(x0), "try-error")

  # Check whether the result is correct in case of FALSE FALSE
  x1a <- mask_crop_subset(r1, p, mask = FALSE, crop = FALSE)
  expect_equal(dim(x1a), c(50, 100, 1))
  x1b <- mask_crop_subset(rstack1, p, mask = FALSE, crop = FALSE)
  expect_equal(dim(x1b), c(50, 100, 5))

  # Check whether the result is correct in case of FALSE TRUE
  x2a <- mask_crop_subset(rstack1, p, mask = FALSE, crop = TRUE)
  expect_equal(dim(x2a), c(3, 3, 5))
  x2b <- round(sum(raster::cellStats(x2a, sum, na.rm = TRUE)), 0)
  expect_equal(x2b, 583)

  # Check whether the result is correct in case of TRUE FALSE
  x3a <- mask_crop_subset(rstack1, p, mask = TRUE, crop = FALSE)
  expect_equal(dim(x3a), c(50, 100, 5))
  x3b <- round(sum(raster::cellStats(x3a, sum, na.rm = TRUE)), 0)
  expect_equal(x3b, 133)

  # Check whether the result is correct in case of TRUE TRUE
  x4a <- mask_crop_subset(r1, p, mask = TRUE, crop = TRUE)
  expect_equal(dim(x4a), c(3, 3, 1))
  x4b <- round(sum(raster::cellStats(x4a, sum, na.rm = TRUE)), 0)
  expect_equal(x4b, 45)
  x4c <- mask_crop_subset(rstack1, p, mask = TRUE, crop = TRUE)
  expect_equal(dim(x4c), c(3, 3, 5))
  x4d <- round(sum(raster::cellStats(x4c, sum, na.rm = TRUE)), 0)
  expect_equal(x4d, 133)
  # Check whether the stack converts to brick
  expect_equal("RasterBrick" %in% class(x4c), TRUE)

  # Check whether subset works
  x5a <- mask_crop_subset(rstack1, p, mask = TRUE, crop = TRUE, idx = 1:3)
  expect_equal(dim(x5a), c(3, 3, 3))
  x5b <- round(sum(raster::cellStats(x5a, sum, na.rm = TRUE)), 0)
  expect_equal(x5b, 76)

})
