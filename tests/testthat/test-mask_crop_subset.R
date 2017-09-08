context("mask_crop_subset")

test_that("mask_crop_subset works", {

  p <- raster::getData(name = "GADM", country = "Italy", level = 0)

  # Check whether the result is correct in case of TRUE TRUE
  x1 <- mask_crop_subset(s, p, mask = TRUE, crop = TRUE)
  expect_equal(dim(x1), c(16, 17, 5))
  y1 <- round(max(raster::cellStats(x1, sum, na.rm = TRUE)), 0)
  expect_equal(y1, 18)

  # Check whether the result is correct in case of TRUE FALSE
  x2 <- mask_crop_subset(s, p, mask = TRUE, crop = FALSE)
  expect_equal(dim(x2), c(256, 512, 5))
  y2 <- round(max(raster::cellStats(x2, sum, na.rm = TRUE)), 0)
  expect_equal(y2, 18)

  # Check whether the result is correct in case of FALSE FALSE
  x3 <- mask_crop_subset(s, p, mask = FALSE, crop = FALSE)
  expect_equal(dim(x3), c(256, 512, 5))
  y3 <- round(max(raster::cellStats(x3, sum, na.rm = TRUE)), 0)
  expect_equal(y3, 336257)

  # Check whether the result is correct in case of FALSE TRUE
  x4 <- mask_crop_subset(s, p, mask = FALSE, crop = TRUE)
  expect_equal(dim(x4), c(16, 17, 5))
  y4 <- round(max(raster::cellStats(x4, sum, na.rm = TRUE)), 0)
  expect_equal(y4, 83)

  # Check whether subset works
  x5 <- mask_crop_subset(s, p, mask = TRUE, crop = TRUE, idx = 1:3)
  expect_equal(dim(x5), c(16, 17, 3))
  y5 <- round(max(raster::cellStats(x5, sum, na.rm = TRUE)), 0)
  expect_equal(y5, 9)

})
