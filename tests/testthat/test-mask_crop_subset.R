context("mask_crop_subset")

test_that("mask_crop_subset works", {

  # Check whether the stopping option works correctly
  x0 <- try(mask_crop_subset("s", shape), silent = TRUE)
  expect_equal(class(x0), "try-error")

  # Check whether the result is correct
  x1a <- mask_crop_subset(r_risico, shape)
  expect_equal(dim(x1a), c(288, 832, 1))
  x1b <- round(sum(raster::cellStats(x1a, sum, na.rm = TRUE)), 0)
  expect_equal(x1b, 2125036)
  x1c <- mask_crop_subset(r = raster::stack(r_risico, r_risico), p = shape)
  expect_equal(dim(x1c), c(288L, 832L, 2L))
  x1d <- round(sum(raster::cellStats(x1c, sum, na.rm = TRUE)), 0)
  expect_equal(x1d, 4250072)
  # Check whether the stack converts to brick
  expect_equal("RasterBrick" %in% class(x1c), TRUE)

  # Check whether subset works
  x2a <- mask_crop_subset(r = raster::stack(r_risico, r_risico, r_risico),
                          p = shape, idx = 1:2)
  expect_equal(dim(x2a), c(288L, 832L, 2L))
  expect_equal(round(sum(raster::cellStats(x2a, sum, na.rm = TRUE)), 0),
               4250072)

})
