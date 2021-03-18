context("mask_crop_subset")

test_that("mask_crop_subset works", {

  # Check whether the stopping option works correctly
    x <- try(mask_crop_subset("r_risico", dummy_polygon), silent = TRUE)
    expect_equal(class(x), "try-error")

  # Check whether the stopping option works correctly
  x <- try(mask_crop_subset(r_risico, "dummy_polygon"), silent = TRUE)
  expect_equal(class(x), "try-error")

  # Check whether the result is correct if r is a RasterLayer
  x <- mask_crop_subset(r_risico, dummy_polygon)
  expect_equal(dim(x), c(288, 1019, 1))
  expect_equal(round(sum(raster::cellStats(x, sum, na.rm = TRUE)), 0),
               2125036)

  # Check whether the result is correct if r is a RasterStack
  x <- mask_crop_subset(r = raster::stack(r_risico, r_risico),
                        p = dummy_polygon)
  expect_equal(dim(x), c(288, 1019, 2))
  expect_equal(round(sum(raster::cellStats(x, sum, na.rm = TRUE)), 0),
               4250072)

  # Check whether the RasterStack converts to RasterBrick
  expect_equal("RasterBrick" %in% class(x), TRUE)

  # Check whether subset works
  x <- mask_crop_subset(r = raster::stack(r_risico, r_risico, r_risico),
                        p = dummy_polygon,
                        idx = 1:2)
  expect_equal(dim(x), c(288L, 1019L, 2L))
  expect_equal(round(sum(raster::cellStats(x, sum, na.rm = TRUE)), 0),
               4250072)

})
