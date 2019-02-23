context("mask_with_fuelmodel")

test_that("mask_with_fuelmodel works in the range 0+360", {

  xmasked <- mask_with_fuelmodel(rstack1[[1]])

  # Check whether layers are named correctly
  expect_equal(round(raster::cellStats(xmasked, mean), 3), 13.103)

  # Check whether longitudes are correct
  expect_equal(round(raster::extent(xmasked)@xmin, 1), 0)
  expect_equal(round(raster::extent(xmasked)@xmax, 1), 360)

})

test_that("mask_with_fuelmodel works in the range -180+180", {

  y <- raster::rotate(rstack1[[1]])

  ymasked <- mask_with_fuelmodel(y)

  # Check whether layers are rotated correctly
  expect_equal(round(raster::extent(ymasked)@xmin, 1), -180)
  expect_equal(round(raster::extent(ymasked)@xmax, 1), 180)

})
