context("mask_with_fuelmodel")

test_that("mask_with_fuelmodel works in the range 0+360", {
  
  xmasked <- mask_with_fuelmodel(s[[1]])
  
  # Check whether layers are named correctly
  expect_equal(round(raster::cellStats(xmasked, mean),3), 7.582)
  
})

test_that("mask_with_fuelmodel works in the range -180+180", {
  
  y <- raster::rotate(s[[1]])
  
  ymasked <- mask_with_fuelmodel(y)
  
  # Check whether layers are rotated correctly
  expect_equal(round(raster::extent(ymasked),0)[1], -180)
  expect_equal(round(raster::extent(ymasked),0)[2], 180)
  
})
