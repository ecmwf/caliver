context("stack_netcdf_files")

test_that("stack_netcdf_files works", {

  x <- raster::brick(geff5nc)

  expect_equal(dim(x), c(256, 512, 5))

})
