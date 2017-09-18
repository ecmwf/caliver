context("stack_netcdf_files")

test_that("stack_netcdf_files works", {

  geff0 <- try(stack_netcdf_files(input_dir = NULL,
                                  pattern = NULL,
                                  output_file = NULL,
                                  varname = NULL),
                silent = TRUE)
  expect_equal(class(geff0), "try_error")

  x <- raster::brick(geff5nc)
  expect_equal(dim(x), c(256, 512, 5))

})
