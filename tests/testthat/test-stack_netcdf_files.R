context("stack_netcdf_files")

test_that("stack_netcdf_files works", {

  # Test whether stop options work correctly
  geff0 <- try(stack_netcdf_files(input_dir = NULL,
                                  pattern = NULL,
                                  output_file = NULL,
                                  varname = NULL),
                silent = TRUE)
  expect_equal(class(geff0), "try-error")

  x <- raster::brick(geff5nc)
  expect_equal(dim(x), c(256, 512, 5))

})
