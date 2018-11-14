context("stack_netcdf_files")

test_that("stack_netcdf_files works", {

  # Test whether stop options work correctly
  geff0 <- try(stack_netcdf_files(input_dir = NULL,
                                  pattern = NULL,
                                  output_file = NULL,
                                  varname = NULL),
                silent = TRUE)
  expect_equal(class(geff0), "try-error")

  # Test whether the function generates the correct default name
  geff5vr <- stack_netcdf_files(input_dir = tempdir(),
                                pattern = "r[0-9]*.nc",
                                varname = NULL,
                                output_file = NULL)
  expect_equal(basename(geff5vr), "outfile.nc")

  # Test whether the function generates the correct result
  x <- raster::brick("outfile.nc")
  expect_equal(dim(x), c(50, 100, 3))

  unlink(geff5vr)

})
