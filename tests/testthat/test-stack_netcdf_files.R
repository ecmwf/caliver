context("stack_netcdf_files")

test_that("stack_netcdf_files works", {

  temporary_dir <- tempdir()
  # Generate dummy nc files to test stak_netcdf_files()
  raster::writeRaster(r1, filename = file.path(temporary_dir, "r1.nc"),
                      format = "CDF", overwrite = TRUE)
  raster::writeRaster(r2, filename = file.path(temporary_dir, "r2.nc"),
                      format = "CDF", overwrite = TRUE)
  raster::writeRaster(r3, filename = file.path(temporary_dir, "r3.nc"),
                      format = "CDF", overwrite = TRUE)

  # Test whether stop options work correctly
  geff0 <- try(stack_netcdf_files(input_dir = NULL,
                                  pattern = NULL,
                                  output_file = NULL,
                                  varname = NULL),
                silent = TRUE)
  expect_equal(class(geff0), "try-error")

  skip("Skip")
  # Test whether the function generates the correct default name
  geff5vr <- stack_netcdf_files(input_dir = temporary_dir,
                                pattern = "r[0-9]*.nc",
                                varname = NULL,
                                output_file = file.path(temporary_dir,
                                                        "outfile.nc"))
  expect_equal("RasterStack" %in% class(geff5vr))
  # Test whether the function generates the correct dimensions
  expect_equal(dim(geff5vr), c(50, 100, 3))

})
