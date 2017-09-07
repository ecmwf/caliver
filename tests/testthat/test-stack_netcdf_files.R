context("stack_netcdf_files")

test_that("stack_netcdf_files works", {

  # my_temp_dir <- tempdir()
  # 
  # file.copy(from = geff5nc,
  #           to = file.path(my_temp_dir, "teststackA.nc"),
  #           overwrite = TRUE)
  # 
  # file.copy(from = geff5nc,
  #           to = file.path(my_temp_dir, "teststackB.nc"),
  #           overwrite = TRUE)
  # 
  # merged_file <- stack_netcdf_files(input_dir = my_temp_dir,
  #                                   varname = NULL,
  #                                   pattern = "^teststack.*.nc$",
  #                                   recursive = FALSE,
  #                                   output_file = NULL)
  # print(merged_file)
  x <- raster::brick(geff5nc)

  expect_equal(dim(x), c(256, 512, 5))

})
