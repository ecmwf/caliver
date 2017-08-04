context("get_percentile_raster")

my_temp_dir <- tempdir() # works on all platforms with a platform-dependent result

download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/outTest.nc", destfile = file.path(my_temp_dir, "outTest.nc"), method="curl")

input_file <- file.path(my_temp_dir, "outTest.nc")

test_that("get_percentile_raster works with one prob", {
  
  probsMaps <- get_percentile_raster(input_file_path = input_file,
                                     probs = 50,
                                     output_dir = my_temp_dir)
  
  # Check whether layers are named correctly
  expect_equal(names(probsMaps), "FWI50")
  
  mean50 <- round(raster::cellStats(probsMaps$FWI50, stat = 'mean'), 0)
  expect_equal(mean50, 10)
  
})

test_that("get_percentile_raster works with multiple probs", {
  
  probsMaps <- get_percentile_raster(input_file_path = input_file,
                                     probs = c(50, 75),
                                     output_dir = my_temp_dir)
  
  # Check whether the class is correct
  expect_equal("RasterStack" %in% class(probsMaps), TRUE)
  
  # Check whether layers are named correctly
  expect_equal(names(probsMaps), c("FWI50", "FWI75"))
  
  mean50 <- round(raster::cellStats(probsMaps$FWI50, stat = 'mean'), 0)
  mean75 <- round(raster::cellStats(probsMaps$FWI75, stat = 'mean'), 0)
  expect_equal(mean50, 10)
  expect_equal(mean75, 17)
  
})
