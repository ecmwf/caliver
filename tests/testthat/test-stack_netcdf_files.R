context("stack_netcdf_files")

test_that("stack_netcdf_files works", {
  
  myTempDir <- tempdir()
  download.file(url = paste0("https://dl.dropboxusercontent.com/u/23404805/",
                             "caliver_test_data/testA.nc"), 
                destfile = file.path(myTempDir, "testA.nc"))
  inFile <- file.path(myTempDir, "testA.nc")
  
  file.copy(from = inFile, to = file.path(myTempDir, "testB.nc"))
  
  mergedFile <- stack_netcdf_files(input_dir = myTempDir,
                                   pattern = "^test",
                                   output_file = file.path(myTempDir, "TestAB.nc"))
  
  x <- raster::stack(mergedFile)
  
  expect_equal(dim(x), c(256, 512, 2))
  
})
