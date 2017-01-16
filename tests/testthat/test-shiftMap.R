context("shiftMap")

test_that("shiftMap works", {
  
  skip_on_appveyor()
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  download.file(url = paste0("https://dl.dropboxusercontent.com/u/23404805/", 
                             "caliver_test_data/outTest.nc"), 
                destfile = file.path(myTempDir, "outTest.nc"))
  
  inFile <- file.path(myTempDir, "outTest.nc")
  
  x <- raster::raster(inFile)
  
  expect_equal(round(x@extent@xmin), 0)
  expect_equal(round(x@extent@xmax), 360)
  
  myTempDir <- tempdir()
  
  y <- raster::raster(shiftMap(inFile = inFile, outDir = myTempDir))
  
  expect_equal(round(y@extent@xmin), -180)
  expect_equal(round(y@extent@xmax), 180)
  
})
