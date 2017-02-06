context("shiftMap")

x <- "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/outTest.nc"

test_that("shiftMap works", {
  
  skip_on_appveyor()
  
  # works on all platforms with a platform-dependent result
  myTempDir <- tempdir() 
  
  inFile <- file.path(myTempDir, "outTest.nc")
  
  download.file(url = x, destfile = inFile, method="curl")
  
  x <- raster::raster(inFile)
  
  expect_equal(round(x@extent@xmin), 0)
  expect_equal(round(x@extent@xmax), 360)
  
  y <- raster::raster(shiftMap(inFile = inFile, outDir = myTempDir))
  
  expect_equal(round(y@extent@xmin), -180)
  expect_equal(round(y@extent@xmax), 180)
  
})
