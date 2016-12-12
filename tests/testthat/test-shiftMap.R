context("shiftMap")

test_that("shiftMap works", {
  
  skip_on_appveyor()
  
  inFile <- system.file("extdata", "outTest.nc", package="caliver")
  
  x <- raster::raster(inFile)
  
  expect_equal(round(x@extent@xmin), 0)
  expect_equal(round(x@extent@xmax), 360)
  
  myTempDir <- tempdir()
  
  y <- raster::raster(shiftMap(inFile = inFile, outDir = myTempDir))
  
  expect_equal(round(y@extent@xmin), -180)
  expect_equal(round(y@extent@xmax), 180)
  
})
