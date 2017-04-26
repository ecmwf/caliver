context("FireDangerLevels")

test_that("FireDangerLevels works", {
  
  inFile <- tempfile()
  download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/outTest.nc", destfile = inFile)
  
  r <- raster::rotate(raster::brick(inFile))
  
  # Check whether the result is correct in case of TRUE TRUE
  x <- FireDangerLevels(r)
  expect_equal(x, c(3, 8, 20, 41, 81))
  
})
