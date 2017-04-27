context("plotPDF")

myTempDir <- tempdir()
download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/outTest.nc", destfile = file.path(myTempDir, "outTest.nc"), method="curl")
inFile <- file.path(myTempDir, "outTest.nc")

x <- raster::raster(inFile)

test_that("plotPDF match expectations for no y-limit",{
  
  p <- plotPDF(fireIndex = x, countryName = "Italy", 
               thresholds = c(1,2,3,4,5))
  expect_equal(p$scales$scales[[2]]$limits, c(0, 40))
  
})

test_that("Plot layers match expectations",{
  
  p <- plotPDF(fireIndex = x, countryName = "Italy", 
               thresholds = c(1,2,3,4,5), upperLimit = 100)
  
  expect_equal(length(p$layers) == 3, TRUE)
  expect_identical(p$labels$y, "Density")
  expect_identical(p$labels$x, "FWI")
  expect_equal(p$scales$scales[[2]]$limits, c(0, 100))
  
})
