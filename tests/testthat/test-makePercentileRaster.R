context("makePercentileRaster")

test_that("makePercentileRaster works", {
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/outTest.nc", destfile = file.path(myTempDir, "outTest.nc"), method="curl")
  
  inFile <- file.path(myTempDir, "outTest.nc")
  
  probsMaps <- makePercentileRaster(inFilePath = inFile, 
                                    probs = c(50, 75),
                                    outDir = myTempDir,
                                    mask = "")
  
  # Check whether the class is correct
  expect_equal("RasterStack" %in% class(probsMaps), TRUE)
  
  # Check whether layers are named correctly
  expect_equal(names(probsMaps), c("FWI50", "FWI75"))
  
  mean50 <- round(raster::cellStats(probsMaps$FWI50, stat = 'mean'), 0)
  mean75 <- round(raster::cellStats(probsMaps$FWI75, stat = 'mean'), 0)
  expect_equal(mean50, 10)
  expect_equal(mean75, 17)
  
})
