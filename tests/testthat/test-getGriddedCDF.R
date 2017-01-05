context("getGriddedCDF")

test_that("getGriddedCDF works", {
  
  skip_on_appveyor()
  # skip_on_cran() # this takes long to download!
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/outTest.nc", destfile = file.path(myTempDir, "outTest.nc"))
  
  inFile <- file.path(myTempDir, "outTest.nc")
  
  probsMaps <- getGriddedCDF(ncfile = inFile, 
                             probs = c(50, 75), 
                             outDir = myTempDir)
  
  expect_equal(length(probsMaps) == 2, TRUE)
  expect_equal(names(probsMaps), c("OUTTEST_50th_percentile", 
                                   "OUTTEST_75th_percentile"))
  expect_equal("RasterLayer" %in% class(probsMaps$OUTTEST_50th_percentile), 
               TRUE)
  expect_equal("RasterLayer" %in% class(probsMaps$OUTTEST_75th_percentile), 
               TRUE)
  
  mean50 <- round(raster::cellStats(probsMaps$OUTTEST_50th_percentile, 
                                    stat = 'mean'), 0)
  mean75 <- round(raster::cellStats(probsMaps$OUTTEST_75th_percentile, 
                                    stat = 'mean'), 0)
  expect_equal(mean50, 10)
  expect_equal(mean75, 17)
  
})
