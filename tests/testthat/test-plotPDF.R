context("plotPDF")

myTempDir <- tempdir() # works on all platforms with a platform-dependent result

download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/outTest.nc", destfile = file.path(myTempDir, "outTest.nc"), method="curl")

inFile <- file.path(myTempDir, "outTest.nc")

x <- raster::raster(inFile)

test_that("plotPDF works with rotateMap = TRUE", {
  
  test1 <- file.path(myTempDir, "test1.png")
  
  png(filename = test1)
  plotPDF(x, "Italy", c(1,2,3,4,5), upperLimit = 100)
  dev.off()
  
  # Check whether layers are named correctly
  # getFingerprint(test1)
  isSimilar(file = test1, fingerprint = "D2EDAD31859233CA", threshold = 0.1)
  
})
