context("plotPercentiles")

myTempDir <- tempdir() # works on all platforms with a platform-dependent result

download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/outTest.nc", destfile = file.path(myTempDir, "outTest.nc"), method="curl")

inFile <- file.path(myTempDir, "outTest.nc")

probsMaps <- makePercentileRaster(inFilePath = inFile, 
                                  probs = 50,
                                  outDir = myTempDir)

test_that("plotPercentiles works with rotateMap = TRUE", {
  
  test1 <- file.path(myTempDir, "test1.png")
  
  png(filename = test1)
  plotPercentiles(maps = probsMaps, rotateMap = TRUE)
  dev.off()
  
  isSimilar(file = test1, fingerprint = "B30F6C1B71ECC4B0", threshold = 0.1)
  
})

test_that("plotPercentiles works with rotateMap = FALSE", {
  
  test2 <- file.path(myTempDir, "test2.png")
  
  png(filename = test2)
  plotPercentiles(maps = probsMaps, rotateMap = FALSE)
  dev.off()
  
  isSimilar(file = test2, fingerprint = "B32B784FCCCCC4D0", threshold = 0.1)
  
})

test_that("plotPercentiles works with region = EURO", {
  
  test3 <- file.path(myTempDir, "test3.png")
  
  png(filename = test3)
  plotPercentiles(maps = probsMaps, rotateMap = TRUE, region = "EURO")
  dev.off()
  
  # getFingerprint(test3)
  isSimilar(file = test3, fingerprint = "AF0F306FF8B0C4D0", threshold = 0.1)
  
})

test_that("plotPercentiles works with multi probs", {
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/outTest.nc", destfile = file.path(myTempDir, "outTest.nc"), method="curl")
  
  inFile <- file.path(myTempDir, "outTest.nc")
  
  probsMaps <- makePercentileRaster(inFilePath = inFile, 
                                    probs = c(50, 90),
                                    outDir = myTempDir)
  
  test4 <- file.path(myTempDir, "test4.png")
  
  png(filename = test4)
  plotPercentiles(maps = probsMaps, rotateMap = TRUE, region = "EURO")
  dev.off()
  
  # getFingerprint(test4)
  isSimilar(file = test4, fingerprint = "A0092F017F6F196F", threshold = 0.1)
  
})
