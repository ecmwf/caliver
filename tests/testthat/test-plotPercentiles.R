context("plotPercentiles")

test_that("plotPercentiles works", {
  
  skip_on_appveyor()
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  # list.files(myTempDir)
  
  if (length(grep("est", list.files(myTempDir))) > 0) {
    file.remove(paste0(myTempDir, "/", 
                       list.files(myTempDir)[grep("est", 
                                                  list.files(myTempDir))]))
  }
  
  download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/testPercentile.nc", destfile = file.path(myTempDir, "testPercentile.nc"))
  
  inFile <- file.path(myTempDir, "testPercentile.nc")
  
  probsMaps <- getGriddedCDF(ncfile = inFile, 
                             probs = c(50, 75), 
                             outDir = myTempDir)
  
  p <- plotPercentiles(maps = probsMaps, rotateMap = TRUE)
  
  expect_equal(p$xlab, "Longitude")
  expect_equal(p$ylab, "Latitude")
  expect_equal(round(p$x.limits,0), c(-180, 180))
  expect_equal(round(p$y.limits,0), c(-90, 90))
  
})
