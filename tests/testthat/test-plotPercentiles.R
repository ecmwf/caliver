context("plotPercentiles")

test_that("plotPercentiles works", {
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  # list.files(myTempDir)
  
  if (length(grep("est", list.files(myTempDir))) > 0) {
    file.remove(paste0(myTempDir, "/", 
                       list.files(myTempDir)[grep("est", 
                                                  list.files(myTempDir))]))
  }
  
  inFile <- system.file("extdata", "testPercentile.nc", package="caliver")
  
  if (file.exists(inFile) == TRUE) {
    file.copy(from = inFile, to = paste0(myTempDir, "/testPercentile.nc"))
  }
  
  probsMaps <- getGriddedCDF(ncfile = inFile, 
                             probs = c(50, 75), 
                             outDir = myTempDir)
  
  p <- plotPercentiles(maps = probsMaps, rotateMap = TRUE)
  
  expect_equal(p$xlab, "Longitude")
  expect_equal(p$ylab, "Latitude")
  expect_equal(round(p$x.limits,0), c(-180, 180))
  expect_equal(round(p$y.limits,0), c(-90, 90))
  
})
