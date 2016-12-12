context("getGriddedCDF")

test_that("getGriddedCDF works", {
  
  skip_on_appveyor()
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  inFile <- system.file("extdata", "outTest.nc", package="caliver")
  
  probsMaps <- getGriddedCDF(ncfile = inFile, 
                             probs = c(50, 75), 
                             outDir = myTempDir)
  
  # list.files(myTempDir)
  
  expect_equal(length(grep("outTest", list.files(myTempDir))), 2)
  
})
