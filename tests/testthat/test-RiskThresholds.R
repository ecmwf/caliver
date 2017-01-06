context("RiskThresholds")

test_that("RiskThresholds works", {
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  download.file(url = paste0("https://dl.dropboxusercontent.com/u/23404805/",
                             "caliver_test_data/outTest.nc"), 
                             destfile = file.path(myTempDir, "outTest.nc"))
  
  inFile <- file.path(myTempDir, "outTest.nc")
  
  # Generate probability maps
  probMaps <- getGriddedCDF(ncfile = file.path(myTempDir, "outTest.nc"), 
                            probs = c(50, 75, 90, 99), 
                            region = "GLOB",
                            mask = "fuel_model")
  
  RiskTableVar <- RiskThresholds(probMaps)
  
  expect_equal(round(as.numeric(RiskTableVar[2,2]), 3), 10.217)
  expect_equal(round(as.numeric(RiskTableVar[1,3]), 3), 0.079)
  
})
