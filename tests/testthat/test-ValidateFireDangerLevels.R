context("ValidateFireDangerLevels")

test_that("ValidateFireDangerLevels works", {
  
  skip_on_travis()
  
  inFile <- tempfile()
  download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/outTest.nc", destfile = inFile)
  
})
