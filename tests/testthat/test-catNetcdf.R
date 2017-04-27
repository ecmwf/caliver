context("catNetcdf")

test_that("catNetcdf works", {
  
  myTempDir <- tempdir()
  download.file(url = paste0("https://dl.dropboxusercontent.com/u/23404805/",
                             "caliver_test_data/testA.nc"), 
                destfile = file.path(myTempDir, "testA.nc"))
  inFile <- file.path(myTempDir, "testA.nc")
  
  file.copy(from = inFile, to = file.path(myTempDir, "testB.nc"))
  
  mergedFile <- catNetcdf(inDir = myTempDir, 
                          pattern = "^test",
                          outFile = "TestAB.nc",
                          outDir = myTempDir)
  
  x <- raster::brick(mergedFile)
  
  expect_equal(dim(x), c(256, 512, 2))
  
})
