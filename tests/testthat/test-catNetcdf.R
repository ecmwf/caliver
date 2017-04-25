context("catNetcdf")

test_that("catNetcdf works", {
  
  skip_on_travis()
  
  myTempDir <- tempdir()
  
  if (length(grep("test", list.files(myTempDir))) > 0) {
    file.remove(file.path(myTempDir, list.files(myTempDir)[grep("test", 
                                                  list.files(myTempDir))]))
  }
  
  download.file(url = paste0("https://dl.dropboxusercontent.com/u/23404805/",
                             "caliver_test_data/testA.nc"), 
                destfile = file.path(myTempDir, "testA.nc"), method="curl")
  
  inFile <- file.path(myTempDir, "testA.nc")
  
  file.copy(from = inFile, to = file.path(myTempDir, "testB.nc"))
  
  mergedFile <- catNetcdf(inDir = myTempDir, 
                          startingString = "test",
                          outFile = "TestAB.nc",
                          outDir = myTempDir)
  
  expect_equal(dim(raster::stack(mergedFile)), c(256, 512, 2))
  
})
