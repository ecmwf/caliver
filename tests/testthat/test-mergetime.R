context("mergetime")

test_that("mergetime works", {
  
  skip_on_travis()
  
  skip_on_appveyor()
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  # list.files(myTempDir)
  
  if (length(grep("test", list.files(myTempDir))) > 0) {
    file.remove(file.path(myTempDir, list.files(myTempDir)[grep("test", 
                                                  list.files(myTempDir))]))
  }
  
  download.file(url = paste0("https://dl.dropboxusercontent.com/u/23404805/",
                             "caliver_test_data/testA.nc"), 
                destfile = file.path(myTempDir, "testA.nc"))
  
  inFile <- file.path(myTempDir, "testA.nc")
  
  file.copy(from = inFile, to = file.path(myTempDir, "testB.nc"))
  
  mergedFile <- mergetime(dirs = myTempDir, 
                          startingString = "test", 
                          outFile = "TestAB.nc",
                          outDir = myTempDir)
  
  expect_equal(dim(raster::stack(mergedFile)), c(256, 512, 2))
  
})
