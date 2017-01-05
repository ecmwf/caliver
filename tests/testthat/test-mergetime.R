context("mergetime")

test_that("mergetime works", {
  
  skip_on_appveyor()
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  # list.files(myTempDir)
  
  if (length(grep("test", list.files(myTempDir))) > 0) {
    file.remove(paste0(myTempDir, "/", 
                       list.files(myTempDir)[grep("test", 
                                                  list.files(myTempDir))]))
  }
  
  download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/testA.nc", destfile = file.path(myTempDir, "testA.nc"))
  inFile <- file.path(myTempDir, "testA.nc")
  
  file.copy(from = inFile, to = paste0(myTempDir, "/testB.nc"))
  
  mergedFile <- mergetime(dirs = myTempDir, 
                          startingString = "test", 
                          outFile = "TestAB.nc",
                          outDir = myTempDir)
  
  # print(mergedFile)
  
  x <- ncdf4::nc_open(mergedFile)
  
  expect_equal(x$dim$time$len, 2)
  
  file.remove(mergedFile)
  
})
