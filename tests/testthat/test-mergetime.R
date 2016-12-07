context("mergetime")

test_that("mergetime works", {
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  # list.files(myTempDir)
  
  if (length(grep("test", list.files(myTempDir))) > 0) {
    file.remove(paste0(myTempDir, "/", 
                       list.files(myTempDir)[grep("test", 
                                                  list.files(myTempDir))]))
  }
  
  inFile <- system.file("extdata", "testA.nc", package="caliver") 
  
  if (file.exists(inFile) == TRUE) {
    file.copy(from = inFile, to = paste0(myTempDir, "/testA.nc"))
    file.copy(from = inFile, to = paste0(myTempDir, "/testB.nc"))
  }
  
  mergedFile <- mergetime(dirs = myTempDir, 
                          startingString = "test", 
                          outFile = "outTest.nc",
                          outDir = myTempDir)
  
  # print(mergedFile)
  
  x <- ncdf4::nc_open(mergedFile)
  
  expect_equal(x$dim$time$len, 2)
  
  file.remove(mergedFile)
  
})
