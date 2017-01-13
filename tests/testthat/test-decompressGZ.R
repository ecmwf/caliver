context("test-decompressGZ")

test_that("decompression works", {
  
  skip_on_travis()
  
  skip_on_appveyor()
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  download.file(url = paste0("https://dl.dropboxusercontent.com/u/23404805/", 
                             "caliver_test_data/test.nc.gz"), 
                destfile = file.path(myTempDir, "test.nc.gz"))
  
  decompressGZ(dirs = myTempDir, keep = FALSE)
  
  expect_equal("test.nc" %in% list.files(myTempDir), TRUE)
  
})
