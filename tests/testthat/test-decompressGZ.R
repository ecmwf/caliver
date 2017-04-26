context("test-decompressGZ")

test_that("decompression works with keep = FALSE", {
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  download.file(url = paste0("https://dl.dropboxusercontent.com/u/23404805/",
                             "caliver_test_data/test.nc.gz"),
                destfile = file.path(myTempDir, "test.nc.gz"))
  
  decompressGZ(inDir = myTempDir, keep = FALSE)
  expect_equal("test.nc" %in% list.files(myTempDir), TRUE)
  expect_equal(!("test.nc.gz" %in% list.files(myTempDir)), TRUE)
  
  file.remove(file.path(myTempDir, "test.nc"))
  
})

test_that("decompression works with keep = TRUE", {
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  download.file(url = paste0("https://dl.dropboxusercontent.com/u/23404805/",
                             "caliver_test_data/test.nc.gz"),
                destfile = file.path(myTempDir, "test.nc.gz"))
  
  decompressGZ(inDir = myTempDir, keep = TRUE)
  expect_equal("test.nc" %in% list.files(myTempDir), TRUE)
  expect_equal("test.nc.gz" %in% list.files(myTempDir), TRUE)
  
  file.remove(file.path(myTempDir, "test.nc"))
  file.remove(file.path(myTempDir, "test.nc.gz"))

})
