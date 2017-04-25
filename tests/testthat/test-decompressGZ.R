context("test-decompressGZ")

myTempDir <- tempdir() # works on all platforms with a platform-dependent result

download.file(url = paste0("https://dl.dropboxusercontent.com/u/23404805/",
                           "caliver_test_data/test.nc.gz"),
              destfile = file.path(myTempDir, "test.nc.gz"),
              method="curl")

test_that("decompression works with keep = TRUE", {

  decompressGZ(inDir = myTempDir, keep = TRUE)
  expect_equal(list.files(myTempDir), c("test.nc", "test.nc.gz"))
  file.remove(file.path(myTempDir, "test.nc"))

})

test_that("decompression works with keep = FALSE", {

  decompressGZ(inDir = myTempDir, keep = FALSE)
  expect_equal(list.files(myTempDir), "test.nc")
  
  file.remove(file.path(myTempDir, "test.nc"))
  
})
