context("test-decompressGZ")

test_that("decompressGZ works", {

  myTempDir <- tempdir() # works on all platforms with a platform-dependent result

  download.file(url = paste0("https://dl.dropboxusercontent.com/u/23404805/",
                             "caliver_test_data/test.nc.gz"),
                destfile = file.path(myTempDir, "test.nc.gz"))

  decompressGZ(inDir = myTempDir)
  expect_equal("test.nc" %in% list.files(myTempDir), TRUE)
  expect_equal("test.nc.gz" %in% list.files(myTempDir), FALSE)

  file.remove(file.path(myTempDir, "test.nc"))

})
