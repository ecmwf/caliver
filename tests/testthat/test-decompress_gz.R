context("test-decompress_gz")

test_that("decompress_gz works", {

  myTempDir <- tempdir() # works on all platforms with a platform-dependent result

  download.file(url = paste0("https://dl.dropboxusercontent.com/u/23404805/",
                             "caliver_test_data/test.nc.gz"),
                destfile = file.path(myTempDir, "test.nc.gz"))

  decompress_gz(input_dir = myTempDir)
  expect_equal("test.nc" %in% list.files(myTempDir), TRUE)
  expect_equal("test.nc.gz" %in% list.files(myTempDir), FALSE)

  file.remove(file.path(myTempDir, "test.nc"))

})
