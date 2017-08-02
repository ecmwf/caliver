context("test-import_GEFF_data_from_tar")

test_that("import_GEFF_data_from_tar works", {

  myTempDir <- tempdir()

  download.file(url = paste0("https://dl.dropboxusercontent.com/u/23404805/",
                             "caliver_test_data/",
                             "get-files-atls13-95e2cf679cd58ee9b4db4dd119a05a8d-eKeAG8.tar"),
                destfile = file.path(myTempDir, "test.tar"))

  s <- import_GEFF_data_from_tar(archive = file.path(myTempDir, "test.tar"))

  expect_equal("RasterBrick" %in% class(s), TRUE)
  expect_equal(dim(s), c(256, 512, 120))

  file.remove(file.path(myTempDir, "test.tar"))
  file.remove(list.files(path = myTempDir,
                         pattern = "*.nc", full.names = TRUE))

})
