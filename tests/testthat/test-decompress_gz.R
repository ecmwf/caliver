context("test-decompress_gz")

test_that("decompress_gz works", {

  skip("Skip - deprecated function")

  # Dropbox link
  dlink <- "https://www.dropbox.com/s/4afx6o33hkl0lfk/test.nc.gz?dl=0"

  # Create a temporary directory
  my_temp_dir <- tempdir()

  # Download file
  download.file(url = dlink, destfile = file.path(my_temp_dir, "test.nc.gz"))

  # Decompress downloaded file
  decompress_gz(input_dir = my_temp_dir)

  # Test whether the file is in the temporary dir
  expect_equal("test.nc" %in% list.files(my_temp_dir), TRUE)

  file.remove(file.path(my_temp_dir, "test.nc"))

})
