context("test-decompressGZ")

test_that("decompression works", {
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  file.copy(from = system.file("extdata", "test.nc.gz", package="caliver"),  
            to = paste0(myTempDir, "/test.nc.gz"))
  
  decompressGZ(dirs = myTempDir, keep = TRUE)
  
  expect_equal("test.nc" %in% list.files(myTempDir), TRUE)
  
})
