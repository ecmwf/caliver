context("test-decompressGZ")

test_that("decompression works", {
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  file.copy(from = system.file(package = "caliver", "/extdata/test.nc.gz"),  
            to = paste0(myTempDir, "/test.nc.gz"))
  
  decompressGZ(dirs = myTempDir, keep = TRUE)
  
  expect_equal("test.nc" %in% list.files(myTempDir), TRUE)
  
})
