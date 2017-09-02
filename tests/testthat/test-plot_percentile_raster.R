context("plot_percentile_raster")

myTempDir <- tempdir() # works on all platforms with a platform-dependent result

download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/outTest.nc", destfile = file.path(myTempDir, "outTest.nc"), method="curl")

input_file <- file.path(myTempDir, "outTest.nc")

probsMaps <- get_percentile_raster(input_file_path = input_file,
                                   probs = 50,
                                   output_dir = myTempDir)

test_that("plot_percentile_raster works with rotate_map = TRUE", {
  
  test1 <- file.path(myTempDir, "test1.png")
  
  png(filename = test1)
  plot_percentile_raster(maps = probsMaps, rotate_map = TRUE)
  dev.off()
  
  visualTest::isSimilar(file = test1, 
                        fingerprint = "B30F6C1B71ECC4B0", 
                        threshold = 0.1)
  
})

test_that("plot_percentile_raster works with rotate_map = FALSE", {
  
  test2 <- file.path(myTempDir, "test2.png")
  
  png(filename = test2)
  plot_percentile_raster(maps = probsMaps, rotate_map = FALSE)
  dev.off()
  
  visualTest::isSimilar(file = test2, 
                        fingerprint = "B32B784FCCCCC4D0", 
                        threshold = 0.1)
  
})

test_that("plot_percentile_raster works with region = EURO", {
  
  test3 <- file.path(myTempDir, "test3.png")
  
  png(filename = test3)
  plot_percentile_raster(maps = probsMaps, rotate_map = TRUE, region = "EURO")
  dev.off()
  
  # getFingerprint(test3)
  visualTest::isSimilar(file = test3, 
                        fingerprint = "AF0F306FF8B0C4D0", 
                        threshold = 0.1)
  
})

test_that("plot_percentile_raster works with multi probs", {
  
  myTempDir <- tempdir() # works on all platforms with a platform-dependent result
  
  download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/outTest.nc", destfile = file.path(myTempDir, "outTest.nc"), method="curl")
  
  input_file <- file.path(myTempDir, "outTest.nc")
  
  probsMaps <- get_percentile_raster(input_file_path = input_file, 
                                     probs = c(50, 90),
                                     output_dir = myTempDir)
  
  test4 <- file.path(myTempDir, "test4.png")
  
  png(filename = test4)
  plot_percentile_raster(maps = probsMaps, rotate_map = TRUE, region = "EURO")
  dev.off()
  
  # visualTest::getFingerprint(test4)
  visualTest::isSimilar(file = test4, 
                        fingerprint = "A0092F017F6F196F", 
                        threshold = 0.1)
  
})
