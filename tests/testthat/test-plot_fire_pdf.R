context("plot_fire_pdf")

myTempDir <- tempdir()
download.file(url = "https://dl.dropboxusercontent.com/u/23404805/caliver_test_data/outTest.nc", destfile = file.path(myTempDir, "outTest.nc"), method="curl")
inFile <- file.path(myTempDir, "outTest.nc")

x <- raster::raster(inFile)

test_that("plot_fire_pdf match expectations for no y-limit",{
  
  p <- plot_fire_pdf(fire_index = x,
                     thresholds = c(1,2,3,4,5),
                     upper_limit = NULL,
                     v_lines = NULL)

  expect_equal(p$scales$scales[[2]]$limits, c(0, 40))
  
})

test_that("Plot layers match expectations without vLines",{
  
  p <- plot_fire_pdf(fire_index = x,
                     thresholds = c(1,2,3,4,5),
                     upper_limit = 100)
  
  expect_equal(length(p$layers) == 3, TRUE)
  expect_identical(p$labels$y, "Density")
  expect_identical(p$labels$x, "FWI")
  expect_equal(p$scales$scales[[2]]$limits, c(0, 100))
  
})

test_that("Plot layers match expectations with vLines",{
  
  p <- plot_fire_pdf(fire_index = x,
                     thresholds = c(1,2,3,4,5),
                     upper_limit = 100,
                     v_lines = c("90%" = 0.90))
  
  expect_equal(length(p$layers) == 5, TRUE)
  expect_identical(p$labels$y, "Density")
  expect_identical(p$labels$x, "FWI")
  expect_equal(p$scales$scales[[2]]$limits, c(0, 100))
  
})
