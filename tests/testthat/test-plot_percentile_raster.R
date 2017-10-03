context("plot_percentile_raster")

probs_maps <- get_percentile_raster(input_file = geff5nc,
                                    probs = 50,
                                    output_dir = tempdir())

test_that("plot_percentile_raster works with rotate_map = TRUE", {

  test1 <- file.path(tempdir(), "test1.png")

  png(filename = test1)
  plot_percentile_raster(maps = probs_maps, rotate_map = TRUE)
  dev.off()

  # This is to get the fingerprint: visualTest::getFingerprint(test1)
  visualTest::isSimilar(file = test1,
                        fingerprint = "B34F7C4B70E4C4B0",
                        threshold = 0.1)

})

test_that("plot_percentile_raster works with rotate_map = FALSE", {

  test2 <- file.path(tempdir(), "test2.png")

  png(filename = test2)
  plot_percentile_raster(maps = probs_maps, rotate_map = FALSE)
  dev.off()

  visualTest::isSimilar(file = test2,
                        fingerprint = "B32B784FECC4C4D0",
                        threshold = 0.1)

})

test_that("plot_percentile_raster works with region = EURO", {

  test3 <- file.path(tempdir(), "test3.png")

  png(filename = test3)
  plot_percentile_raster(maps = probs_maps, rotate_map = TRUE, region = "EURO")
  dev.off()

  visualTest::isSimilar(file = test3,
                        fingerprint = "AF0F786FF090C4D0",
                        threshold = 0.1)

})

test_that("plot_percentile_raster works with multi probs", {

  probs_maps <- get_percentile_raster(input_file = geff5nc,
                                      probs = c(50, 90),
                                      output_dir = tempdir())

  test4 <- file.path(tempdir(), "test4.png")

  png(filename = test4)
  plot_percentile_raster(maps = probs_maps, rotate_map = TRUE, region = "EURO")
  dev.off()

  visualTest::isSimilar(file = test4,
                        fingerprint = "800B2D093F6F2D6F",
                        threshold = 0.1)

})
