context("classify_index")

test_that("Classify custom raster", {

  r1 <- raster::raster(nrows = 3, ncols = 3,
                       vals = c(0, 5, 7, 10, 20, 30, 40, 50, 60))
  r1_class <- classify_index(r1, thresholds = NULL, labels = NULL)
  expected_classes <- c(1, 1, 2, 2, 3, 4, 5, 5, 6)

  expect_equal(as.numeric(raster::values(r1_class)), expected_classes)

})
