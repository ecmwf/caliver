context("anomaly")

test_that("Testing the file anomaly.R", {

  x <- anomaly(r, b)
  expect_true(raster::cellStats(x, max) == 6)

})
