context("anomaly")

test_that("Testing the file anomaly.R", {

  x1 <- anomaly(r = r1_shifted, clima = rstack1)
  expect_true(raster::cellStats(x1, max) == 11)

})
