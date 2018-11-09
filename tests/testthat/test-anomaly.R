context("anomaly")

test_that("Testing the file anomaly.R", {

  x1 <- anomaly(r = r1, clima = rstack1)
  expect_equal(raster::cellStats(x1, max), 12)

  names(r6) <- "X2013.01.01"
  x2 <- anomaly(r = r6, clima = rstack2)
  expect_true(raster::cellStats(x2 - x1, sum) > 0)

  # The plotting function cannot be tested
  # because it returns different values in different environments.

})
