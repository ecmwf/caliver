context("anomaly")

test_that("Testing the file anomaly.R", {

  x1 <- anomaly(r = r1_shifted, clima = rstack1)
  expect_equal(raster::cellStats(x1, max), 12)

  names(r6) <- "X2013.01.01"
  x2 <- anomaly(r = r6, clima = rstack1)
  expect_true(raster::cellStats(x2 - x1, sum) == 2277)

  # The plotting function cannot be tested properly
  # because it returns different values in different environments.
  xplot <- plot_anomaly(x1)
  expect_equal(xplot$rect$top, 0)

})
