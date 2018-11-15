context("ranking")

test_that("Testing the file ranking.R", {

  x <- ranking(r = r1_shifted, clima = rstack1)
  expect_equal(raster::cellStats(x, max), 7)

  # The plotting function cannot be tested properly
  # because it returns different values in different environments.
  xplot <- plot_ranking(x)
  expect_equal(xplot$rect$top, 0)

})
