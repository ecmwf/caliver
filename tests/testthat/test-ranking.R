context("ranking")

test_that("Testing the file ranking.R", {

  x <- ranking(r = r1, clima = rstack1)
  expect_equal(raster::cellStats(x, max), 7)

  # The plotting function cannot be tested
  # because it returns different values in different environments.

})
