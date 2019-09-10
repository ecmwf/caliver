context("ranking")

test_that("Testing the file ranking.R", {

  x <- ranking(r = r1_shifted, clima = rstack1)
  expect_equal(raster::cellStats(x, max), 6)

})
