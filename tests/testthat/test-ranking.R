context("ranking")

test_that("Testing the file ranking.R", {

  x <- ranking(r, b)
  expect_equal(raster::cellStats(x, max), 1)

})
