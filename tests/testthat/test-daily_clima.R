context("daily_clima")

test_that("Testing the file daily_clima.R", {

  x1 <- daily_clima(r = rstack1, dates = "2018-01-01", probs = 0.5)
  expect_equal(raster::cellStats(x1, min), 2)
  expect_equal(raster::cellStats(x1, max), 24)

})
