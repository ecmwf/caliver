context("regionalMask")

test_that("regionalMask works", {
  
  e <- regionalMask(region = "EURO")
  
  expect_equal("RasterLayer" %in% class(e), TRUE)
  
  expect_equal(e@ncols, 1440)
  expect_equal(e@nrows, 720)
  
})
