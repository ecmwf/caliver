context("regionalMask")

## TODO: Rename context
## TODO: Add more tests

test_that("multiplication works", {
  
  e <- regionalMask(region = "Europe")
  
  expect_equal("RasterLayer" %in% class(e), TRUE)
  expect_equal(e@ncols, 512)
  expect_equal(e@nrows, 256)
  
})
