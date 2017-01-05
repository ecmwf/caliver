context("getGFED4BasisRegions")

test_that("getGFED4BasisRegions works", {
  
  GFED4BasisRegions <- getGFED4BasisRegions()
  
  expect_equal(dim(GFED4BasisRegions), c(720, 1440, 1))
  
})
