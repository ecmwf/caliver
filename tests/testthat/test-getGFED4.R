context("getGFED4")

test_that("getGFED4 works", {
  
  GFED4BasisRegions <- getGFED4(varname = 'BasisRegions')
  
  expect_equal(dim(GFED4BasisRegions), c(720, 1440, 1))
  
})
