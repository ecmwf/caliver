context("getGFED4")

test_that("getGFED4 works with basis regions", {
  
  GFED4BasisRegions <- getGFED4(varname = 'BasisRegions')
  
  expect_equal("SpatialPolygonsDataFrame" %in% class(GFED4BasisRegions), TRUE)
  
  GFED4BasisRegions <- getGFED4(varname = 'BasisRegions', region = 'EURO')
  
  expect_equal(dim(GFED4BasisRegions), c(21860, 1))
  
})

test_that("getGFED4 works with BurnedAreas", {
  
  skip_on_os("linux")
  
  DailyBurnedAreas <- getGFED4(years = 2003, 
                               tempRes = "daily", 
                               varname = "BurnedArea")
  
  expect_equal("RasterBrick" %in% class(DailyBurnedAreas), TRUE)
  
  expect_equal(dim(DailyBurnedAreas), c(720, 1440, 365))
  
})
