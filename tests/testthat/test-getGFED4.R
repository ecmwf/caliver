context("getGFED4")

test_that("getGFED4 works with basis regions", {
  
  GFED4BasisRegions <- getGFED4(varname = "BasisRegions")
  
  expect_equal("SpatialPolygonsDataFrame" %in% class(GFED4BasisRegions), TRUE)
  
  GFED4BasisRegions <- getGFED4(varname = "BasisRegions", region = "EURO")
  
  expect_equal(dim(GFED4BasisRegions), c(21860, 1))
  
})

test_that("getGFED4 stops with null Dates", {
  
  x <- try(DailyBurnedAreas <- getGFED4(startDate = NULL, endDate = NULL,
                                        tempRes = "daily", 
                                        varname = "BurnedArea"), silent = TRUE)
  
  expect_equal(class(x), "try-error")
  
})

test_that("getGFED4 stops with null tempRes", {
  
  x <- try(DailyBurnedAreas <- getGFED4(startDate = "2003-01-01", 
                                        endDate = "2003-01-02",
                                        tempRes = NULL, 
                                        varname = "BurnedArea"), silent = TRUE)
  
  expect_equal(class(x), "try-error")
  
})

test_that("getGFED4 stops with unavailable dates", {
  
  x <- try(DailyBurnedAreas <- getGFED4(startDate = "1900-01-01", 
                                        endDate = "1900-01-02",
                                        tempRes = "daily", 
                                        varname = "BurnedArea"), silent = TRUE)
  
  expect_equal(class(x), "try-error")
  
})

test_that("getGFED4 stops with null varname", {
  
  x <- try(DailyBurnedAreas <- getGFED4(startDate = "2003-01-01", 
                                        endDate = "2003-01-02",
                                        tempRes = "daily", 
                                        varname = NULL), silent = TRUE)
  
  expect_equal(class(x), "try-error")
  
})

test_that("getGFED4 works with monthly data (1 month)", {
  
  DailyBurnedAreas <- getGFED4(startDate = "2003-01-01", endDate = "2003-01-31",
                               tempRes = "monthly", varname = "BurnedArea")
  
  expect_equal("RasterLayer" %in% class(DailyBurnedAreas), TRUE)
  expect_equal(dim(DailyBurnedAreas), c(720, 1440, 1))
  
})

test_that("getGFED4 works with daily data (5 days)", {
  
  skip_on_travis()
  skip_on_os("linux")
  
  DailyBurnedAreas <- getGFED4(startDate = "2003-01-01", endDate = "2003-01-05",
                               tempRes = "daily", varname = "BurnedArea")
  
  expect_equal("RasterBrick" %in% class(DailyBurnedAreas), TRUE)
  expect_equal(dim(DailyBurnedAreas), c(720, 1440, 5))
  
})
