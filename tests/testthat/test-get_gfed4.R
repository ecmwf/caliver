context("get_gfed4")

test_that("get_gfed4 works with basis regions", {
  
  GFED4BasisRegions <- get_gfed4(varname = "BasisRegions")
  
  expect_equal("SpatialPolygonsDataFrame" %in% class(GFED4BasisRegions), TRUE)
  
  EURO <- get_gfed4(varname = "BasisRegions", region = "EURO")
  
  expect_equal(dim(EURO), c(21860, 1))
  
})

test_that("get_gfed4 stops with null Dates", {
  
  x <- try(DailyBurnedAreas <- get_gfed4(start_date = NULL, end_date = NULL,
                                         temporal_resolution = "daily", 
                                        varname = "BurnedArea"), silent = TRUE)
  
  expect_equal(class(x), "try-error")
  
})

test_that("get_gfed4 stops with null temporal_resolution", {
  
  x <- try(DailyBurnedAreas <- get_gfed4(start_date = "2003-01-01", 
                                        end_date = "2003-01-02",
                                        temporal_resolution = NULL, 
                                        varname = "BurnedArea"), silent = TRUE)
  
  expect_equal(class(x), "try-error")
  
})

test_that("get_gfed4 stops with unavailable dates", {
  
  x <- try(DailyBurnedAreas <- get_gfed4(start_date = "1900-01-01", 
                                        end_date = "1900-01-02",
                                        temporal_resolution = "daily", 
                                        varname = "BurnedArea"), silent = TRUE)
  
  expect_equal(class(x), "try-error")
  
})

test_that("get_gfed4 stops with null varname", {
  
  x <- try(DailyBurnedAreas <- get_gfed4(start_date = "2003-01-01", 
                                        end_date = "2003-01-02",
                                        temporal_resolution = "daily", 
                                        varname = NULL), silent = TRUE)
  
  expect_equal(class(x), "try-error")
  
})

test_that("get_gfed4 works with monthly data (1 month)", {
  
  MonthlyBurnedAreas <- get_gfed4(start_date = "2003-01-01", 
                                 end_date = "2003-01-31",
                                 temporal_resolution = "monthly", 
                                 varname = "BurnedArea")
  
  expect_equal("RasterLayer" %in% class(MonthlyBurnedAreas), TRUE)
  expect_equal(dim(MonthlyBurnedAreas), c(720, 1440, 1))
  
})

test_that("get_gfed4 works with daily data (5 days)", {
  
  DailyBurnedAreas <- get_gfed4(start_date = "2003-01-01",
                                end_date = "2003-01-05",
                                temporal_resolution = "daily",
                                varname = "BurnedArea")
  
  expect_equal("RasterBrick" %in% class(DailyBurnedAreas), TRUE)
  expect_equal(dim(DailyBurnedAreas), c(720, 1440, 5))
  
})
