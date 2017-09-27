context("get_gfed4")

test_that("get_gfed4 works with basis regions", {

  gfed4_basis_regions <- get_gfed4(varname = "BasisRegions")

  expect_equal("SpatialPolygonsDataFrame" %in% class(gfed4_basis_regions), TRUE)

  euro <- get_gfed4(varname = "BasisRegions", region = "EURO")

  expect_equal(dim(euro), c(21860, 1))

})

test_that("get_gfed4 stops with null Dates", {

  x <- try(get_gfed4(start_date = NULL,
                     end_date = NULL,
                     temporal_resolution = "daily",
                     varname = "BurnedArea"), silent = TRUE)

  expect_equal(class(x), "try-error")
  rm(x)

})

test_that("get_gfed4 stops with null temporal_resolution", {

  x <- try(get_gfed4(start_date = "2003-01-01",
                     end_date = "2003-01-02",
                     temporal_resolution = NULL,
                     varname = "BurnedArea"), silent = TRUE)

  expect_equal(class(x), "try-error")
  rm(x)

})

test_that("get_gfed4 stops with null varname", {

  x <- try(get_gfed4(start_date = "2003-01-01",
                     end_date = "2003-01-02",
                     temporal_resolution = "daily",
                     varname = NULL), silent = TRUE)

  expect_equal(class(x), "try-error")
  rm(x)

})

test_that("get_gfed4 works with monthly data (1 month)", {

  monthly_burned_areas <- get_gfed4(start_date = "2003-01-01",
                                    end_date = "2003-01-31",
                                    temporal_resolution = "monthly",
                                    varname = "BurnedArea")

  expect_equal("RasterLayer" %in% class(monthly_burned_areas), TRUE)
  expect_equal(dim(monthly_burned_areas), c(720, 1440, 1))

})

test_that("get_gfed4 works with daily data (5 days)", {

  daily_burned_areas <- get_gfed4(start_date = "2003-01-01",
                                  end_date = "2003-01-05",
                                  temporal_resolution = "daily",
                                  varname = "BurnedArea")

  expect_equal("RasterBrick" %in% class(daily_burned_areas), TRUE)
  expect_equal(dim(daily_burned_areas), c(720, 1440, 5))

})
