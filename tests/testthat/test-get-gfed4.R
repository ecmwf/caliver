context("get_gfed4")

test_that("Check get_gfed4 behaves as expected", {

  # Basis regions: only BONA region
  BONA <- get_gfed4(varname = "BasisRegions", region = "BONA")
  expect_true("SpatialPolygonsDataFrame" %in% class(BONA))
  expect_true(BONA@bbox[[1]] == -178.25)
  expect_true(BONA@bbox[[2]] == 42)
  expect_true(BONA@bbox[[3]] == -52.5)
  expect_true(BONA@bbox[[4]] == 80)

  skip("Skip two tests")
  # Monthly burned areas
  BurnedAreas <- get_gfed4(start_date = "2003-01-01",
                           end_date = "2003-01-31",
                           temporal_resolution = "monthly",
                           varname = "BurnedArea")
  # Daily burned areas
  DailyBurnedAreas <- get_gfed4(start_date = "2003-01-01",
                                end_date = "2003-01-02",
                                temporal_resolution = "daily",
                                varname = "BurnedArea")

})
