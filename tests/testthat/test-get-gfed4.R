context("get_gfed4")

test_that("Check get_gfed4 behaves returns BasisRegions", {

  # Basis regions: only BONA region
  bona <- get_gfed4(varname = "BasisRegions", region = "BONA")
  expect_true("SpatialPolygonsDataFrame" %in% class(bona))
  expect_true(bona@bbox[[1]] == -178.25)
  expect_true(bona@bbox[[2]] == 42)
  expect_true(bona@bbox[[3]] == -52.5)
  expect_true(bona@bbox[[4]] == 80)

})
