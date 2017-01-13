context("getPercRiskIndex")

## TODO: Rename context
## TODO: Add more tests

test_that("getPercRiskIndex works", {
  
  raster = readRDS(system.file("extdata", "RISICO", 'RISICO_raster.rds', package="caliver"))
  shape = maptools::readShapePoly(system.file("extdata", "RISICO", 'italy_provinces.shp', package="caliver"))
  
  output_gt_75 <- getPercRiskIndex(raster, shape, perc.val = 75, mod = "gt")
  
  expect_equal(length(shape), length(output_gt_75)) # Check if the output table has the same dimension as the shapefile
})
