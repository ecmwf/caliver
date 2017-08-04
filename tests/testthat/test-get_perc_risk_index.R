context("get_perc_risk_index")

test_that("get_perc_risk_index works", {

  raster = readRDS(system.file("extdata", "RISICO", "RISICO_raster.rds",
                               package="caliver"))

  shape = maptools::readShapePoly(system.file("extdata", "RISICO", 
                                              "italy_provinces.shp", 
                                              package = "caliver"))

  output_gt_75 <- get_perc_risk_index(raster, shape, perc_val = 75, mod = "gt")

  # Check if the output table has the same dimension as the shapefile
  expect_equal(length(shape), length(output_gt_75))

})
