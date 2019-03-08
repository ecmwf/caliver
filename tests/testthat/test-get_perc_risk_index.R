context("get_perc_risk_index")

# Read test data
r <- readRDS(system.file("extdata", "RISICO_raster.rds", package = "caliver"))
# Set missing crs
raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs"

test_that("get_perc_risk_index works", {

  output_gt_75 <- get_perc_risk_index(r,
                                      shape,
                                      perc_val = 75,
                                      mod = "gt")
  # Check if the output table has the same dimension as the shapefile
  expect_equal(round(output_gt_75, 0), 98)

  output_lt_50 <- get_perc_risk_index(r_stack = rstack1,
                                      p_shape = shape,
                                      perc_val = 50,
                                      mod = "lt")
  # Check if the output table has the same dimension as the shapefile
  expect_equal(round(output_lt_50, 0), c(5, 10, 6, 8, 7))
  expect_equal(length(output_lt_50), raster::nlayers(rstack1))

})
