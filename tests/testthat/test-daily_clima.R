context("daily_clima")

test_that("get_percentile_map works with all non-null inputs", {

  maps <- daily_clima(b = b, dates = as.Date("2020-08-15"))

  # Check output data type
  expect_true("RasterBrick" %in% class(maps))
  
  # Check percentile value
  mean_maps <- round(mean(raster::cellStats(maps, mean)), 0)
  expect_equal(mean_maps, 47)

})
