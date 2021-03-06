context("daily_clima")

test_that("daily_clima works", {

  maps <- daily_clima(b = b, dates = as.Date("2020-08-15"))

  # Check output data type
  expect_true("RasterBrick" %in% class(maps))

  # Check values
  mean_maps <- round(mean(raster::cellStats(maps, mean)), 0)
  expect_equal(mean_maps, 46)

})
