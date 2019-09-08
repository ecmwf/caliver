context("vdi")

test_that("Calculate vdi for custom raster", {

  dc <- raster::raster(nrows = 2, ncols = 3,
                       vals = c(350, 650, 800, 700, 500, 1000))
  dmc <- raster::raster(nrows = 2, ncols = 3,
                        vals = c(15, 15, 15, 50, 300, 300))
  expected_classes <- c(1, 2, 3, 3, 4, 5)

  expect_equal(as.numeric(raster::values(vdi(dc, dmc))), expected_classes)

})
