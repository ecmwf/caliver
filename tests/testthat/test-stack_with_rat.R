context("stack_with_rat")

test_that("Test stack_with_rat with RasterLayer", {
  
  set.seed(0)
  r_test <- raster::raster(ncol = 100, nrow = 50)
  raster::values(r_test) <- round(runif(raster::ncell(r_test), 1, 3))
  x <- stack_with_rat(r = r_test,
                      ids = 1:3, classes = c("Low", "Moderate", "High"))
  
  expect_equal(raster::compareRaster(r_test, x), TRUE)
  
})
