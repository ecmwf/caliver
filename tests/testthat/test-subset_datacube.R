context("subset_datacube")

test_that("Testing the file subset_datacube.R", {

  # Generate dummy raster and stack
  set.seed(150)
  r1 <- r2 <- r3 <- r4 <- r5 <- r6 <- raster(ncol = 100, nrow = 50)
  raster::values(r1) <- round(runif(raster::ncell(r1), 1, 25))
  raster::values(r2) <- round(runif(raster::ncell(r1), 1, 25))
  raster::values(r3) <- round(runif(raster::ncell(r1), 1, 25))
  raster::values(r4) <- round(runif(raster::ncell(r1), 1, 25))
  raster::values(r5) <- round(runif(raster::ncell(r1), 1, 25))
  raster::values(r6) <- round(runif(raster::ncell(r1), 5, 25))
  # Stack layers
  rstack1 <- raster::stack(r1, r2, r3, r4, r5, r6)
  # Name the layers
  names(rstack1) <- seq.Date(from = as.Date("2017-12-30"),
                             to = as.Date("2018-01-04"),
                             by = "day")
  rstack2 <- subset_datacube(rstack1, "2017-12-31", "2018-01-03")

  expect_equal(raster::nlayers(rstack2), 4)

})
