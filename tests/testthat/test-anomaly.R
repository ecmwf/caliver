context("anomaly")

test_that("Testing the file anomaly.R", {

  # Generate dummy raster and stack
  set.seed(150)
  r1 <- r2 <- r3 <- r4 <- r5 <- r6 <- raster(ncol = 100, nrow = 50)
  raster::values(r1) <- round(runif(raster::ncell(r1), 1, 25))
  raster::values(r2) <- round(runif(raster::ncell(r1), 1, 25))
  raster::values(r3) <- round(runif(raster::ncell(r1), 1, 25))
  raster::values(r4) <- round(runif(raster::ncell(r1), 1, 25))
  raster::values(r5) <- round(runif(raster::ncell(r1), 1, 25))
  raster::values(r6) <- round(runif(raster::ncell(r1), 5, 25))
  # Name the layers
  names(r1) <- "X2018.01.01"
  names(r2) <- "X2017.01.01"
  names(r3) <- "X2016.01.01"
  names(r4) <- "X2015.01.01"
  names(r5) <- "X2014.01.01"
  names(r6) <- "X2013.02.01"
  # Stack layers
  rstack1 <- raster::stack(r2, r3, r4, r5, r6)

  x1 <- anomaly(r = r1, clima = rstack1)
  expect_equal(raster::cellStats(x1, max), 12)

  names(r6) <- "X2013.01.01"
  # Stack layers
  rstack2 <- raster::stack(r2, r3, r4, r5, r6)
  x2 <- anomaly(r = r6, clima = rstack2)
  expect_equal(raster::cellStats(x2 - x1, sum) > 0, TRUE)

})
