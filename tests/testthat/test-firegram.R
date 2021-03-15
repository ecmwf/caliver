context("firegram")

test_that("firegram works", {
  
  thresholds <- c(3, 9, 23, 49, 98)

  # Generate dummy HRES forecast (10 days lead time)
  hres <- raster::brick(lapply(1:10,
                               function(i) raster::setValues(r,
                                                             runif(n = raster::ncell(r),
                                                                   min = 0, max = 100))))
  names(hres) <- seq.Date(from = as.Date("2011-01-01"), by = "day", length.out = 10)
  
  # Generate dummy ENS forecast (51 members and 15 days lead time)
  ens <- raster::brick(lapply(1:(51*15),
                              function(i) raster::setValues(r,
                                                            runif(n = raster::ncell(r),
                                                                  min = 0, max = 100))))
  names(ens) <- rep(seq.Date(from = as.Date("2011-01-01"), by = "day", length.out = 15), 51)
  
  # Firegram
  p <- firegram(thresholds, hres, ens)
  expect_true(length(p$layers) == 3)

})
