context("subset_datacube")

test_that("Testing the file subset_datacube.R", {

  rstack2 <- subset_datacube(r = b, from = "1993-01-01", to = "1993-01-01")
  expect_equal(raster::nlayers(rstack2), 1)

})
