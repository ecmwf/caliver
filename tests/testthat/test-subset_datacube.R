context("subset_datacube")

test_that("Testing the file subset_datacube.R", {

  rstack2 <- subset_datacube(rstack1, "2016-01-01", "2016-01-01")
  expect_equal(raster::nlayers(rstack2), 1)

})
