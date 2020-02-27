context("utils")

test_that("Testing the file utils.R - quant_function", {

  # Check whether the result is correct
  x <- structure(c(19, 19, 19, 19, 20, 22, 21, 21, 22, 22, 23, 22, 22,
                   23, 23, 24, 24, 24, 24, 24, 25, 24, 24, 25, 25),
                 .Dim = c(5L, 5L),
                 .Dimnames = list(c("X2017.01.01", "X2016.01.01", "X2015.01.01",
                                    "X2014.01.01", "X2013.02.01"),
                                  c("75%", "85%", "90%", "95%", "98%")))

  expect_equal(x, round(.quant_function(rstack1), 1))

})

test_that("Testing the file utils.R - background_map_fun", {

  # Check whether the result is correct
  example_box <- as(raster::extent(-180, +180, -90, +90), "SpatialPolygons")
  p <- {
    raster::plot(example_box)
    .background_map_fun()
  }

  # Check the function does not generate a dummy layer
  expect_equal(length(p$layers), 0)

})

test_that("Testing the file utils.R - .classify_hazard", {


  # Check whether the result is correct
  x <- .classify_hazard(r1[], r2[], r3[], r4[], r5[], r6[])

  # Check the function does not generate a dummy layer
  expect_equal(sum(x), 11761)

})

test_that("Testing the file utils.R - .utci_classification", {

  # Check whether the result is correct
  x <- .utci_classification(rtp = r1)

  # Check the function does not generate a dummy layer
  expect_equal(raster::cellStats(x, sum)[[1]], 65792)

})
