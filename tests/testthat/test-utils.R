context("utils")

test_that("Testing the file utils.R - .background_map_fun", {
  
  # Check whether the result is correct
  example_box <- as(raster::extent(-180, +180, -90, +90), "SpatialPolygons")
  p <- {
    raster::plot(example_box)
    .background_map_fun()
  }
  expect_equal(length(p$layers), 0)
  
})

test_that("Testing the file utils.R - .quant_function", {

  # Check whether the result is correct
  x <- structure(c(19, 19, 19, 19, 20, 22, 21, 21, 22, 22, 23, 22, 22,
                   23, 23, 24, 24, 24, 24, 24, 25, 24, 24, 25, 25),
                 .Dim = c(5L, 5L),
                 .Dimnames = list(c("X2017.01.01", "X2016.01.01", "X2015.01.01",
                                    "X2014.01.01", "X2013.02.01"),
                                  c("75%", "85%", "90%", "95%", "98%")))
  expect_equal(x, round(.quant_function(rstack1), 1))

})

test_that("Testing the file utils.R - .convert_long_from_180_to_360", {
  
  x <- .convert_long_from_180_to_360(-10)
  expect_equal(x, 350)
  
})

test_that("Testing the file utils.R - .transform_raster", {
  
  x <- .transform_raster(raster_in = r1, variable_name = "FWI")
  expect_equal(raster::cellStats(x, "sum"), raster::cellStats(r1, "sum"))
  
  x <- .transform_raster(raster_in = r1, variable_name = "BasisRegions")
  expect_equal(raster::cellStats(x, "sum"), raster::cellStats(r1, "sum"))
  
})

test_that("Testing the file utils.R - .create_rat", {
  
  x2compare <- structure(list(ID = structure(1:3, .Label = c("1", "2", "3"),
                                             class = "factor"),
                              Class = structure(1:3, .Label = c("A", "B", "C"),
                                                class = "factor")),
                         row.names = c(NA, 3L), class = "data.frame")
  
  x <- .create_rat(ids = c(1, 2, 3), classes = c("A", "B", "C"))
  expect_equal(x, x2compare)
  
})

test_that("Testing the file utils.R - .get_layers_for_clima", {
  
  x <- .get_layers_for_clima(clima = rstack1, raster_date = "2016.01.01",
                             expand = FALSE)
  expect_equal(nlayers(x), 4)
  
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
