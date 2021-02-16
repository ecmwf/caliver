context("utils")

test_that("Testing the file utils.R - .round.choose", {
  
  # Check whether the results are correct
  expect_equal(.round.choose(41, 5, 1), 45)
  expect_equal(.round.choose(41, 5, 0), 40)
  
})

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
  x <- structure(c(65.4, 91.7, 63.7, 70.8, 78.6, 81.7, 29.7, 57.9, 65.7, 74.1,
                   75.1, 92.8, 64.7, 73.3, 86.8, 86.4, 33.3, 69.5, 72.5, 76.3, 
                   80, 93.4, 65.1, 74.5, 91, 88.8, 35, 75.3, 75.9, 77.3, 84.8,
                   93.9, 65.6, 75.7, 95.1, 91.1, 36.8, 81.2, 79.3, 78.4, 87.7,
                   94.2, 65.9, 76.5, 97.5, 92.5, 37.9, 84.6, 81.4, 79),
                 .Dim = c(10L, 5L),
                 .Dimnames = list(c("X1993.01.01", "X1993.01.02", "X1993.01.03",
                                    "X1993.01.04", "X1993.01.05", "X1993.01.06",
                                    "X1993.01.07", "X1993.01.08", "X1993.01.09",
                                    "X1993.01.10"),
                                  c("75%", "85%", "90%", "95%", "98%")))
  expect_equal(x, round(.quant_function(b[[1:10]]), 1))

})

test_that("Testing the file utils.R - .convert_long_from_180_to_360", {
  
  x <- .convert_long_from_180_to_360(-10)
  expect_equal(x, 350)
  
})

test_that("Testing the file utils.R - .transform_raster", {
  
  x <- .transform_raster(raster_in = r, variable_name = "FWI")
  expect_equal(raster::cellStats(x, "sum"), raster::cellStats(r, "sum"))
  
  x <- .transform_raster(raster_in = r, variable_name = "BasisRegions")
  expect_equal(raster::cellStats(x, "sum"), raster::cellStats(r, "sum"))
  
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
  
  x <- .get_layers_for_clima(b = b, raster_date = "2016.01.01",
                             expand = FALSE)
  expect_equal(nlayers(x), 3)
  
})

test_that("Testing the file utils.R - .classify_hazard", {
  
  
  # Check whether the result is correct
  x <- .classify_hazard(b[[1]][], b[[2]][], b[[3]][],
                        b[[4]][], b[[5]][], b[[6]][])
  
  # Check the function does not generate a dummy layer
  expect_equal(sum(x), 5)
  
})

test_that("Testing the file utils.R - .utci_classification", {
  
  # Check whether the result is correct
  x <- .utci_classification(rtp = r)
  
  # Check the function does not generate a dummy layer
  expect_equal(raster::cellStats(x, sum)[[1]], 120)
  
})
