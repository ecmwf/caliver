context("utils")

test_that("Testing the file utils.R - quant_function", {

  # Check whether the result is correct
  x <- structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16.2, 17, 16.4, 17.3,
                   16.6, 32.9, 32.5, 32.9, 33.9, 35.1, 53.2, 52.8, 55.7,
                   55.2, 55.6, 72.1, 69.9, 73, 73.1, 74.5),
                 .Dim = 5:6, .Dimnames = list(c("fwi.1", "fwi.2", "fwi.3",
                                                "fwi.4", "fwi.5"),
                                              c("50%", "75%", "85%", "90%",
                                                "95%", "98%")))
  expect_equal(x, round(.quant_function(s), 1))

})

test_that("Testing the file utils.R - background_map_fun", {

  # Check whether the result is correct
  example_box <- as(raster::extent(-180, +180, -90, +90), "SpatialPolygons")
  p <- {
    plot(example_box);
    .background_map_fun()
    }
  expect_equal(length(p$layers), 0)

})
