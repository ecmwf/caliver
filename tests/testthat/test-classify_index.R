context("classify_index")

test_that("Classify custom raster", {

  r <- raster::raster(nrows = 3, ncols = 3,
                      vals = c(0, 5, 7, 10, 20, 30, 40, 50, 800))

  classesFWI <- classify_index(r, index = "fwi", thresholds = NULL, labels = NULL)
  expected_classes <- c(1, 1, 2, 2, 3, 4, 5, 5, 6)
  expect_equal(as.numeric(raster::values(classesFWI)), expected_classes)

  classesFFMC <- classify_index(r = r, index = "ffmc", thresholds = NULL, labels = NULL)
  expected_classes <- c(1, 1, 1, 1, 1, 1, 1, 1, 5)
  expect_equal(as.numeric(raster::values(classesFFMC)), expected_classes)

  classesDMC <- classify_index(r = r, index = "dmc", thresholds = NULL, labels = NULL)
  expected_classes <- c(1, 1, 1, 1, 2, 3, 3, 3, 5)
  expect_equal(as.numeric(raster::values(classesDMC)), expected_classes)

  classesDC <- classify_index(r = r, index = "dc", thresholds = NULL, labels = NULL)
  expected_classes <- c(1, 1, 1, 1, 1, 1, 1, 1, 5)
  expect_equal(as.numeric(raster::values(classesDC)), expected_classes)

  classesISI <- classify_index(r = r, index = "isi", thresholds = NULL, labels = NULL)
  expected_classes <- c(1, 2, 3, 4, 5, 5, 5, 5, 5)
  expect_equal(as.numeric(raster::values(classesISI)), expected_classes)

  classesBUI <- classify_index(r = r, index = "bui", thresholds = NULL, labels = NULL)
  expected_classes <- c(1, 1, 1, 1, 1, 2, 2, 3, 5)
  expect_equal(as.numeric(raster::values(classesBUI)), expected_classes)

})
