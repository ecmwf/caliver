context("test-import_geff_data_from_tar")

test_that("import_geff_data_from_tar works", {

  s <- import_geff_data_from_tar(geff5tar)

  expect_equal("RasterBrick" %in% class(s), TRUE)
  expect_equal(all(dim(s) == c(256, 512, 5)), TRUE)

})
