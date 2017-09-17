context("test-import_geff_data_from_tar")

test_that("import_geff_data_from_tar works", {

  expect_equal("RasterBrick" %in% class(s), TRUE)
  expect_equal(all(dim(s) == c(256, 512, 5)), TRUE)

  geff1tar <- system.file(file.path("testdata", "geff1.tar"),
                          package = "caliver")

  s2 <- import_geff_data_from_tar(geff1tar, stack_ncfiles = FALSE)
  expect_equal("character" %in% class(s2), TRUE)

})
