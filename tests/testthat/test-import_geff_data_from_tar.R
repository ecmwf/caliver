context("test-import_geff_data_from_tar")

test_that("import_geff_data_from_tar works", {
  
  skip("Skip - deprecated function")

  geff1tar <- system.file(file.path("testdata", "geff1.tar"),
                          package = "caliver")

  s2 <- import_geff_data_from_tar(geff1tar, stack_ncfiles = FALSE)

  # Check the function returns a path
  expect_equal("character" %in% class(s2), TRUE)

})
