context("read_risico_binary")

test_that("read_risico_binary works", {

  skip_on_appveyor()

  myTempDir <- tempdir() # works on all platforms with a platform-dependent result

  binary_file <- "RISICO_binary.bin"
  test_file <- "RISICO_raster.rds"

  loaded_data <- read_risico_binary(system.file("extdata",
                                                "RISICO",
                                                binary_file,
                                                package = "caliver"))
  test_data <- readRDS(system.file("extdata",
                                   "RISICO",
                                   test_file,
                                   package = "caliver"))

  expect_equal(loaded_data, test_data)

})
