context("read_risico_binary")

test_that("read_risico_binary works", {

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
