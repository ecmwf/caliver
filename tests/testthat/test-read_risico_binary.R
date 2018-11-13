context("read_risico_binary")

test_that("read_risico_binary works", {

  loaded_data <- read_risico_binary(system.file("extdata",
                                                "RISICO_binary.bin",
                                                package = "caliver"))

  test_data <- readRDS(system.file("extdata",
                                   "RISICO_raster.rds",
                                   package = "caliver"))

  expect_equal(loaded_data, test_data)

})
