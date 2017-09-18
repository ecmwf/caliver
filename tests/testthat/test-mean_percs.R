context("mean_percs")

test_that("mean_percs works", {

  test_data <- readRDS(system.file("extdata",
                                   "RISICO",
                                   "RISICO_raster.rds",
                                   package = "caliver"))

  # Check whether the mod option works correctly
  percs <- caliver:::mean_percs(vals = test_data, perc_val = 50, mod = "gt")
  expect_equal(round(percs, 0), 72)
  percs <- caliver:::mean_percs(vals = test_data, perc_val = 50, mod = "lt")
  expect_equal(round(percs, 0), 15)
  percs <- try(caliver:::mean_percs(vals = test_data,
                                    perc_val = 50, mod = "mt"), silent = TRUE)
  expect_equal(class(percs), "try_error")

})
