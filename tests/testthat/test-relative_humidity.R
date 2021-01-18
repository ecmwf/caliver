test_that("Testing relative_humidity", {
  
  # Check whether the result is NULL
  x <- relative_humidity(t2m = 25, # C
                         d2m = 30, # C
                         unit = "Celsius",
                         method = "August-Roche-Magnus")
  expect_equal(is.null(x), TRUE)
  
  # Check whether the result is correct using the ARM relation
  x <- relative_humidity(t2m = 30, # C
                         d2m = 25, # C
                         unit = "Celsius",
                         method = "August-Roche-Magnus")
  expect_equal(round(x, 2), 74.63)
  
  # Check whether the result is correct using the ARM relation
  x <- relative_humidity(t2m = 30 + 273.15, # K
                         d2m = 25 + 273.15, # K
                         unit = "Kelvin",
                         method = "August-Roche-Magnus")
  expect_equal(round(x, 2), 74.63)
  
  # Check whether the result is correct using the ARM relation
  x <- relative_humidity(t2m = 30 + 273.15, # K
                         d2m = 25 + 273.15, # K
                         unit = "Kelvin",
                         method = "Clausius-Clapeyron")
  expect_equal(round(x, 2), 74.69)
  
})
