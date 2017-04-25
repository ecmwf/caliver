context("getFireSeason")

test_that("getFireSeason works", {
  
  # Define period for Reanalysis
  dataDates <- seq.Date(from = as.Date('1980-01-01'),
                        to = as.Date('2016-12-31'),
                        by = "day")
  
  # Define a function to extract fire seasons in Europe
  seasons <- getFireSeason(DATES = dataDates, emisphere = 'north')
  
  # Check whether the proportion of days is correct
  expect_equal(table(seasons)[[1]], 5597)
  expect_equal(table(seasons)[[2]], 7918)
  
  # Define a function to extract fire seasons in Europe
  seasons <- getFireSeason(DATES = dataDates, emisphere = 'south')
  
  # Check whether the proportion of days is correct
  expect_equal(table(seasons)[[1]], 5661)
  expect_equal(table(seasons)[[2]], 7854)
  
})
