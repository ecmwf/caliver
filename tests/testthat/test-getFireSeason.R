context("getFireSeason")

# Define period for Reanalysis
dataDates <- seq.Date(from = as.Date('1980-01-01'),
                      to = as.Date('2016-12-31'),
                      by = "day")

test_that("getFireSeason works for north hemisphere", {
  
  # Define a function to extract fire seasons in the north hemisphere
  seasons <- getFireSeason(DATES = dataDates, zone = 'north')
  # View(data.frame("dates" = dataDates, "season" = seasons))
  
  # Check whether the proportion of days is correct
  expect_equal(table(seasons)[[1]], 5597)
  expect_equal(table(seasons)[[2]], 7918)
})

test_that("getFireSeason works for south hemisphere", {  
  
  # Define a function to extract fire seasons in the south hemisphere
  seasons <- getFireSeason(DATES = dataDates, zone = 'south')
  
  # Check whether the proportion of days is correct
  expect_equal(table(seasons)[[1]], 5661)
  expect_equal(table(seasons)[[2]], 7854)
  
})

test_that("getFireSeason works for tropics", {  
  
  # Define a function to extract fire seasons in the tropics
  seasons <- getFireSeason(DATES = dataDates, zone = 'tropics')
  
  # Check whether the proportion of days is correct
  expect_equal(table(seasons)[[1]], 8964)
  expect_equal(table(seasons)[[2]], 4551)
  
})
