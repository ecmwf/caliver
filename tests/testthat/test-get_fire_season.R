context("get_fire_season")

# Define period for Reanalysis
data_dates <- seq.Date(from = as.Date("1980-01-01"),
                       to = as.Date("2016-12-31"),
                       by = "day")

test_that("get_fire_season works for north hemisphere", {

  # Define a function to extract fire seasons in the north hemisphere
  seasons <- get_fire_season(dates = data_dates, zone = "north")

  # Check whether the proportion of days is correct
  expect_equal(table(seasons)[[1]], 5597)
  expect_equal(table(seasons)[[2]], 7918)
})

test_that("get_fire_season works for south hemisphere", {

  # Define a function to extract fire seasons in the south hemisphere
  seasons <- get_fire_season(dates = data_dates, zone = "south")

  # Check whether the proportion of days is correct
  expect_equal(table(seasons)[[1]], 5661)
  expect_equal(table(seasons)[[2]], 7854)

})

test_that("get_fire_season works for tropics", {

  # Define a function to extract fire seasons in the tropics
  seasons <- get_fire_season(dates = data_dates, zone = "tropics")

  # Check whether the proportion of days is correct
  expect_equal(table(seasons)[[1]], 6744)
  expect_equal(table(seasons)[[2]], 6771)

})
