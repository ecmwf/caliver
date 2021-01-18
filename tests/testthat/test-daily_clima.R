context("daily_clima")

# dummy clima
set.seed(0)
r1 <- raster(nrows = 1, ncols = 1, res = 1,
             xmn = -1, xmx = 1, ymn = -1, ymx = 1, vals = 0.3)
rr <- lapply(1:(365*3), function(i) raster::setValues(r1, runif(raster::ncell(r1))))
rr <- brick(rr)
names(rr) <- seq.Date(from = as.Date("1993-01-01"),
                      to = as.Date("1995-12-31"),
                      by = "day")

test_that("Testing the file daily_clima.R with NULL dates", {
  
  skip("Skip - this takes a long time!")
  x0 <- daily_clima(clima = rr, dates = NULL, probs = 0.5)
  expect_equal(nlayers(x0), 365)
  
})

test_that("Testing the file daily_clima.R", {

  x1 <- daily_clima(clima = rr, dates = "2018-03-01", probs = 0.5)
  expect_equal(nlayers(x1), 1)

})
