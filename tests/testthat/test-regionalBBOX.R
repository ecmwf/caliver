context("regionalBBOX")

test_that("regionalBBOX works", {
  
  e <- regionalBBOX(region = "Europe")
  
  expect_equal(e@xmin, 335)
  expect_equal(e@xmax, 35)
  expect_equal(e@ymin, 30)
  expect_equal(e@ymax, 75)
  
})
