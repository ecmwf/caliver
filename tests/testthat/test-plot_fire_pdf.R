context("plot_fire_pdf")

x <- s[[1]]

test_that("plot_fire_pdf match expectations for no y-limit",{
  
  p <- plot_fire_pdf(fire_index = x,
                     thresholds = c(1,2,3,4,5),
                     upper_limit = NULL,
                     v_lines = NULL)

  expect_equal(p$scales$scales[[2]]$limits, c(0, 120))
  
})

test_that("Plot layers match expectations without vLines",{
  
  p <- plot_fire_pdf(fire_index = x,
                     thresholds = c(1,2,3,4,5),
                     upper_limit = 100)
  
  expect_equal(length(p$layers), 3)
  expect_identical(p$labels$y, "Density")
  expect_identical(p$labels$x, "FWI")
  expect_equal(p$scales$scales[[2]]$limits, c(0, 100))
  
})

test_that("Plot layers match expectations with vLines",{
  
  p <- plot_fire_pdf(fire_index = x,
                     thresholds = c(1,2,3,4,5),
                     upper_limit = 100,
                     v_lines = c("90%" = 0.90))
  
  expect_equal(length(p$layers), 5)
  expect_identical(p$labels$y, "Density")
  expect_identical(p$labels$x, "FWI")
  expect_equal(p$scales$scales[[2]]$limits, c(0, 100))
  
})
