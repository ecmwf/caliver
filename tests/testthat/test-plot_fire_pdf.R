context("plot_fire_pdf")

test_that("plot_fire_pdf match expectations for no FWI upper_limit", {

  # skip("Skip")
  p <- plot_fire_pdf(fire_index = r1,
                     thresholds = c(1, 2, 3, 4, 5),
                     upper_limit = NULL,
                     v_lines = NULL)

  expect_equal(p$scales$scales[[2]]$limits, c(0, 25))
  expect_equal(length(p$layers), 2)
  expect_identical(p$labels$y, "Density")
  expect_identical(p$labels$x, "FWI")

})

test_that("Plot layers match expectations with FWI upper_limit", {

  p <- plot_fire_pdf(fire_index = r1,
                     thresholds = c(1, 2, 3, 4, 5),
                     upper_limit = 15,
                     v_lines = NULL)

  expect_equal(p$scales$scales[[2]]$limits, c(0, 15))

})

test_that("Plot layers match expectations with vLines", {

  p <- plot_fire_pdf(fire_index = r1,
                     thresholds = c(1, 2, 3, 4, 5),
                     upper_limit = 30,
                     v_lines = c("90%" = 0.90))

  expect_equal(length(p$layers), 4)

})
