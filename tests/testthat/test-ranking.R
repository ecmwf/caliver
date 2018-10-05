context("ranking")

test_that("Testing the file ranking.R", {

  # Check whether the result is correct
  x <- ranking(s[[1]], s)
  expect_equal(cellStats(x, max), 7)

  p <- plot_ranking(x)
  expect_equal(round(p$rect$w, 0), 76)
  expect_equal(round(p$rect$h, 0), 115)

})
