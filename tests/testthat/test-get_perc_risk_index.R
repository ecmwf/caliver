context("get_perc_risk_index")

test_that("get_perc_risk_index works", {

  output_gt_75 <- get_perc_risk_index(b = r_risico, poly = shape,
                                      perc_val = 75, mod = "gt")
  # Check if the output table has the same dimension as the shapefile
  expect_equal(round(output_gt_75, 0), 104)

  output_lt_50 <- get_perc_risk_index(b = r_risico, poly = shape,
                                      perc_val = 50, mod = "lt")
  # Check if the output table has the same dimension as the shapefile
  expect_equal(round(output_lt_50, 0), 15)
  expect_equal(length(output_lt_50), 1)

})
