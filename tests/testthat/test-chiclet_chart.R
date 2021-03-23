test_that("multiplication works", {

  # Use dummy dummy forecasts in helper-dummy_data.R: `fc_list`
  # Use dummy clima generated in helper-dummy_data.R: `clima`
  # Use dummy observations generated in helper-dummy_data.R: `dummy_polygon2`

  df1 <- make_chiclet_chart(forecasts = fc_list,
                            p = dummy_polygon2,
                            obs = dummy_obs,
                            type = "raw")
  expect_equal(dim(df1), c(36, 6))
  expect_equal(round(sum(df1$value, na.rm = TRUE), 0), 1642)

  df2 <- make_chiclet_chart(forecasts = fc_list,
                            p = dummy_polygon2,
                            obs = dummy_obs,
                            type = "percentage exceeding clima",
                            clima = clima)
  expect_equal(dim(df2), c(36, 6))
  expect_equal(sum(df2$value, na.rm = TRUE), 1700)

  chiclet1 <- plot_chiclet_chart(df1)
  expect_equal(length(chiclet1$layers), 2)

  chiclet2 <- plot_chiclet_chart(df2)
  expect_equal(length(chiclet2$layers), 2)

})
