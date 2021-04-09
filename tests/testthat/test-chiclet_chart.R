test_that("chiclet_chart works", {

  # Use dummy dummy forecasts in helper-dummy_data.R: `fc_list`
  # Use dummy clima generated in helper-dummy_data.R: `clima`
  # Use dummy observations generated in helper-dummy_data.R: `dummy_polygon2`

  df0 <- make_chiclet_chart(forecasts = fc_list)
  expect_equal(dim(df0), c(36, 7))

  df <- make_chiclet_chart(forecasts = fc_list,
                           p = dummy_polygon2)
  expect_equal(dim(df), c(36, 7))
  expect_equal(round(sum(df$value, na.rm = TRUE), 0), 1642)

  df <- make_chiclet_chart(forecasts = fc_list,
                           p = dummy_polygon2,
                           obs = dummy_obs)
  expect_equal(dim(df), c(36, 7))
  expect_equal(round(sum(df$value, na.rm = TRUE), 0), 1642)

  # When clima is not provided with this type, the function should fail!
  df <- try(make_chiclet_chart(forecasts = fc_list,
                               p = dummy_polygon2,
                               obs = dummy_obs,
                               type = "clima"),
            silent = TRUE)
  expect_true("try-error" %in% class(df))

  df <- make_chiclet_chart(forecasts = fc_list,
                           p = dummy_polygon2,
                           obs = dummy_obs,
                           type = "clima",
                           clima = clima)
  expect_equal(dim(df), c(36, 7))
  expect_equal(sum(df$value, na.rm = TRUE), 1800)

  chiclet1 <- plot_chiclet_chart(df0)
  expect_equal(length(chiclet1$layers), 1)

  chiclet2 <- plot_chiclet_chart(df)
  expect_equal(length(chiclet2$layers), 2)

})
