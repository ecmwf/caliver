context("forecast_summary")

test_that("the forecast_summary functions behaves as expected", {

  # Generate dummy 10-day forecasts to test forecast_summary()
  s1 <- replicate(10, r1)
  s1 <- raster::stack(s1)
  raster::writeRaster(s1,
                      filename = file.path(temporary_dir,
                                           "ECMWF_FWI_20180101_1200_hr_fwi.nc"),
                      format = "CDF", overwrite = TRUE)
  s2 <- replicate(10, r2)
  s2 <- raster::stack(s2)
  raster::writeRaster(s2,
                      filename = file.path(temporary_dir,
                                           "ECMWF_FWI_20180102_1200_hr_fwi.nc"),
                      format = "CDF", overwrite = TRUE)
  s3 <- replicate(10, r3)
  s3 <- raster::stack(s3)
  raster::writeRaster(s3,
                      filename = file.path(temporary_dir,
                                           "ECMWF_FWI_20180103_1200_hr_fwi.nc"),
                      format = "CDF", overwrite = TRUE)

  # Get forecasts from files
  x <- forecast_summary(input_dir = temporary_dir,
                        r = NULL,
                        p = NULL,
                        threshold = 5,
                        start_date = "2018-01-01",
                        end_date = "2018-01-03",
                        obs = NULL,
                        origin = "FWI",
                        index = "fwi")
  expect_equal(x$labels, structure(list(x = "Observation date",
                                        y = "Forecast date",
                                        fill = "value"),
                                   .Names = c("x", "y", "fill")))
  expect_equal(x$data$value, c(82, NA, NA, 82, 81, NA, 82, 81, 81))

  # Get forecasts from RasterStack
  y <- forecast_summary(input_dir = NULL,
                        r = raster::stack(s1 * 2, s2 * 3, s3 * 4),
                        p = shape,
                        threshold = 35,
                        start_date = "2018-01-01",
                        end_date = "2018-01-03",
                        obs = raster::brick(s1 * 10),
                        origin = "FWI",
                        index = "fwi")

  expect_equal(y$data$value, c(0, NA, NA, 0, 100, NA, 0, 100, 0))

})
