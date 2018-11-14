context("forecast_summary")

test_that("the forecast_summary functions behaves as expected", {

  # Generate dummy 10-day forecasts to test forecast_summary()
  s1 <- replicate(10 , r1)
  s1 <- raster::stack(s1)
  raster::writeRaster(s1,
                      filename = file.path(temporary_dir,
                                           "ECMWF_FWI_20180101_1200_hr_fwi.nc"),
                      format = "CDF", overwrite = TRUE)
  s2 <- replicate(10 , r2)
  s2 <- raster::stack(s2)
  raster::writeRaster(s2,
                      filename = file.path(temporary_dir,
                                           "ECMWF_FWI_20180102_1200_hr_fwi.nc"),
                      format = "CDF", overwrite = TRUE)
  s3 <- replicate(10 , r3)
  s3 <- raster::stack(s3)
  raster::writeRaster(s3,
                      filename = file.path(temporary_dir,
                                           "ECMWF_FWI_20180103_1200_hr_fwi.nc"),
                      format = "CDF", overwrite = TRUE)

  x <- forecast_summary(input_dir = temporary_dir,
                        r = NULL,
                        p = NULL,
                        threshold = 5,
                        start_date = "2018-01-01",
                        end_date= "2018-01-03",
                        obs_file_path = NULL,
                        origin = "FWI",
                        index = "fwi")

  expect_equal(final_plot$labels, structure(list(x = "Observation date",
                                                 y = "Forecast date",
                                                 fill = "value"),
                                            .Names = c("x", "y", "fill")))
  expect_equal(final_plot$data$value, c(82, NA, NA, 82, 81, NA, 82, 81, 81))

  unlink(file.path(temporary_dir, "ECMWF_FWI_20180101_1200_hr_fwi.nc"))
  unlink(file.path(temporary_dir, "ECMWF_FWI_20180102_1200_hr_fwi.nc"))
  unlink(file.path(temporary_dir, "ECMWF_FWI_20180103_1200_hr_fwi.nc"))

})
