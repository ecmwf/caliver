context("plot_obs_vs_forecast")

test_that("plot_obs_vs_forecast works", {

  geffdir <- tempdir()

  x1 <- raster::raster(ncols = 20, nrows = 20)
  x1[] <- runif(400)
  raster::writeRaster(x1, file.path(geffdir,
                                    "20150101_20150101_ecfire_fc_fwi_fwi.nc"),
                      format="CDF", overwrite = TRUE)
  x2 <- raster::raster(ncols = 20, nrows = 20)
  x2[] <- runif(400)
  raster::writeRaster(x2, file.path(geffdir,
                                    "20150101_20150102_ecfire_fc_fwi_fwi.nc"),
                      format="CDF", overwrite = TRUE)
  x3 <- raster::raster(ncols = 20, nrows = 20)
  x3[] <- runif(400)
  raster::writeRaster(x3, file.path(geffdir,
                                    "20150102_20150102_ecfire_fc_fwi_fwi.nc"),
                      format="CDF", overwrite = TRUE)
  
  y <- raster::raster(ncols = 20, nrows = 20)
  y[] <- runif(400)
  y <- raster::stack(y, y)
  raster::writeRaster(y,
                      file.path(geffdir,
                                "CAMS_2015-01-01_2015-01-02_frpfire_rotated.nc"),
                      format="CDF", overwrite = TRUE)

  polyg <- as(raster::extent(-90, 90, -45, 45), "SpatialPolygons")
  raster::crs(polyg) <- raster::crs(y)

  my_plot <- plot_obs_vs_forecast(input_dir = geffdir,
                                  p = polyg,
                                  threshold = 0.5,
                                  start_date = "2015-01-01",
                                  end_date = "2015-01-02",
                                  obs_file_path = file.path(geffdir, 
                                                            "CAMS_2015-01-01_2015-01-02_frpfire_rotated.nc"),
                                  forecast_type = "fc",
                                  origin = "fwi",
                                  index = "fwi")

  expect_equal(length(my_plot$layers) == 2, TRUE)
  expect_identical(my_plot$labels$y, "Forecast date")
  expect_identical(my_plot$labels$x, "Observation date")

  test <- file.path(tempdir(), "test.png")
  png(filename = test)
    my_plot
  dev.off()

  # This is to get the fingerprint: visualTest::getFingerprint(test)
  visualTest::isSimilar(file = test,
                        fingerprint = "CA8521CA35FAF1F0",
                        threshold = 0.1)

})
