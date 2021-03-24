test_that("mask_with_fuelmodel works", {

  r_risico_masked <- try(mask_with_fuelmodel(x = r_risico), silent = TRUE)
  expect_true("try-error" %in% class(r_risico_masked))

  fuelmap <- raster::raster(system.file(file.path("extdata",
                                                  "clim_fuelmodel.nc"),
                                        package = "caliver"))
  fuelmap_cropped <- raster::crop(fuelmap, r_risico)
  fuelmap_resampled <- raster::resample(fuelmap_cropped, r_risico, method = "ngb")
  r_risico_masked <- try(mask_with_fuelmodel(x = r_risico,
                                             fuelmap = fuelmap_resampled),
                         silent = TRUE)
  expect_true("RasterLayer" %in% class(r_risico_masked))
  expect_true(sum(r_risico[], na.rm = TRUE) == 12701142)
  expect_true(sum(r_risico_masked[], na.rm = TRUE) == 12697136)
})
