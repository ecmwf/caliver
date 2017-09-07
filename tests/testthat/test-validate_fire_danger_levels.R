context("validate_fire_danger_levels")

# x <- raster::brick("/scratch/mo/moc0/fire/GFED4_BurnedAreas/BurnedArea.gri")
# obsTest <- raster::subset(x, 1)
# raster::writeRaster(obsTest, filename='obsTest.nc', format="CDF", overwrite=TRUE)

# y <- raster::brick("/scratch/mo/moc0/fire/GEFF/reanalysis/FWI_1980-2016.nc")
# simTest <- raster::subset(y, which(names(y) == "X2013.01.01"))
# raster::writeRaster(simTest, filename='simTest.nc', format="CDF", overwrite=TRUE)

obsTest <- tempfile()
download.file(url = "https://www.dropbox.com/s/v5dpnxyxjxf2kwq/obsTest.nc?dl=0", 
              destfile = obsTest)
x <- raster::raster(obsTest)

simTest <- tempfile()
download.file(url = "https://www.dropbox.com/s/vjk8f67rw0oy0kl/simTest.nc?dl=0", 
              destfile = simTest)
y <- raster::raster(simTest)

test_that("validate_fire_danger_levels works", {
  
  # Check whether the result is correct in case of GLOB
  tableTest <- validate_fire_danger_levels(fire_index = y,
                                           observation = x,
                                           fire_threshold = 10,
                                           obs_threshold = 50)

  expect_equal(as.vector(tableTest), c(35168, 6465, 200, 1006))
  
})
