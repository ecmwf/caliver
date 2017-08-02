context("plotOBSvsForecast")

myTempDir <- tempdir()

url <- "https://www.dropbox.com/s/53kuamyycc285f1/CAMS_2017-06-15_2017-06-17_frpfire_rotated.nc?dl=0"
inFile <- file.path(myTempDir, "cams.nc")
download.file(url = url, destfile = inFile)

geffDir <- file.path(myTempDir, "geff")
dir.create(geffDir)

url <- "https://www.dropbox.com/s/2fszm9btqewxdqf/20170615_20170615_FWI.nc?dl=0"
download.file(url = url, destfile = file.path(geffDir, "20170615_20170615_FWI.nc"))
url <- "https://www.dropbox.com/s/2fszm9btqewxdqf/20170615_20170616_FWI.nc?dl=0"
download.file(url = url, destfile = file.path(geffDir, "20170615_20170616_FWI.nc"))
url <- "https://www.dropbox.com/s/2fszm9btqewxdqf/20170615_20170617_FWI.nc?dl=0"
download.file(url = url, destfile = file.path(geffDir, "20170615_20170617_FWI.nc"))
url <- "https://www.dropbox.com/s/ql22wlwky516y59/20170616_20170616_FWI.nc?dl=0"
download.file(url = url, destfile = file.path(geffDir, "20170616_20170616_FWI.nc"))
url <- "https://www.dropbox.com/s/ql22wlwky516y59/20170616_20170617_FWI.nc?dl=0"
download.file(url = url, destfile = file.path(geffDir, "20170616_20170617_FWI.nc"))
url <- "https://www.dropbox.com/s/885lnpcni4mahik/20170617_20170617_FWI.nc?dl=0"
download.file(url = url, destfile = file.path(geffDir, "20170617_20170617_FWI.nc"))

test_that("plotPDF match expectations for no y-limit",{
  
  p <- plotOBSvsForecast(inDir = geffDir, 
                         areaOfInterest = as(raster::extent(-9.2, -7.5, 38.8, 41.4),
                                             "SpatialPolygons"), 
                         threshold = 14, 
                         startDate = "2017-06-15", endDate = "2017-06-17",
                         obsFilePath = inFile)
  
  expect_equal(length(p$layers) == 2, TRUE)
  expect_identical(p$labels$y, "Forecast date")
  expect_identical(p$labels$x, "Observation date")
  
})
