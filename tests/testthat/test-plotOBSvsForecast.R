context("plotOBSvsForecast")

test_that("plotOBSvsForecast works",{

  myTempDir <- tempdir()

  url <- "https://u23404805.dl.dropboxusercontent.com/u/23404805/caliver_test_data/CAMS_2015-01-01_2015-01-02_frpfire_rotated.nc"
  inFile <- file.path(myTempDir, "cams.nc")
  download.file(url = url, destfile = inFile)

  geffDir <- file.path(myTempDir, "geff")
  dir.create(geffDir)
  
  testfileURL <- "https://u23404805.dl.dropboxusercontent.com/u/23404805/caliver_test_data/20150101_20150101_ecfire_fc_fwi_fwi.nc"
  download.file(url = testfileURL, 
                destfile = file.path(geffDir, 
                                     "20150101_20150101_ecfire_fc_fwi_fwi.nc"))
  
  testfileURL <- "https://u23404805.dl.dropboxusercontent.com/u/23404805/caliver_test_data/20150101_20150102_ecfire_fc_fwi_fwi.nc"
  download.file(url = testfileURL, 
                destfile = file.path(geffDir, 
                                     "20150101_20150102_ecfire_fc_fwi_fwi.nc"))

  testfileURL <- "https://u23404805.dl.dropboxusercontent.com/u/23404805/caliver_test_data/20150102_20150102_ecfire_fc_fwi_fwi.nc"
  download.file(url = testfileURL, 
                destfile = file.path(geffDir, 
                                     "20150102_20150102_ecfire_fc_fwi_fwi.nc"))

  p <- plotOBSvsForecast(inDir = geffDir,
                         areaOfInterest = as(raster::extent(-9.2, -7.5,
                                                            38.8, 41.4),
                                             "SpatialPolygons"), 
                         threshold = 14, 
                         startDate = "2015-01-01",
                         endDate = "2015-01-02",
                         obsFilePath = inFile)

  expect_equal(length(p$layers) == 2, TRUE)
  expect_identical(p$labels$y, "Forecast date")
  expect_identical(p$labels$x, "Observation date")
  
  # test <- file.path(myTempDir, "test.png")
  # png(filename = test)
  #   p
  # dev.off()
  # 
  # # visualTest::getFingerprint(test)
  # visualTest::isSimilar(file = test, 
  #                       fingerprint = "A4E39A1CD9EAD834", 
  #                       threshold = 0.1)

})
