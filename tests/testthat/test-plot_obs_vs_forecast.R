# context("plot_obs_vs_forecast")
# 
# test_that("plot_obs_vs_forecast works",{
#   
#   skip("skip temporarily for testing")
# 
#   url <- "https://www.dropbox.com/s/939t7tipitme17o/CAMS_2015-01-01_2015-01-02_frpfire_rotated.nc?dl=0"
#   input_file <- file.path(tempdir(), "cams.nc")
#   download.file(url = url, destfile = input_file)
# 
#   geffDir <- file.path(tempdir(), "geff")
#   dir.create(geffDir)
#   
#   testfileURL <- "https://www.dropbox.com/s/5isrbxg6qvrnxns/20150101_20150101_ecfire_fc_fwi_fwi.nc?dl=0"
#   download.file(url = testfileURL, 
#                 destfile = file.path(geffDir, 
#                                      "20150101_20150101_ecfire_fc_fwi_fwi.nc"))
#   
#   testfileURL <- "https://www.dropbox.com/s/796cq51n2scddno/20150101_20150102_ecfire_fc_fwi_fwi.nc?dl=0"
#   download.file(url = testfileURL, 
#                 destfile = file.path(geffDir, 
#                                      "20150101_20150102_ecfire_fc_fwi_fwi.nc"))
# 
#   testfileURL <- "https://www.dropbox.com/s/yr135dyqm8sv2ax/20150102_20150102_ecfire_fc_fwi_fwi.nc?dl=0"
#   download.file(url = testfileURL, 
#                 destfile = file.path(geffDir, 
#                                      "20150102_20150102_ecfire_fc_fwi_fwi.nc"))
# 
#   my_plot <- plot_obs_vs_forecast(input_dir = geffDir,
#                                   p = as(raster::extent(-9.2, -7.5, 38.8, 41.4),
#                                          "SpatialPolygons"), 
#                          threshold = 14, 
#                          start_date = "2015-01-01",
#                          end_date = "2015-01-02",
#                          obs_file_path = input_file)
# 
#   expect_equal(length(my_plot$layers) == 2, TRUE)
#   expect_identical(my_plot$labels$y, "Forecast date")
#   expect_identical(my_plot$labels$x, "Observation date")
#   
#   # test <- file.path(tempdir(), "test.png")
#   # png(filename = test)
#   #   p
#   # dev.off()
#   # 
#   # # visualTest::getFingerprint(test)
#   # visualTest::isSimilar(file = test, 
#   #                       fingerprint = "A4E39A1CD9EAD834", 
#   #                       threshold = 0.1)
# 
# })
