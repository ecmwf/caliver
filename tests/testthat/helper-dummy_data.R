# Generate dummy RasterLayer
set.seed(0)
r <- raster::raster(nrows = 2, ncols = 2, # res = 0.5,
                    xmn = 0, xmx = 360, ymn = -90, ymx = 90, vals = 30)
names(r) <- as.Date("2018-01-01")

# Generate dummy forecasts
fc1 <- raster::brick(lapply(1:10,
                            function(i) raster::setValues(r,
                                                          runif(n = raster::ncell(r),
                                                                min = 0,
                                                                max = 100))))
names(fc1) <- as.Date(x = seq.Date(from = as.Date("2018-01-01"), to = as.Date("2018-01-10"), by = "day"), format = "X%Y.%m.%d")
fc2 <- raster::brick(lapply(1:10,
                            function(i) raster::setValues(r,
                                                          runif(n = raster::ncell(r),
                                                                min = 0,
                                                                max = 100))))
names(fc2) <- as.Date(x = seq.Date(from = as.Date("2018-01-02"), to = as.Date("2018-01-11"), by = "day"), format = "X%Y.%m.%d")
fc3 <- raster::brick(lapply(1:10,
                            function(i) raster::setValues(r,
                                                          runif(n = raster::ncell(r),
                                                                min = 0,
                                                                max = 100))))
names(fc3) <- as.Date(x = seq.Date(from = as.Date("2018-01-03"), to = as.Date("2018-01-12"), by = "day"), format = "X%Y.%m.%d")
fc_list <- list(fc1, fc2, fc3)

# Generate dummy observations
dummy_obs <- raster::brick(lapply(1:(10 + 2),
                                  function(i) raster::setValues(r,
                                                                runif(n = raster::ncell(r),
                                                                      min = 0,
                                                                      max = 100))))
names(dummy_obs) <- as.Date(x = seq.Date(from = as.Date("2018-01-10"), to = as.Date("2018-01-21"), by = "day"), format = "X%Y.%m.%d.00.00.00")

# Generate dummy climatology
b <- raster::brick(lapply(1:(365 * 3),
                    function(i) raster::setValues(r,
                                                  runif(n = raster::ncell(r),
                                                        min = 0,
                                                        max = 100))))
names(b) <- seq.Date(from = as.Date("1993-01-01"),
                     to = as.Date("1995-12-31"),
                     by = "day")
clima <- daily_clima(b = b, dates = seq.Date(from = as.Date("2018-01-01"),
                                            to = as.Date("2018-01-12"),
                                            by = "day"))

# Define dummy polygons
dummy_polygon <- as(raster::extent(7, 18, 37, 40), "SpatialPolygons")
raster::crs(dummy_polygon) <- "+proj=longlat +datum=WGS84 +no_defs"

dummy_polygon2 <- as(raster::extent(0, 180, 0, 60), "SpatialPolygons")
raster::crs(dummy_polygon2) <- "+proj=longlat +datum=WGS84 +no_defs"

# Read RISICO test data
r_risico <- readRDS(system.file("extdata", "RISICO_raster.rds",
                                package = "caliver"))
