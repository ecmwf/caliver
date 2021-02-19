# Generate dummy RasterLayer
set.seed(0)
r <- raster::raster(nrows = 2, ncols = 2, # res = 0.5,
                    xmn = 0, xmx = 360, ymn = -90, ymx = 90, vals = 30)
names(r) <- as.Date("2018-01-01")
# Generate dummy RasterBrick
b <- raster::brick(lapply(1:(365 * 3),
                    function(i) raster::setValues(r,
                                                  runif(n = raster::ncell(r),
                                                        min = 0,
                                                        max = 100))))
names(b) <- seq.Date(from = as.Date("1993-01-01"),
                         to = as.Date("1995-12-31"),
                         by = "day")
clima <- list(b[[1:300]], b[[301:600]], b[[601:900]])
names(clima) <- as.Date(names(b)[1:3], format = "X%Y.%m.%d")

# Define dummy polygon
# dummy_polygon <- as(raster::extent(7, 18, 37, 40), "SpatialPolygons")
# raster::crs(dummy_polygon) <- "+proj=longlat +datum=WGS84 +no_defs"
# save(dummy_polygon, file = 'data/dummy_polygon.rda', compress='xz')
data(dummy_polygon)

# Read RISICO test data
r_risico <- readRDS(system.file("extdata", "RISICO_raster.rds",
                                package = "caliver"))
