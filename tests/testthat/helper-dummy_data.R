# Generate dummy raster and stack
set.seed(150)
r1 <- r2 <- r3 <- r4 <- r5 <- r6 <- raster::raster(ncol = 100, nrow = 50)
raster::values(r1) <- round(runif(raster::ncell(r1), 1, 25))
raster::values(r2) <- round(runif(raster::ncell(r1), 1, 25))
raster::values(r3) <- round(runif(raster::ncell(r1), 1, 25))
raster::values(r4) <- round(runif(raster::ncell(r1), 1, 25))
raster::values(r5) <- round(runif(raster::ncell(r1), 1, 25))
raster::values(r6) <- round(runif(raster::ncell(r1), 5, 25))

# Name the layers
names(r1) <- "X2018.01.01"
names(r2) <- "X2017.01.01"
names(r3) <- "X2016.01.01"
names(r4) <- "X2015.01.01"
names(r5) <- "X2014.01.01"
names(r6) <- "X2013.02.01"

# Create a stack of layers
rstack1 <- raster::stack(r2, r3, r4, r5, r6)
rextent <- raster::extent(0, 360, -90, 90)
raster::extent(rstack1) <- rextent

# Shift a raster to test rotation
r1_shifted <- raster::shift(r1, 180)

# Define generic polygon
shape <- as(raster::extent(6, 18, 35, 47), "SpatialPolygons")
# Set missing crs
raster::crs(shape) <- "+proj=longlat +datum=WGS84 +no_defs"

# Dummy clima
set.seed(0)
clima1 <- raster(nrows = 1, ncols = 1, res = 1,
                 xmn = -1, xmx = 1, ymn = -1, ymx = 1, vals = 0.3)
rr <- lapply(1:(365*3), function(i) raster::setValues(clima1, runif(raster::ncell(clima1))))
rr <- brick(rr)
names(rr) <- seq.Date(from = as.Date("1993-01-01"),
                      to = as.Date("1995-12-31"),
                      by = "day")

rr_clima <- list(rr[[1:300]], rr[[301:600]], rr[[601:900]])
names(rr_clima) <- as.Date(names(rr)[1:3], format = "X%Y.%m.%d")
