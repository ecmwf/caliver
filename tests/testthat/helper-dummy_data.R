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
r1_shifted <- raster::shift(raster::rotate(raster::shift(r1, 180)), 180)

# Define period for Reanalysis
data_dates <- seq.Date(from = as.Date("1980-01-01"),
                       to = as.Date("2016-12-31"),
                       by = "day")

# Define temporary folder
temporary_dir <- tempdir()

# Define generic polygon
shape <- raster::getData(name = "GADM", country = "Italy", level = 1)
