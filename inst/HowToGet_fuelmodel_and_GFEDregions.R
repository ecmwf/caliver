# Rotate raster from -180:180 to 0:360 extent
# Author: Skipton Woolley
# Date : Feb 2015
# Licence GPL v3
# source: https://gist.github.com/skiptoniam/b5acc7b84e37fc20d514

rotate_2_360 <- function(x, filename='', ...){
  require(raster)
  e <- raster::extent(x)
  xrange <- e@xmax - e@xmin
  if (xrange < 350 | xrange > 370 | e@xmin < -190 | e@xmax > 190) {
    
    warning('this does not look like an appropriate object for this function, use the rotate function in raster package')
    
  }
  
  ext1 <- extent(0, 180, -90, 90)
  if (is.null(intersect(e, ext1 ))) {
    r1 <- NULL
  } else {
    r1 <- raster::crop(x, ext1)
  }		
  #ext2 <- extent(-180+res(x)[1], 0, -90, 90)
  ext2 <- raster::extent(-180, 0, -90, 90)
  if (is.null(intersect(e, ext2 ))) {
    r2 <- NULL
  } else {
    r2 <- raster::crop(x, ext2)
    r2 <- raster::shift(r2, 360)
  }
  ln <- names(x)
  if (is.null(r1)) {
    out <- r2
  } else if (is.null(r2)) {
    out <- r1		
  } else {
    out <- raster::merge(r1, r2, overlap=FALSE)
  }
  names(out) <- names(x)
  out@z <- x@z
  
  p <- projection(out)	
  if (length(grep("\\+over", p)) > 0) {
    projection(out) <- gsub("[[:space:]]\\+over", "", p)
  }
  
  if (filename != '') {
    out <- raster::writeRaster(out, filename, ...)
  }
  return(out)
}

# Fuelmodel
fuelmodel <- raster::raster("/var/tmp/moc0/geff/clim_fuelmodel.nc_ggN256")
raster::extent(fuelmodel)
fuelmodel@crs <- probsMaps[[1]]@crs
rasterVis::levelplot(fuelmodel)
save(fuelmodel, file = "/home/mo/moc0/Repositories/caliver/data/fuelmodel.rda")

# Download one of the GFED4 files
fname <- tempfile()
download.file(url = "http://www.falw.vu/~gwerf/GFED/GFED4/GFED4.1s_1997.hdf5",
              destfile = fname)
# source("http://bioconductor.org/biocLite.R")
# biocLite("rhdf5")
library("rhdf5")
# list content
h5ls(fname)
# estract only the basis regions
basis_regions <- h5read(file = fname, name = "ancill/basis_regions")
# convert them to raster, transpose x and y, fix the extent, resample and set 0s to NA
basis_regions <- raster::raster(basis_regions)
raster::plot(basis_regions)
basis_regions <- raster::t(basis_regions)
raster::plot(basis_regions)
raster::extent(basis_regions) <- raster::extent(raster::rotate(probsMaps[[1]]))
raster::plot(basis_regions)
basis_regions <- rotate_2_360(basis_regions)
#basis_regions <- raster::rotate(basis_regions)
raster::plot(basis_regions)
basis_regions[basis_regions == 0] <- NA
basis_regions@crs <- probsMaps[[1]]@crs
#  Repsample using the attributes of the low res raster for output
GFEDregions <- raster::resample(basis_regions, probsMaps[[1]], method = "ngb")
# Plot
rasterVis::levelplot(GFEDregions)
raster::plot(GFEDregions)
# Looks good! This file is also saved in the data folder of this package
# save(GFEDregions, file = "/home/mo/moc0/Repositories/caliver/data/GFEDregions.rda")
# data("GFEDregions")