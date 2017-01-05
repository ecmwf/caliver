### UTILITY FUNCTIONS

# Rotate raster from -180:180 to 0:360 extent
# Author: Skipton Woolley
# Date : Feb 2015
# Licence GPL v3
# source: https://gist.github.com/skiptoniam/b5acc7b84e37fc20d514

rotate_2_360 <- function(x, filename='', ...){
  
  # require(raster)
  
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
  
  p <- raster::projection(out)	
  if (length(grep("\\+over", p)) > 0) {
    raster::projection(out) <- gsub("[[:space:]]\\+over", "", p)
  }
  
  if (filename != '') {
    out <- raster::writeRaster(out, filename, ...)
  }
  return(out)
}
