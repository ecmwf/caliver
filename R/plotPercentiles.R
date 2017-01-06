#' Plot maps of percentiles
#'
#' @description This function plots the maps of percentiles
#'
#' @param maps is the result of getGriddedCDF()
#' @param rotateMap logical, if TRUE it uses a background map
#' @param region string of characters describing the region.
#' @param ... additional graphical parameters inherited from raster::plot().
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'   plotPercentiles(maps)
#' }
#'

plotPercentiles <- function(maps, rotateMap = FALSE, region = "GLOB", ...){
  
  if (rotateMap == TRUE){
    
    # Rotate a Raster* object that has x coordinates (longitude) from 0 to 360, 
    # to standard coordinates between -180 and 180 degrees. 
    # Longitude between 0 and 360 is frequently used in data 
    # from global climate models.
    rotatedMap <- raster::rotate(maps)
    lonRange = "-180/+180"
    
  }else{
    
    rotatedMap <- maps
    lonRange = "0/360"
    
  }
  # raster::plot(rotatedMap)
  
  if (region != "GLOB"){
    
    maskMap <- regionalMask(region)
    maskMap <- raster::resample(maskMap, rotatedMap, method = "ngb")
    croppedMap <- raster::trim(raster::mask(rotatedMap, maskMap))
    # mapExtent <- regionalBBOX(region, lonRange = lonRange)
    # rotatedMap <- raster::crop(rotatedMap, mapExtent)
    
  }else{
    
    croppedMap <- rotatedMap
    
  }
  # raster::plot(croppedMap)
  
  # Define a background map
  backgroundMap <- rworldmap::getMap(resolution = "low")
  # We want to plot the background map on each layers of the stack, so we need 
  # to create a function and pass it to the addfun argument (see ?raster::plot)
  fun <- function() {
    raster::plot(backgroundMap, add = TRUE, border = 'lightgray')
  }
  
  rastMin <- min(raster::cellStats(croppedMap, stat = 'min', na.rm = TRUE))
  rastMax <- max(raster::cellStats(croppedMap, stat = 'max', na.rm = TRUE))
  
  raster::plot(croppedMap, addfun = fun, 
               col = rev(heat.colors(10, alpha = 1)),
               breaks = round(seq(from = rastMin, to = rastMax,
                                  length.out = 10), 0), ...)
  
}
