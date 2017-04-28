#' Plot maps of percentiles
#'
#' @description This function plots the maps of percentiles
#'
#' @param maps is the result of getGriddedCDF()
#' @param rotateMap logical, if TRUE it uses a background map
#' @param region string of characters describing the region.
#' @param ... additional graphical parameters inherited from plot() in the raster package.
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
    
  }else{
    
    rotatedMap <- maps
    
  }
  
  if (region != "GLOB"){
    
    maskMap <- getGFED4(varname = 'BasisRegions', region = region)
    croppedMap <- raster::trim(raster::mask(rotatedMap, maskMap))
    
  }else{
    
    croppedMap <- rotatedMap
    
  }
  
  # Define a background map
  backgroundMap <- rworldmap::getMap(resolution = "low")
  # We want to plot the background map on each layers of the stack, so we need 
  # to create a function and pass it to the addfun argument 
  # (see ?plot in the raster package)
  fun <- function() {
    plot(backgroundMap, add = TRUE, border = 'lightgray')
  }
  
  rastMin <- min(raster::cellStats(croppedMap, stat = 'min', na.rm = TRUE))
  rastMax <- max(raster::cellStats(croppedMap, stat = 'max', na.rm = TRUE))
  
  # Define palette (from: rev(grDevices::heat.colors(n = 10)))
  heatcolors <- c("#FFFFBFFF", "#FFFF40FF", "#FFFF00FF", "#FFDB00FF", 
                  "#FFB600FF", "#FF9200FF", "#FF6D00FF", "#FF4900FF",
                  "#FF2400FF", "#FF0000FF")
  
  raster::plot(croppedMap, 
               addfun = fun, 
               col = heatcolors,
               breaks = round(seq(from = rastMin, 
                                  to = rastMax, 
                                  length.out = 10), 0))
  
}
