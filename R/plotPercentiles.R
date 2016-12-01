#' Plot maps of percentiles
#'
#' @description This function plots the maps of percentiles
#'
#' @param maps is the result of getGriddedCDF()
#' @param backgroundMap logical, if TRUE it uses a background map
#' @param rotateMap logical, if TRUE it uses a background map
#' @param cropMap logical, if TRUE it crops the map
#' @param region string of characters describing the region.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'   plotPercentiles(maps)
#' }
#'

plotPercentiles <- function(maps, 
                            backgroundMap = FALSE,
                            rotateMap = FALSE,
                            cropMap = FALSE, region = "GLOB"){
  
  if (class(maps) == "list"){
    # Generate a raster stack
    stackMap <- raster::stack(maps)
  }
  
  if (class(maps) == "RasterStack" | 
      class(maps) == "RasterBrick" | 
      class(maps) == "raster"){
    stackMap <- maps
  }
  
  if (rotateMap == TRUE){
    # Rotate a Raster* object that has x coordinates (longitude) from 0 to 360, 
    # to standard coordinates between -180 and 180 degrees. 
    # Longitude between 0 and 360 is frequently used in data 
    # from global climate models.
    rotatedMap <- raster::rotate(stackMap)
    
    if (cropMap == TRUE){
      mapExtent <- regionalBBOX(region, lonRange = "-180/+180")
      rotatedMap <- raster::crop(rotatedMap, mapExtent)
    }
    
  }else{
    rotatedMap <- stackMap
  }
  
  if (backgroundMap == TRUE){
    
    p <- rasterVis::levelplot(rotatedMap, 
                              col.regions = rev(grDevices::heat.colors(20)), 
                              colorkey = list(space = "right"))
    # Define a background map
    backgroundMap <- rworldmap::getMap(resolution = "low")
    p <- p + 
      latticeExtra::layer(sp::sp.lines(backgroundMap, lwd=0.8, col='darkgray'))
    
    print(p)
    
  }else{
    
    rasterVis::levelplot(rotatedMap, 
                         col.regions = rev(grDevices::heat.colors(20)), 
                         colorkey = list(space = "right"))
    
  }
  
}
