#' Plot maps of percentiles
#'
#' @description This function plots the maps of percentiles contained in x
#'
#' @param listOfMaps is the result of getGriddedCDF()
#' @param backgroundMap logical, if TRUE it uses a background map
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'   plotPercentiles(listOfMaps)
#' }
#'

plotPercentiles <- function(listOfMaps, backgroundMap = FALSE){
  
  # Generate a raster stack
  stackMap <- raster::stack(listOfMaps)
  
  p <- rasterVis::levelplot(stackMap, 
                            col.regions = rev(grDevices::heat.colors(20)), 
                            # par.settings=RdBuTheme,
                            colorkey = list(space = "right"))
  
  if (backgroundMap == TRUE) {
    # Define a background map
    backgroundMap <- rworldmap::getMap(resolution = "low")
    p <- p + 
      latticeExtra::layer(sp::sp.lines(backgroundMap, lwd=0.8, col='darkgray'))
  }
    
  print(p)
  
}
