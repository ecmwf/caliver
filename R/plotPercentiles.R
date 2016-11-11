#' Plot maps of percentiles
#'
#' @description This function plots the maps of percentiles contained in x
#'
#' @param listOfMaps is the result of getGriddedCDF()
#' @param background This is a background map (it can be the result of rworldmap::getMap(resolution = "low"))
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   plotPercentiles(listOfMaps)
#' }
#'

plotPercentiles <- function(listOfMaps, background = NULL){
  
  p <- rasterVis::levelplot(raster::stack(listOfMaps), 
                            col.regions = rev(grDevices::heat.colors(20)), 
                            colorkey = list(space = "right"))
  
  if (!is.null(background)) {
    
    p <- p + latticeExtra::layer(sp::sp.lines(newmap, lwd=0.8, col='darkgray'))
    
  }
  
  plot(p)
  
}