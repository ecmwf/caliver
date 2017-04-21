#' @title maskcrop
#'
#' @description mask and/or crop a Raster* based on a Polygon.
#'
#' @param r Raster* object
#' @param p SpatialPolygon* object
#' @param mask logical, TRUE to apply mask (default) using p, FALSE otherwise
#' @param crop logical, TRUE to crop (default) using p, FALSE otherwise
#' 
#' @return A Raster* object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#'   r <- raster::brick('FWI_1980-2016.nc') 
#'   p <- raster::getData(name = "GADM", country = "Italy", level = 0)        
#'   maskcrop(r, p, mask = TRUE, crop = TRUE)
#'   
#' }
#'

maskcrop <- function(r, p, mask = TRUE, crop = TRUE){

  if (mask == TRUE) {
    
    message('Masking raster over polygon')
    FWImasked <- raster::mask(r, p, progress = 'text')
    
  }else{
    
    FWImasked <- r
    
  }
  
  if (crop == TRUE) {
    
    message('Cropping raster over polygon')
    FWIareal <- raster::crop(FWImasked, p, progress = 'text')
    
  }else{
    
    FWIareal <- FWImasked
    
  }
  
  return(FWIareal)
  
}
