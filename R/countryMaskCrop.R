#' Mask a Raster* object using the border of a country
#'
#' @param x Raster* to mask
#' @param countryName name of the country to use as mask
#' 
#' @details The countryName is used to filter spatial polygon with borders from the \code{map} package. 
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   xmasked <- countryMaskCrop(x, countryName = "Spain")
#' }
#'

countryMaskCrop <- function(x, countryName){
  
  # Define country borders
  countryMap <- maps::map(regions=sov.expand(countryName), 
                          plot = FALSE, fill = TRUE)
  
  country <- maptools::map2SpatialPolygons(map = countryMap, 
                                           IDs = countryMap$names,
                                           proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  xMasked <- raster::mask(x, country, progress = 'text')
  xCropped <- raster::crop(xMasked, country, progress = 'text')
  
  return(xCropped)
  
}
