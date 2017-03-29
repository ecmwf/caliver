#' Mask a Raster* object using the border of a country
#'
#' @param x Raster* to mask
#' @param countryName name of the country to use as mask
#' @param mask logical (TRUE by default) to decide whether to mask the layer.
#' @param crop logical (TRUE by default) to decide whether to crop the layer.
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

countryMaskCrop <- function(x, countryName, mask = TRUE, crop = TRUE){
  
  # Define country borders
  # maps::sov.expand(countryName)
  countryMap <- maps::map(regions = countryName, 
                          plot = FALSE, fill = TRUE)
  
  country <- maptools::map2SpatialPolygons(map = countryMap, 
                                           IDs = countryMap$names,
                                           proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  
  id <- which(sapply(methods::slot(country, "polygons"), 
                     function(x) methods::slot(x, "ID")) == countryName)
  country <- country[id,]
  
  if (mask == TRUE) xMasked <- raster::mask(x, country, progress = 'text')
  if (crop == TRUE) {
    xCropped <- raster::crop(xMasked, country, progress = 'text')
  }
  
  return(xCropped)
  
}
