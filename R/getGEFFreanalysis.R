#' @title getGEFFreanalysis
#'
#' @description Get reanalysis data from the Global ECMWF Fire Forecasting (GEFF) system
#' 
#' @param startDate this is a string with the first date to download, e.g '2015-01-01'
#' @param endDate this is a string with the last date to download, e.g '2015-12-31'
#' @param bb this is a vector containing the bounding box to get data for. By default the global extent will be downloaded. If a smaller extent is needed, this is expected in the form c(MinLatitude, Maxlatitude, MinLongitude, MaxLongitude).
#'
#' @return A RasterBrick
#'
#' @examples
#' \dontrun{
#' 
#'   # Get reanalysis data for 2015 over Italy
#'   GEFFreanalysis2015 <- getGEFFreanalysis(startDate = '2015-01-01', 
#'                                           endDate = '2015-12-31',
#'                                           bb = c(35, 47, 6, 18))
#'            
#' }
#'

getGEFFreanalysis <- function(startDate, 
                              endDate, 
                              bb = NULL){
  
  # Internal ECMWF proxy
  # # Sys.setenv(http_proxy="http://proxy.ecmwf.int:3333/")
  
  baseURL <- 'http://earthserver.ecmwf.int/rasdaman/ows?service=WCS&version=2.0.1&request=ProcessCoverages&query='
  query1 <- 'for c in (geff_fire_weather_index) return encode(c['
  query2 <- sprintf('Lat(%s:%s),Long(%s:%s),', bb[1], bb[2], bb[3], bb[4])
  query3 <- sprintf('ansi("%s":"%s")],"netcdf")', paste0(startDate, 'T00:00'), paste0(endDate, 'T00:00'))
  
  if (is.null(bb)) {
    request <- noquote(paste0(baseURL, query1, query3))
  }else{
    request <- noquote(paste0(baseURL, query1, query2, query3))
  }
  
  tempF <- file.path(tempdir(), "geff_fire_weather_index.nc")
  download.file(url = request, destfile = tempF)
  
  rea <- raster::brick(tempF)
  
  unlink(tempF)
  
  return(rea)
  
}
