#' Define regional bounding box extent
#'
#' @description This function defines the regional bounding box extent of a map given the name of the region
#'
#' @param region string of characters describing the region. It can only assume the 12 values listed below:
#' \itemize{
##'  \item{"Globe"}{or global} - default extent
##'  \item{"North America"}{or NAme}
##'  \item{"Central America"}{or CAme}
##'  \item{"South America"}{or SAme}
##'  \item{"Europe"}{or Euro}
##'  \item{"North Africa"}{or NHAf}
##'  \item{"South Africa"}{or SHAf}
##'  \item{"North Asia"}{or NAsi}
##'  \item{"South Asia"}{or SAsi}
##'  \item{"Tropical Asia"}{or TAsi}
##'  \item{"Australia"}{or Aust}
##'  \item{"East of Moscow"}{or EoMo}
##' }
##' @param lonRange Longitude can be defined in the range "-180/+180" (default) or "0/360".
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   e <- regionalBBOX(region = "Europe")
#' }
#'


regionalBBOX <- function(region = "Globe", lonRange="-180/+180"){
  
  if (region == "Globe" | region == "global") {
    minimumLatitude <- -90
    maximumLatitude <- 90
    minimumLongitude <- 0
    maximumLongitude <- 360
    if (lonRange == "-180/+180"){
      minimumLongitude <- ifelse(minimumLongitude > 180, 
                                 minimumLongitude - 360, minimumLongitude)
      maximumLongitude <- ifelse(maximumLongitude > 180, 
                                 maximumLongitude - 360, maximumLongitude)
    }
  }
  if (region == "North America" | region == "NAme") {
    minimumLatitude <- 30
    maximumLatitude <- 75
    minimumLongitude <- 190
    maximumLongitude <- 330
    if (lonRange == "-180/+180"){
      minimumLongitude <- ifelse(minimumLongitude > 180, 
                                 minimumLongitude - 360, minimumLongitude)
      maximumLongitude <- ifelse(maximumLongitude > 180, 
                                 maximumLongitude - 360, maximumLongitude)
    }
  }
  if (region == "Central America" | region == "CAme") {
    minimumLatitude <- 0
    maximumLatitude <- 30
    minimumLongitude <- 190
    maximumLongitude <- 330
    if (lonRange == "-180/+180"){
      minimumLongitude <- ifelse(minimumLongitude > 180, 
                                 minimumLongitude - 360, minimumLongitude)
      maximumLongitude <- ifelse(maximumLongitude > 180, 
                                 maximumLongitude - 360, maximumLongitude)
    }
  }
  if (region == "South America" | region == "SAme") {
    minimumLatitude <- -60
    maximumLatitude <- 0
    minimumLongitude <- 190
    maximumLongitude <- 330
    if (lonRange == "-180/+180"){
      minimumLongitude <- ifelse(minimumLongitude > 180, 
                                 minimumLongitude - 360, minimumLongitude)
      maximumLongitude <- ifelse(maximumLongitude > 180, 
                                 maximumLongitude - 360, maximumLongitude)
    }
  }
  if (region == "Europe" | region == "Euro") {
    minimumLatitude <- 30
    maximumLatitude <- 75
    minimumLongitude <- 330
    maximumLongitude <- 60
    if (lonRange == "-180/+180"){
      minimumLongitude <- ifelse(minimumLongitude > 180, 
                                 minimumLongitude - 360, minimumLongitude)
      maximumLongitude <- ifelse(maximumLongitude > 180, 
                                 maximumLongitude - 360, maximumLongitude)
    }
  }
  if (region == "North Africa" | region == "NHAf") {
    minimumLatitude <- 0
    maximumLatitude <- 30 
    minimumLongitude <- 330
    maximumLongitude <- 60
    if (lonRange == "-180/+180"){
      minimumLongitude <- ifelse(minimumLongitude > 180, 
                                 minimumLongitude - 360, minimumLongitude)
      maximumLongitude <- ifelse(maximumLongitude > 180, 
                                 maximumLongitude - 360, maximumLongitude)
    }
  }
  if (region == "South Africa" | region == "SHAf") {
    minimumLatitude <- -35
    maximumLatitude <- 0
    minimumLongitude <- 330
    maximumLongitude <- 60
    if (lonRange == "-180/+180"){
      minimumLongitude <- ifelse(minimumLongitude > 180, 
                                 minimumLongitude - 360, minimumLongitude)
      maximumLongitude <- ifelse(maximumLongitude > 180, 
                                 maximumLongitude - 360, maximumLongitude)
    }
  }
  if (region == "North Asia" | region == "NAsi") {
    minimumLatitude <- 30
    maximumLatitude <- 75
    minimumLongitude <- 60
    maximumLongitude <- 190
    if (lonRange == "-180/+180"){
      minimumLongitude <- ifelse(minimumLongitude > 180, 
                                 minimumLongitude - 360, minimumLongitude)
      maximumLongitude <- ifelse(maximumLongitude > 180, 
                                 maximumLongitude - 360, maximumLongitude)
    }
  }
  if (region == "South Asia" | region == "SAsi") {
    minimumLatitude <- 10
    maximumLatitude <- 30
    minimumLongitude <- 60
    maximumLongitude <- 190
    if (lonRange == "-180/+180"){
      minimumLongitude <- ifelse(minimumLongitude > 180, 
                                 minimumLongitude - 360, minimumLongitude)
      maximumLongitude <- ifelse(maximumLongitude > 180, 
                                 maximumLongitude - 360, maximumLongitude)
    }
  }
  if (region == "Tropical Asia" | region == "TAsi") {
    minimumLatitude <- -10
    maximumLatitude <- 10
    minimumLongitude <- 60
    maximumLongitude <- 190
    if (lonRange == "-180/+180"){
      minimumLongitude <- ifelse(minimumLongitude > 180, 
                                 minimumLongitude - 360, minimumLongitude)
      maximumLongitude <- ifelse(maximumLongitude > 180, 
                                 maximumLongitude - 360, maximumLongitude)
    }
  }
  if (region == "Australia" | region == "Aust") {
    minimumLatitude <- -50
    maximumLatitude <- -10
    minimumLongitude <- 60
    maximumLongitude <- 190
    if (lonRange == "-180/+180"){
      minimumLongitude <- ifelse(minimumLongitude > 180, 
                                 minimumLongitude - 360, minimumLongitude)
      maximumLongitude <- ifelse(maximumLongitude > 180, 
                                 maximumLongitude - 360, maximumLongitude)
    }
  }
  if (region == "East of Moscow" | region == "EoMo") {
    minimumLatitude <- 50
    maximumLatitude <- 60
    minimumLongitude <- 35
    maximumLongitude <- 55
    if (lonRange == "-180/+180"){
      minimumLongitude <- ifelse(minimumLongitude > 180, 
                                 minimumLongitude - 360, minimumLongitude)
      maximumLongitude <- ifelse(maximumLongitude > 180, 
                                 maximumLongitude - 360, maximumLongitude)
    }
  }
  
  return(raster::extent(c(minimumLongitude, maximumLongitude, 
                          minimumLatitude, maximumLatitude)))
  
}
