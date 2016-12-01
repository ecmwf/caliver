#' Define regional bounding box extent
#'
#' @description This function defines the regional bounding box extent of a map given the name of the region
#'
#' @param region string of characters describing the region. It can only assume the 15 values listed below:
#' \itemize{
#'   \item{"Global"}{or GLOB}
##'  \item{"Boreal North America"}{or BONA}
##'  \item{"Temperate North America"}{or TENA}
##'  \item{"Central America"}{or CEAM}
##'  \item{"Northern Hemisphere South America"}{or NHSA}
##'  \item{"Southern Hemisphere South America"}{or SHSA}
##'  \item{"Europe"}{or EURO}
##'  \item{"Middle East"}{or MIDE}
##'  \item{"Northern Hemisphere Africa"}{or NHAF}
##'  \item{"Southern Hemisphere Africa"}{or SHAF}
##'  \item{"Boreal Asia"}{or BOAS}
##'  \item{"Central Asia"}{or CEAS}
##'  \item{"Southeast Asia"}{or SEAS}
##'  \item{"Equatorial Asia"}{or EQAS}
##'  \item{"Australia and New Zealand"}{or AUST}
##' }
#' @param lonRange Longitude can be defined in the range "-180/+180" (default) or "0/360".
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   e <- regionalBBOX(region = "Europe")
#' }
#'


regionalBBOX <- function(region = "Globe", lonRange="0/360"){
  
  if (region == "Global" | region == "GLOB") {
    minimumLatitude <- -90
    maximumLatitude <- 90
    minimumLongitude <- 0
    maximumLongitude <- 360
  }
  if (region == "Boreal North America" | region == "BONA") {
    minimumLatitude <- 40
    maximumLatitude <- 80
    minimumLongitude <- 190
    maximumLongitude <- 330
  }
  if (region == "Temperate North America" | region == "TENA") {
    minimumLatitude <- 20
    maximumLatitude <- 55
    minimumLongitude <- 190
    maximumLongitude <- 330
  }
  if (region == "Central America" | region == "CEAM") {
    minimumLatitude <- 0
    maximumLatitude <- 40
    minimumLongitude <- 220
    maximumLongitude <- 300
  }
  if (region == "Northern Hemisphere South America" | region == "NHSA") {
    minimumLatitude <- -5
    maximumLatitude <- 15
    minimumLongitude <- 270
    maximumLongitude <- 320
  }
  if (region == "Southern Hemisphere South America" | region == "SHSA") {
    minimumLatitude <- -60
    maximumLatitude <- 5
    minimumLongitude <- 270
    maximumLongitude <- 330
  }
  if (region == "Europe" | region == "EURO") {
    minimumLatitude <- 30
    maximumLatitude <- 75
    minimumLongitude <- 335
    maximumLongitude <- 35
  }
  if (region == "Middle East" | region == "MIDE") {
    minimumLatitude <- 10
    maximumLatitude <- 45
    minimumLongitude <- 15
    maximumLongitude <- 75
  }
  if (region == "Northern Hemisphere Africa" | region == "NHAF") {
    minimumLatitude <- -5
    maximumLatitude <- 40 
    minimumLongitude <- 330
    maximumLongitude <- 60
  }
  if (region == "Southern Hemisphere Africa" | region == "SHAF") {
    minimumLatitude <- -40
    maximumLatitude <- 5
    minimumLongitude <- 360
    maximumLongitude <- 60
  }
  if (region == "Boreal Asia" | region == "BOAS") {
    minimumLatitude <- 40
    maximumLatitude <- 80
    minimumLongitude <- 25
    maximumLongitude <- 195
  }
  if (region == "Central Asia" | region == "CEAS") {
    minimumLatitude <- 15
    maximumLatitude <- 65
    minimumLongitude <- 15
    maximumLongitude <- 160
  }
  if (region == "Southeast Asia" | region == "SEAS") {
    minimumLatitude <- 0
    maximumLatitude <- 45
    minimumLongitude <- 45
    maximumLongitude <- 150
  }
  if (region == "Equatorial Asia" | region == "EQAS") {
    minimumLatitude <- -15
    maximumLatitude <- 15
    minimumLongitude <- 60
    maximumLongitude <- 190
  }
  if (region == "Australia and New Zealand" | region == "AUST") {
    minimumLatitude <- -50
    maximumLatitude <- -5
    minimumLongitude <- 100
    maximumLongitude <- 190
  }
  
  if (lonRange == "-180/+180"){
    minimumLongitude <- ifelse(minimumLongitude > 180, 
                               minimumLongitude - 360, minimumLongitude)
    maximumLongitude <- ifelse(maximumLongitude > 180, 
                               maximumLongitude - 360, maximumLongitude)
  }
  
  return(raster::extent(c(minimumLongitude, maximumLongitude, 
                          minimumLatitude, maximumLatitude)))
  
}
