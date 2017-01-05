#' Define a mask for the regional extent
#'
#' @description This function defines the regional extent of a map given the name of the region. It is based on the GFED basis regions defined here: www.globalfiredata.org/data.html
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
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   e <- regionalMask(region = "Europe")
#' }
#'

regionalMask <- function(region = "GLOB"){
  
  load(system.file("data", "GFEDregions.rda", package="caliver"))
  
  if (region == "Boreal North America" | region == "BONA") {
    GFEDregions[GFEDregions != 1] <- NA
  }
  if (region == "Temperate North America" | region == "TENA") {
    GFEDregions[GFEDregions != 2] <- NA
  }
  if (region == "Central America" | region == "CEAM") {
    GFEDregions[GFEDregions != 3] <- NA
  }
  if (region == "Northern Hemisphere South America" | region == "NHSA") {
    GFEDregions[GFEDregions != 4] <- NA
  }
  if (region == "Southern Hemisphere South America" | region == "SHSA") {
    GFEDregions[GFEDregions != 5] <- NA
  }
  if (region == "Europe" | region == "EURO") {
    GFEDregions[GFEDregions != 6] <- NA
  }
  if (region == "Middle East" | region == "MIDE") {
    GFEDregions[GFEDregions != 7] <- NA
  }
  if (region == "Northern Hemisphere Africa" | region == "NHAF") {
    GFEDregions[GFEDregions != 8] <- NA
  }
  if (region == "Southern Hemisphere Africa" | region == "SHAF") {
    GFEDregions[GFEDregions != 9] <- NA
  }
  if (region == "Boreal Asia" | region == "BOAS") {
    GFEDregions[GFEDregions != 10] <- NA
  }
  if (region == "Central Asia" | region == "CEAS") {
    GFEDregions[GFEDregions != 11] <- NA
  }
  if (region == "Southeast Asia" | region == "SEAS") {
    GFEDregions[GFEDregions != 12] <- NA
  }
  if (region == "Equatorial Asia" | region == "EQAS") {
    GFEDregions[GFEDregions != 13] <- NA
  }
  if (region == "Australia and New Zealand" | region == "AUST") {
    GFEDregions[GFEDregions != 14] <- NA
  }
  
  return(GFEDregions)
  
}
