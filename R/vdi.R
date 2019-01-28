#' @title vdi
#'
#' @description This function calculates the Vegetation Drought Index
#' (defined by Meteo France), as a combination of Drought Code and Duff Moisture
#' Code
#'
#' @param dc is the Raster* containing the Drought Code.
#' @param dmc is the Raster* containing the Duff Moisture Code.
#'
#' @return The function returns a categorical Raster* object.
#' Values and their descriptions are listed below:
#' \enumerate{
#' \item No fire vulnerability, consireding an important superficial
#' humidification.
#' \item Very limited drying. Small fires possible.
#' \item The zone is considered vulnerable, due to strong wind and low humidity.
#' \item Important drying; the zone is considered vulnerable. Fires can occur in
#' any considtions, excepts by high air moisture. Very severe Fire Weather
#' Danger by moderate wond, even low wind with foehn effect or very hot and
#' very dry air. Strong drougth rules are applied, the FWI is no longer
#' appropriate.
#' \item Extreme drying, the zone is considered extremely vulnerable. Very big
#' fire conditions are gathered. Permanent risk of very big fires on slope
#' zones. Catastrophic fires are possible in any zones, by moderate or string
#' wind. Strong drought rules are applied, the FWI is no longer appropriate.
#' The IPse works well and models very fast fire propagation apeeds (sometimes
#' underestimated).
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   dc <- brick("dc.nc")
#'   dmc <- brick("dmc.nc")
#'   x <- vdi(dc, dmc)
#' }
#'

vdi <- function(dc, dmc){

  dc_dmc_class <- function(dcx, dmcx){
    # Blue zone
    ifelse(dcx <= 400 & dmcx <= 20, 1,
           # Green zone
           ifelse(dcx <= 700 & dmcx <= 20, 2,
           ifelse(dcx <= 600 & dmcx <= 70, 2,
           ifelse(dcx <= 450 & dmcx <= 110, 2,
           ifelse(dcx <= 400 & dmcx <= 170, 2,
                  # Yellow zone
                  ifelse(dcx > 700 & dmcx <= 20, 3,
                  ifelse(dcx > 600 & dmcx <= 70, 3,
                  ifelse(dcx <= 650 & dmcx <= 110, 3,
                  ifelse(dcx <= 500 & dmcx <= 170, 3,
                  ifelse(dcx <= 450 & dmcx <= 200, 3,
                  ifelse(dcx <= 300 & dmcx <= 250, 3,
                         # Red zone
                         ifelse(dcx > 650 & dmcx <= 110, 4,
                         ifelse(dcx <= 1000 & dmcx <= 170, 4,
                         ifelse(dcx <= 750 & dmcx <= 200, 4,
                         ifelse(dcx <= 650 & dmcx <= 250, 4,
                         ifelse(dcx <= 600 & dmcx > 250, 4, 5))))))))))))))))
  }

  veg_drought <- raster::overlay(dc, dmc, fun = dc_dmc_class, progress = "text")

  # Define a Raster Attribute Table (RAT)
  rat <- data.frame(id = 1:5,
                    danger = c("No Vulnerability", "Limited drying",
                               "Moderate drying", "Important drying",
                               "Extreme drying"),
                    stringsAsFactors = FALSE)
  rat$id <- factor(x = rat$id, levels = 1:5)
  rat$danger <- factor(x = rat$danger,
                       levels = c("No Vulnerability", "Limited drying",
                                  "Moderate drying", "Important drying",
                                  "Extreme drying"))
  names(rat) <- c("ID", "Danger")

  veg_drought <- raster::ratify(veg_drought)
  levels(veg_drought) <- rat

  return(veg_drought)
}
