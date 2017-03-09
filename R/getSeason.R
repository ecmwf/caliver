#' Get the season for each date in a vector
#'
#' @param DATES vector of dates
#' @param WS Winter Solstice (date in the format Y-m-d)
#' @param SE Spring Equinox (date in the format Y-m-d)
#' @param SS Summer Solstice (date in the format Y-m-d)
#' @param FE Fall Equinox (date in the format Y-m-d)
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Modify default seasons
#'   seasons <- getSeason(DATES, 
#'                        SS = as.Date("2012-4-1", format = "%Y-%m-%d"), 
#'                        FE = as.Date("2012-11-1", format = "%Y-%m-%d"))
#' }
#'

getSeason <- function(DATES, WS = NULL, SE = NULL, SS = NULL, FE = NULL){

  # Winter Solstice
  if (is.null(WS)) {
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") 
  }
  
  # Spring Equinox
  if (is.null(SE)) {
    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") 
  }
  
  # Summer Solstice
  if (is.null(SS)) {
    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") 
  }
  
  # Fall Equinox
  if (is.null(FE)) {
    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") 
  }
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  season <- ifelse (d >= WS | d < SE, "Winter",
                    ifelse (d >= SE & d < SS, "Spring",
                            ifelse (d >= SS & d < FE, "Summer", "Fall")))
  
  return(season)
  
}
