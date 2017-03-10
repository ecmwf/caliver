#' Get the season for each date in a vector
#'
#' @param DATES vector of dates
#' @param FSS Fire Season Start (date in the format Y-m-d)
#' @param FSE Fire Season End (date in the format Y-m-d)
#' @param emisphere this can either be "north" or "south"
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Modify default seasons
#'   seasons <- getFireSeason(DATES, 
#'                        FSS = as.Date("2012-04-01", format = "%Y-%m-%d"), 
#'                        FSE = as.Date("2012-10-31", format = "%Y-%m-%d"))
#' }
#'

getFireSeason <- function(DATES, FSS = NULL, FSE = NULL, emisphere = "north"){
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  # Fire Season Start
  if (is.null(FSS) & emisphere == "north") {
    FSS <- as.Date("2012-04-01",  format = "%Y-%m-%d") 
  }else{
    FSS <- as.Date("2012-10-01", format = "%Y-%m-%d")
  }
  
  # Fire Season End
  if (is.null(FSE) & emisphere == "north") {
    FSE <- as.Date("2012-10-31",  format = "%Y-%m-%d") 
  }else{
    FSE <- as.Date("2012-04-30", format = "%Y-%m-%d")
  }
  
  if (emisphere == "north"){
    season <- ifelse(d >= FSS & d <= FSE, TRUE, FALSE)
  }else{
    FirstJan <- as.Date("2012-01-01", format = "%Y-%m-%d")
    LastDec <- as.Date("2012-12-31", format = "%Y-%m-%d")
    season <- ifelse(d >= FirstJan & d <= FSE | d >= FSS & d <= LastDec, 
                     TRUE, FALSE)
  }
  
#   if (zone == "tropics"){
#     season <- ifelse(d >= FSS & d <= FSE, TRUE, FALSE)
#   }else{
#     FirstJan <- as.Date("2012-01-01", format = "%Y-%m-%d")
#     LastDec <- as.Date("2012-12-31", format = "%Y-%m-%d")
#     season <- ifelse(d >= FirstJan & d <= FSE | d >= FSS & d <= LastDec, 
#                      TRUE, FALSE)
#   }
  
  return(season)
  
}
