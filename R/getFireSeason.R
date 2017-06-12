# Copyright 2016 European Centre for Medium-Range Weather Forecasts (ECMWF)
# This software is licensed under the terms of the Apache Licence Version 2.0 
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#' Get the season for each date in a vector
#'
#' @param DATES vector of dates
#' @param FSS Fire Season Start (date in the format Y-m-d)
#' @param FSE Fire Season End (date in the format Y-m-d)
#' @param zone this can either: "north", "south" or "tropics"
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Modify default seasons
#'   seasons <- getFireSeason(DATES, 
#'                            FSS = as.Date("2012-04-01", format = "%Y-%m-%d"), 
#'                            FSE = as.Date("2012-10-31", format = "%Y-%m-%d"))
#' }
#'

getFireSeason <- function(DATES, FSS = NULL, FSE = NULL, zone = "north"){
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  if (is.null(FSS) & zone == "north"){
    # Fire Season Start
    FSS <- as.Date("2012-04-01",  format = "%Y-%m-%d") 
    # Fire Season End
    FSE <- as.Date("2012-10-31",  format = "%Y-%m-%d")
  }
  
  if (is.null(FSS) & zone == "south"){
    # Fire Season Start
    FSS <- as.Date("2012-10-01", format = "%Y-%m-%d")
    # Fire Season End
    FSE <- as.Date("2012-04-30", format = "%Y-%m-%d")
  }
  
  if (is.null(FSE) & zone == "tropics"){
    # Fire Season Start
    FSS <- as.Date("2012-07-01", format = "%Y-%m-%d")
    # Fire Season End
    FSE <- as.Date("2012-10-31", format = "%Y-%m-%d")
  }
  
  # Season
  FirstJan <- as.Date("2012-01-01", format = "%Y-%m-%d")
  LastDec <- as.Date("2012-12-31", format = "%Y-%m-%d")
  if (FSS < FSE){
    season <- ifelse(d >= FSS & d <= FSE, TRUE, FALSE)
  }else{
    season <- ifelse(d >= FirstJan & d <= FSE | d >= FSS & d <= LastDec, TRUE,
                     FALSE)
  }
  
  return(season)
  
}
