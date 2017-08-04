#' @title get_fire_season
#' 
#' @description Get the season for each date in a vector
#'
#' @param DATES vector of dates
#' @param fss Fire Season Start (date in the format Y-m-d)
#' @param fse Fire Season End (date in the format Y-m-d)
#' @param zone this can either: "north", "south" or "tropics"
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Modify default seasons
#'   seasons <- get_fire_season(DATES,
#'                            fss = as.Date("2012-04-01", format = "%Y-%m-%d"),
#'                            fse = as.Date("2012-10-31", format = "%Y-%m-%d"))
#' }
#'

get_fire_season <- function(DATES, fss = NULL, fse = NULL, zone = "north"){

  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format = "2012-%m-%d"))

  if (is.null(fss) & zone == "north"){

    # Fire Season Start
    fss <- as.Date("2012-04-01",  format = "%Y-%m-%d") 
    # Fire Season End
    fse <- as.Date("2012-10-31",  format = "%Y-%m-%d")

  }

  if (is.null(fss) & zone == "south"){

    # Fire Season Start
    fss <- as.Date("2012-10-01", format = "%Y-%m-%d")
    # Fire Season End
    fse <- as.Date("2012-04-30", format = "%Y-%m-%d")

  }

  if (is.null(fse) & zone == "tropics"){

    # Fire Season Start
    fss <- as.Date("2012-07-01", format = "%Y-%m-%d")
    # Fire Season End
    fse <- as.Date("2012-10-31", format = "%Y-%m-%d")

  }

  # Season
  first_january <- as.Date("2012-01-01", format = "%Y-%m-%d")
  last_december <- as.Date("2012-12-31", format = "%Y-%m-%d")
  if (fss < fse){

    season <- ifelse(d >= fss & d <= fse, TRUE, FALSE)

  } else {

    season <- ifelse(d >= first_january & d <= fse |
                       d >= fss & d <= last_december,
                     TRUE, FALSE)

  }

  return(season)

}
