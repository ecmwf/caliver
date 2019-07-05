#' @title get_fire_season_length
#'
#' @description Get a map of the length of the fire season.
#' This function is experimental and not exported for the time being.
#'
#' @param x RasterBrick containing the information.
#' @param threshold numeric value used to identify the fire season.
#'
#' @examples
#' \dontrun{
#'   # Load data
#'   era <- brick("/perm/rd/nen/claudia/era_t2_an_daymax.nc")
#'   # Calculate map of fire season length
#'   fire_length <- calc(era, get_fire_season_length)
#'   # Plot
#'   plot(rotate(fire_length, main = "Fire season length"))
#'   maps::map("world", add = TRUE, col = "lightgrey")
#'   }
#'

get_fire_season_length <- function(x, threshold = 268.15){

  y <- x > threshold
  dat_above <- which(y)

  df_above <- R.utils::seqToIntervals(dat_above)

  return(max(df_above[,2] - df_above[, 1]))

}
