#' @title subset_datacube
#'
#' @description This function subsets a datacube (RasterStack or RasterBrick)
#' based on dates.
#'
#' @param r is the Raster layer to compare to the climatology.
#' @param from string, starting date (e.g. "2018-12-30").
#' @param to string, ending date (e.g. "2018-12-31").
#'
#' @details If the \code{from} and \code{to} strings are in the format
#' "YYYY-MM-DD", they are automatically converted into a date.
#' 
#' @return The function returns a subset of \code{r}, with layer's date in the
#' range starting with \code{from} and ending with \code{to}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   r <- raster(nrows = 2, ncols = 2,
#'               xmn = 0, xmx = 360, ymn = -90, ymx = 90, vals = 30)
#'   # Generate dummy RasterBrick
#'   b <- raster::brick(lapply(1:(365 * 3),
#'                      function(i) raster::setValues(r,
#'                      unif(n = raster::ncell(r), min = 0, max = 100))))
#'   names(b) <- seq.Date(from = as.Date("1993-01-01"),
#'                        to = as.Date("1995-12-31"),
#'                        by = "day")
#'   subset_datacube(r = b, from = "1993-01-01", to = "1993-01-01")
#' }
#'

subset_datacube <- function(r, from, to){

  # Get the dates of layers in the datacube
  # This could be done by r_date <- r@z[[1]], but it won't work with random
  r_date <- substr(x = names(r), start = 2, stop = nchar(names(r)))
  r_date <- gsub(pattern = "\\.", replacement = "-", x = r_date)

  # Generate the sequence of dates to keep
  from_to_dates <- seq.Date(from = as.Date(from), to = as.Date(to), by = "day")

  # Get the climatology dates
  dates_to_keep <- which(r_date %in% as.character(from_to_dates))

  return(r[[dates_to_keep]])

}
