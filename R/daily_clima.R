#' @title daily_clima
#'
#' @description This function generates daily climatological maps.
#'
#' @param b RasterBrick/Stack containing the historical observations or a proxy
#' (typically a reanalysis) that is used to derive the climatological
#' information. This needs to contain daily layers for multiple years.
#' \code{names(b)} should contain dates for comparison
#' (e.g. X2017.01.01).
#' @param dates Dates for which we need to calculate daily climatology.
#' By default, this is a leap year.
#' 
#' @return The function returns a RasterBrick (if \code{dates} contains one date)
#' or a list of RasterBricks (if \code{dates} contains more than one date).
#' Extent, resolution and land-sea mask match those of \code{b}.
#' Values are the subset of \code{b} related to the given \code{dates}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Generate dummy RasterBrick
#'   set.seed(0)
#'   r <- raster(nrows = 2, ncols = 2,
#'               xmn = 0, xmx = 360, ymn = -90, ymx = 90, vals = 30)
#'   b <- raster::brick(lapply(1:(365 * 3),
#'        function(i) raster::setValues(r, runif(n = raster::ncell(r),
#'                                               min = 0, max = 100))))
#'   names(b) <- seq.Date(from = as.Date("1993-01-01"),
#'                        to = as.Date("1995-12-31"),
#'                        by = "day")
#'   daily_clima(b, as.Date("1996-01-01"))
#' }
#'

daily_clima <- function(b, dates = NULL){

  if (is.null(dates)) {
    # By default DO NOT use a leap year
    dates <- seq.Date(from = as.Date("2001-01-01"),
                      to = as.Date("2001-12-31"),
                      by = "day")
  }

  if (!lubridate::is.Date(dates)) dates <- as.Date(dates)

  # Initialise empty list of stacks
  clima_maps <- list()

  # Assemble daily climatology over all the years in the brick
  for (i in seq_along(dates)){
    
    message(paste("Day", i, "=", format(dates[[i]], "%B %d")))
    
    # Extract brick of layers corresponding to a given date
    clima_sub <- .get_layers_for_clima(b = b,
                                       raster_date = dates[[i]])
    
    clima_maps[[i]] <- clima_sub
    
  }
  names(clima_maps) <- dates

  if (length(dates) == 1){
    clima_maps <- clima_maps[[1]]
    if (raster::nlayers(clima_maps) == 1){
      clima_maps <- clima_maps[[1]]
    }
  }

  return(clima_maps)

}
