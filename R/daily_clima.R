#' @title daily_clima
#'
#' @description This function generates daily climatological maps.
#'
#' @param r RasterBrick or RasterStack object used to calculate the
#' climatology. This needs to contain daily layers for multiple years.
#' \code{names(r)} should also contain dates for comparison
#' (e.g. X2017.01.01).
#' @param dates Dates for which we need to calculate daily climatology.
#' By default, this is a leap year.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   daily_clima(r, dates)
#' }
#'

daily_clima <- function(r, dates = NULL){

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
    clima_sub <- .get_layers_for_clima(clima = r,
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
