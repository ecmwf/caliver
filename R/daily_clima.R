#' @title daily_clima
#'
#' @description This function generates daily climatological maps.
#'
#' @param clima RasterBrick or RasterStack object used to calculate the
#' climatology. This needs to contain daily layers for multiple years.
#' \code{names(clima)} should also contain dates for comparison
#' (e.g. X2017.01.01).
#' @param dates Dates for which we need to calculate daily climatology.
#' By default, this is a leap year.
#' @param probs probability (or percentile).
#' This is a number between 0 and 1.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   daily_clima(clima, dates, probs)
#' }
#'

daily_clima <- function(clima, dates = NULL, probs){

  if (is.null(dates)) {
    # By default DO NOT use a leap year
    dates <- seq.Date(from = as.Date("2001-01-01"),
                      to = as.Date("2001-12-31"),
                      by = "day")
  }

  if (!lubridate::is.Date(dates)) dates <- as.Date(dates)

  # Initialise empty list of stacks
  clima_maps <- list()

  for (i in seq_along(probs)){

    message(paste("Stacking layers for prob =", probs[i]))

    # For each prob create a stack
    clima_maps[[i]] <- raster::stack()

    # Assemble daily climatology over all the years in the brick
    for (j in seq_along(dates)){

      message(paste("Day", j, "=", format(dates[[j]], "%B %d")))

      # Extract layers corresponding to a given date
      clima_sub <- .get_layers_for_clima(clima = clima,
                                         raster_date = dates[[j]])

      temp_map <- raster::calc(x = clima_sub,
                               fun = function(x){
                                 raster::quantile(x, probs[i], na.rm = TRUE)
                                 })
      names(temp_map) <- paste0("X", dates[j], "_", probs[i])
      clima_maps[[i]] <- raster::stack(clima_maps[[i]], temp_map)

    }

  }
  names(clima_maps) <- probs

  if (length(probs) == 1){
    clima_maps <- clima_maps[[1]]
    if (raster::nlayers(clima_maps) == 1){
      clima_maps <- clima_maps[[1]]
    }
  }

  return(clima_maps)

}
