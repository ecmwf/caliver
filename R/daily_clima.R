#' @title daily_clima
#'
#' @description This function generates daily climatological maps.
#'
#' @param r RasterBrick or RasterStack object used to calculate the climatology
#' \code{names(r)} should also contain dates for comparison (e.g. X2017.01.01).
#' @param dates Dates for which we need to calculate daily climatology.
#' By default, this is a leap year.
#' @param probs probability (or percentile).
#' This is a decimal number between 0 and 1.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   daily_clima(r, dates, probs)
#' }
#'

daily_clima <- function(r, dates = NULL, probs){

  if (is.null(dates)) {
    # By default use a leap year
    dates <- seq.Date(from = as.Date("2000-01-01"),
                      to = as.Date("2000-12-31"),
                      by = "day")
  }

  # Initialise empty list of stacks
  clima_maps <- list()
  for (i in seq_along(probs)){

    message(paste("Stacking layers for prob =", probs[i]))

    # For each prob create a stack
    clima_maps[[i]] <- raster::stack()

    # Assemble daily climatology over 37 years
    for (j in seq_along(dates)){

      message(paste("Day =", j))

      # which indices correspond to day j?
      idx <- which(substr(names(r), 7, 11) ==
                     gsub("-", ".", substr(as.character(dates[j]), 6, 10)))

      # Do not take the single day but the period spanning 4 days before and
      # 4 days after the given date
      idx_vector <- c()
      for (k in seq_along(idx)){
        idx_vector <- c(idx_vector, (idx[k] - 4):(idx[k] + 4))
      }
      if (any(idx_vector <= 0)){
        elements2remove <- which(idx_vector <= 0)
        idx_vector <- idx_vector[-elements2remove]
      }
      if (any(idx_vector > raster::nlayers(r))){
        elements2remove <- which(idx_vector > raster::nlayers(r))
        idx_vector <- idx_vector[-elements2remove]
      }

      if (length(idx_vector) < length(idx) * 9){
        message(paste0("Caution: climatology for the ", dates[j],
                       " is calculated using ", length(idx_vector),
                       " days rather then ", length(idx) * 9, "!"))
      }

      # Collection of layers spanning the date of interest +/- 4 days & 37 years
      r_sub <- r[[idx_vector]]

      temp_map <- raster::calc(x = r_sub,
                               fun = function(x){
                                 raster::quantile(x, probs[i], na.rm = TRUE)
                                 })
      names(temp_map) <- paste0("X", dates[j], "_", probs[i])
      clima_maps[[i]] <- raster::stack(clima_maps[[i]], temp_map)

      rm(j, k, idx, idx_vector, r_sub)

    }

  }
  names(clima_maps) <- probs

  if (length(probs) == 1){
    clima_maps <- clima_maps[[1]]
  }

  return(clima_maps)

}
