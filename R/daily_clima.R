#' @title daily_clima
#'
#' @description This function generates daily climatological maps.
#'
#' @param r RasterBrick or RasterStack object used to calculate the climatology
#' @param dates Dates for which we need to calculate daily climatology.
#' \code{names(r)} should also contain dates for comparison (e.g. X2017.01.01).
#' @param probs probability (or percentile)
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   daily_clima(r, dates, probs)
#' }
#'

daily_clima <- function(r, dates, probs){

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
                     gsub("-", ".", substr(as.character(dates[j]),6,10)))

      # Do not take the single day but the period spanning 4 days before and after
      idxVector <- c()
      for (k in seq_along(idx)){
        idxVector <- c(idxVector, (idx[k] - 4):(idx[k] + 4))
      }
      if (any(idxVector <= 0)){
        elements2remove <- which(idxVector <= 0)
        idxVector <- idxVector[-elements2remove]
      }
      if (any(idxVector > raster::nlayers(r))){
        elements2remove <- which(idxVector > raster::nlayers(r))
        idxVector <- idxVector[-elements2remove]
      }

      if(length(idxVector) < length(idx)*9){
        message(paste0("Caution: climatology for the ", dates[j],
                       " is calculated using ", length(idxVector),
                       " days rather then ", length(idx)*9, "!"))
      }

      # Collection of layers spanning the date of interest +/- 4 days & 37 years
      r_sub <- r[[idxVector]]

      temp_map <- raster::calc(x = r_sub,
                       fun = function(x){raster::quantile(x,
                                                          probs[i],
                                                          na.rm = TRUE)})
      clima_maps[[i]] <- raster::stack(clima_maps[[i]], temp_map)

      rm(j, k, idx, idxVector, r_sub)

    }

  }
  names(clima_maps) <- probs

  if (length(probs) == 1){
    clima_maps <- clima_maps[[1]]
  }

  return(clima_maps)

}
