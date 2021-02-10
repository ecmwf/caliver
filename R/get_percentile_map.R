#' @title get_percentile_map
#'
#' @description This function calculates percentile(s) at each grid point. Wrappers raster::calc.
#'
#' @param r Raster* object (either RasterStack or RasterBrick). This could be the
#' full record of daily indices or daily climatology.
#' @param probs numeric vector of probabilities with values in the range [0, 1]
#' listing which percentiles should be calculated.
#'
#' @return list containing all the generated percentile maps
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   x <- get_percentile_map(r, probs = c(0.50, 0.75, 0.90, 0.99))
#' }
#'

get_percentile_map <- function(r, probs){

  if (!all(probs < 1)) {
    stop("Please make sure probs are in the range [0, 1]")
  }
  
  quant_fun <- function(x) quantile(x, probs = probs, na.rm = TRUE)
  
  if (class(r) == "list"){
    
    # Using climatological maps
    dates <- as.Date(names(r))
    perc_maps <- list()
    
    for (i in seq_along(dates)){
      
      r_day <- r[[i]]
      
      temp <- raster::calc(r_day, fun = quant_fun, probs = probs, progress = "text")
      
      perc_maps[[i]] <- temp
      
    }
    names(perc_maps) <- dates
    
    if (length(dates) == 1){
      perc_maps <- perc_maps[[1]]
      if (raster::nlayers(perc_maps) == 1){
        perc_maps <- perc_maps[[1]]
      }
    }

  } else {

    # Using full record
    perc_maps <- raster::calc(r, fun = quant_fun, probs = probs, progress = "text")

  }

  return(perc_maps)

}
