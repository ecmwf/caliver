#' @title get_perc_risk_index
#'
#' @description Generates the mean of the values over a certain percentile
#' threshold for the pixel of the raster stack under the polygons of a shapefile
#'
#' @param r_stack is the raster or raster stack
#' @param p_shape is the shapefile on which to aggregate the values
#' @param perc_val is the percentile value used as a threshold
#' @param mod defines if the values considered for the mean are above (gt) or
#' below (lt) the threshold
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   r.index <- get_perc_risk_index(r_stack = r_stack, p_shape = poly,
#'                               perc_val = 50, mod = "lt")
#' }
#'

get_perc_risk_index <- function(r_stack, p_shape, perc_val = 75, mod = "gt"){

  r_vals <- raster::extract(r_stack, p_shape)

  index_val <- lapply(r_vals, FUN = mean_percs, perc_val = perc_val, mod = mod)

  return(index_val)

}
