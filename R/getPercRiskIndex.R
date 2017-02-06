#' @title Generates the percentile risk index from a raster stack and a set of polygons
#'
#' @description generates the mean of the values over a certain percentile theshold 
#'              for the pixel of the raster stack under the polygons of a shapefile
#'
#' @param r_stack is the raster or raster stack
#' @param p_shape is the shapefile on which to aggregate the values
#' @param perc.val is the percentile value used as a threshold
#' @param mod defines if the values considered for the mean are above (gt) or below (lt) the threshold
#'
#' @export 
#' 
#' @examples
#' \dontrun{
#'   r.index <- getPercRiskIndex(r_stack = r_stack, p_shape = poly, 
#'                               perc.val = 50, mod = "lt")
#' }
#'

getPercRiskIndex <- function(r_stack, p_shape, perc.val = 75, mod = "gt"){
  
	r.vals <- raster::extract(r_stack, p_shape)

	index.val <- lapply(r.vals, FUN = mean_percs, perc.val = perc.val, mod = mod)
	
	return(index.val)
	
}
