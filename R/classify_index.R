#' @title classify_index
#'
#' @description This function classifies a fire danger index based on user
#' defined danger thresholds. If no thresholds are specified, the function
#' assumes the index is FWI and thresholds are those defined by JRC for Europe:
#' very Low = 0-5.2, low = 5.2-11.2, moderate = 11.2-21.3, high = 21.3-38.0,
#' very high = 38-50.0, extreme = > 50.
#'
#' @param r is the Raster* object to classify.
#' @param thresholds numeric vector containing 5 thresholds.
#' @param labels string of characters to be used as labels
#'
#' @return The function returns a Raster* object of the same dimensions of
#' \code{r} but the values are categorical from 1 to 6 (corresponding to
#' very low, low, moderate, high, very high and extreme danger, respectively).
#' Some cells may contain NAs, this happens typically over the sea.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   r <- brick("ECMWF_FWI_20180723_1200_hr_fwi.nc")
#'   x <- classify_index(r, thresholds = c(5.2, 11.2, 21.3, 38, 50))
#'
#'   # This can be plotted using rasterVis::levelplot()
#'   rasterVis::levelplot(x)
#'   # Plot above plus custom labels
#'   rasterVis::levelplot(x, names.attr = substring(names(x), 2))
#'   # Plot above plus custom palette
#'   rasterVis::levelplot(x, names.attr = substring(names(x), 2),
#'                        col.regions = colorspace::diverge_hcl(6,
#'                        palette = "Berlin"))
#'   # Plot above but different custom palette
#'   rasterVis::levelplot(x, names.attr = substring(names(x), 2),
#'                        col.regions = colorRamps::matlab.like(n = 6))
#'
#' }
#'

classify_index <- function(r, thresholds = NULL, labels = NULL){

  if (is.null(thresholds) | is.null(labels)){
    thresholds <- c(5.2, 11.2, 21.3, 38, 50)
    labels <- c("Very low", "Low", "Moderate", "High", "Very high", "Extreme")
  }

  index_class <- raster::cut(r, breaks = c(-Inf, thresholds, Inf))

  # Convert to Stack and associate a Raster Attribute Table (RAT)
  index_stack <- stack_with_rat(r = index_class,
                                ids = 1:(length(thresholds) + 1),
                                classes = labels)

  return(index_stack)

}
