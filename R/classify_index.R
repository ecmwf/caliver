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
#' }
#'

classify_index <- function(r, thresholds = NULL){

  if (is.null(thresholds)){
    thresholds <- c(5.2, 11.2, 21.3, 38, 50)
  }

  index_class <- raster::cut(r, breaks = c(-Inf, thresholds, Inf))

  # Convert to Stack and associate a Raster Attribute Table (RAT)
  index_stack <- stack_with_rat(r = index_class,
                                ids = 1:6,
                                classes = c("Very low", "Low", "Moderate",
                                            "High", "Very high", "Extreme"))

  return(index_stack)

}

#' @title plot_classified_index
#'
#' @description Plot classified index as shown in GWIS:
#' \url{https://bit.ly/2BbBfsm}
#'
#' @param r is the Raster* object already classified.
#' @param custom_palette palette to use (default is \code{viridis::plasma})
#' @param ... other plotting arguments, see \code{?raster::plot} function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   r <- brick("cfwis_ffwi_20170101_1200_00.nc")[[1]]
#'   clima <- brick("fwi.nc")
#'   anomaly_map <- anomaly(r, clima)
#'   plot_anomaly(anomaly_map,
#'                custom_palette = colorRamps::matlab.like(n = length(breaks)))
#' }
#'

plot_classified_index <- function(r, custom_palette = NULL, ...){

  if (is.null(custom_palette)){
    # Define palette
    custom_palette <- rev(viridis::viridis(n = length(levels(r)[[1]][, "ID"])))
  }

  rasterVis::levelplot(r, att = "Danger", col.regions = custom_palette)

}
