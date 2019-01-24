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
#' \code{r} but the values are categorical from 1 to 6.
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

  fwi_class <- r
  # Categorize the Raster* object
  a1 <- r <= thresholds[1]                       # Very low danger
  a2 <- (r > thresholds[1] & r <= thresholds[2]) # Low danger
  a3 <- (r > thresholds[2] & r <= thresholds[3]) # Moderate danger
  a4 <- (r > thresholds[3] & r <= thresholds[4]) # High danger
  a5 <- (r > thresholds[4] & r <= thresholds[5]) # Very high danger
  a6 <- r > thresholds[5]                        # Extreme danger

  fwi_class[] <- NA
  fwi_class[a1] <- 1
  fwi_class[a2] <- 2
  fwi_class[a3] <- 3
  fwi_class[a4] <- 4
  fwi_class[a5] <- 5
  fwi_class[a6] <- 6

  return(fwi_class)

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

  breaks <- 1:6

  if (is.null(custom_palette)){
    # Define palette
    custom_palette <- rev(viridis::viridis(n = length(breaks)))
  }

  # place the legend beside each map
  raster::plot(r,
               addfun = .background_map_fun,
               col = custom_palette,
               breaks = breaks,
               legend = c("Very low", "Low", "Moderate",
                          "High", "Very high", "Extreme"),
               ...)

}
