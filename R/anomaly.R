#' @title anomaly
#'
#' @description This function calculates the anomaly (number of standard
#' deviations from the mean climatology) of a forecast layer.
#'
#' @param r is the RasterLayer to compare to the climatology.
#' @param b RasterBrick/Stack containing the historical observations or a proxy
#' (typically a reanalysis) that is used to derive the climatological
#' information.
#'
#' @details The objects \code{r} and \code{b} should be comparable: same
#' resolution and extent.
#' More information on anomaly is available here:
#' https://bit.ly/2Qvekz4. To estimate fire climatology one can use hindcast or
#' reanalysis data. Examples of the latter are available from Zenodo:
#' https://zenodo.org/communities/wildfire.
#' 
#' @return The function returns a RasterLayer with extent, resolution and
#' land-sea mask matching those of \code{r}. Values are the number standard
#' deviations from the historical mean values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Generate dummy RasterLayer
#'   r <- raster(nrows = 1, ncols = 1,
#'               xmn = 0, xmx = 360, ymn = -90, ymx = 90, vals = 0.3)
#'   names(r) <- as.Date("2018-01-01")
#'   # Generate dummy RasterBrick
#'   b <- raster::brick(lapply(1:(365 * 3),
#'                   function(i) raster::setValues(r, runif(raster::ncell(r)))))
#'   names(b) <- seq.Date(from = as.Date("1993-01-01"),
#'                        to = as.Date("1995-12-31"),
#'                        by = "day")
#'   # Compute anomaly
#'   x <- anomaly(r, b)
#'
#'   # This plots nicely using rasterVis::levelplot(), see example on GWIS
#'   # (\url{https://gwis.jrc.ec.europa.eu}
#'   rasterVis::levelplot(x, col.regions = colorRamps::matlab.like(n = 11))
#' }
#'

anomaly <- function(r, b){

  if (raster::compareRaster(r, b) == FALSE){
    stop("r and b are not comparable!")
  }

  # Get date from RasterLayer
  raster_date <- as.Date(gsub(pattern = "\\.", replacement = "-",
                              x = substr(x = names(r), start = 2, stop = 11)))

  # Extract layers corresponding to a given date
  r_sub <- .get_layers_for_clima(b = b, raster_date = raster_date)

  # Mean from b
  mean_clima <- raster::calc(x = r_sub, fun = mean)
  # Standard deviation from b
  sd_clima <- raster::calc(x = r_sub, fun = sd)

  # Generate anomaly map
  anomaly_map <- (r - mean_clima) / sd_clima

  # Categorize the anomaly
  anomaly_map_cat <- raster::cut(anomaly_map,
                                 breaks = c(-Inf, -3, -2, -1.5, -1, -0.5,
                                            0.5, 1, 1.5, 2, 3, Inf))

  # Associate a Raster Attribute Table (RAT)
  anomaly_map_cat <- raster::ratify(anomaly_map_cat)

  # Define a Raster Attribute Table (RAT)
  rat <- levels(anomaly_map_cat)[[1]]
  ids <- 1:11
  classes <- c("<=-3.0", "-3.0..-2.0", "-2.0..-1.5",
               "-1.5..-1.0", "-1.0..-0.5", "-0.5..0.5",
               "0.5..1.0", "1.0..1.5", "1.5..2.0",
               "2.0..3.0", ">3.0")
  if (is.null(rat)) {
    classes_2_use <- sort(unique(raster::getValues(anomaly_map_cat)))
    # Define a Raster Attribute Table (RAT)
    rat <- .create_rat(ids = ids, classes = classes)[classes_2_use, ]
  } else {
    classes_to_use <- unlist(levels(anomaly_map_cat))
    rat$Class = classes[classes_to_use]
  }
  levels(anomaly_map_cat) <- rat

  return(anomaly_map_cat)

}
