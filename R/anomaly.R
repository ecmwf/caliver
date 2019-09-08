#' @title anomaly
#'
#' @description This function calculates the anomaly (number of standard
#' deviations from the mean climatology) of a forecast layer.
#'
#' @param r is the RasterLayer to compare to the climatology.
#' @param clima RasterBrick containing the climatological information.
#'
#' @details \code{r} and \code{clima} should have the same extent.
#' More information on anomaly is available here:
#' https://bit.ly/2Qvekz4. To estimate fire climatology one can use hindcast or
#' reanalysis data. Examples of the latter are available from Zenodo:
#' https://zenodo.org/communities/wildfire.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   r <- brick("cfwis_ffwi_20170101_1200_00.nc")[[1]]
#'   clima <- brick("fwi.nc")
#'   x <- anomaly(r, clima)
#'   
#'   # This plots nicely using rasterVis::levelplot(), in example on GWIS 
#'   # (\url{https://gwis.jrc.ec.europa.eu}
#'   rasterVis::levelplot(x, col.regions = colorRamps::matlab.like(n = 11))
#' }
#'

anomaly <- function(r, clima){

  if (!raster::compareRaster(r, clima)){
    stop(paste("r and clima are not comparable.",
               "Please make sure they have same extent, number of rows and",
               "columns, projection, resolution, and origin."))
  }

  # Get the forecast dates
  forecast_date <- substr(x = names(r), start = 2, stop = 11)

  # Extract layers corresponding to a given date
  r_sub <- get_layers_for_clima(clima, forecast_date)

  # Mean from clima
  mean_clima <- raster::calc(x = r_sub, fun = mean)
  mean_clima <- raster::resample(x = mean_clima, y = r)
  # Standard deviation from clima
  sd_clima <- raster::calc(x = r_sub, fun = sd)
  sd_clima <- raster::resample(x = sd_clima, y = r)
  
  # Generate anomaly map
  anomaly_map <- (r - mean_clima) / sd_clima
  
  # Categorize the anomaly
  anomaly_map_cat <- raster::cut(anomaly_map,
                                 breaks = c(-Inf, -3, -2, -1.5, -1, -0.5,
                                            0.5, 1, 1.5, 2, 3, Inf))
  
  # Associate a Raster Attribute Table (RAT)
  anomaly_map_cat <- raster::ratify(anomaly_map_cat)
  
  # Define a Raster Attribute Table (RAT)
  rat <- .create_rat(ids = 1:11,
                     classes = c("<=-3.0", "-3.0..-2.0", "-2.0..-1.5",
                                "-1.5..-1.0", "-1.0..-0.5", "-0.5..0.5",
                                "0.5..1.0", "1.0..1.5", "1.5..2.0",
                                "2.0..3.0", ">3.0"))
  levels(anomaly_map_cat) <- rat

  return(anomaly_map_cat)

}
