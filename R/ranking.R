#' @title ranking
#'
#' @description The ranking is applied to a forecast map \code{r} and provides
#' percentiles of occurrence of the values based on a given climatology
#' (see \code{clima}).
#'
#' @param r is the RasterLayer to compare to the climatology.
#' @param clima RasterBrick containing the climatological information.
#'
#' @details More information on ranking is available here:
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
#'   x <- ranking(r, clima)
#'   
#'   # This plots nicely using rasterVis::levelplot(), in example on GWIS 
#'   # (\url{https://gwis.jrc.ec.europa.eu}
#'   rasterVis::levelplot(x, col.regions = colorRamps::matlab.like(n = 6))
#'   rasterVis::levelplot(x, col.regions = c("green", "yellow", "salmon",
#'                                           "orange", "red", "black"))
#' }
#'

ranking <- function(r, clima){

  if (!raster::compareRaster(r, clima)){
    stop(paste("r and clima are not comparable.",
               "Please make sure they have same extent, number of rows and",
               "columns, projection, resolution, and origin."))
  }

  # Get the forecast dates
  forecast_date <- substr(x = names(r), start = 2, stop = 11)

  # Extract layers corresponding to a given date
  r_sub <- get_layers_for_clima(clima, forecast_date)

  # Compute reference percentile maps
  prob_maps <- raster::calc(x = r_sub, fun = .quant_function)
  # Resample to match forecast layer
  prob_maps <- raster::resample(prob_maps, r, progress = "text")

  pbelow75 <- r <= prob_maps[[1]]
  p75to85 <- (r > prob_maps[[1]] & r <= prob_maps[[2]])
  p85to90 <- (r > prob_maps[[2]] & r <= prob_maps[[3]])
  p90to95 <- (r > prob_maps[[3]] & r <= prob_maps[[4]])
  p95to98 <- (r > prob_maps[[4]] & r <= prob_maps[[5]])
  pabove98 <- r > prob_maps[[5]]

  ranking_map <- r
  ranking_map[] <- NA
  ranking_map[pbelow75] <- 1
  ranking_map[p75to85] <- 2
  ranking_map[p85to90] <- 3
  ranking_map[p90to95] <- 4
  ranking_map[p95to98] <- 5
  ranking_map[pabove98] <- 6

  # Associate a Raster Attribute Table (RAT)
  ranking_map <- raster::ratify(ranking_map)

  # Define a Raster Attribute Table (RAT)
  rat <- .create_rat(ids = 1:6,
                     classes = c("<=75", "75..85", "85..90",
                                 "90..95", "95..98", "98..100"))
  levels(ranking_map) <- rat

  return(ranking_map)

}
