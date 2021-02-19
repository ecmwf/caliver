#' @title ranking
#'
#' @description The ranking is applied to a forecast map \code{r} and provides
#' percentiles of occurrence of the values based on a given climatology
#' (see \code{b}).
#'
#' @param r is the RasterLayer to compare to the climatology.
#' @param b RasterBrick/Stack containing the historical observations or a proxy
#' (typically a reanalysis) that is used to derive the climatological
#' information.
#'
#' @details The objects \code{r} and \code{b} should be comparable: same
#' resolution and extent.
#' More information on ranking is available here:
#' https://bit.ly/2Qvekz4. To estimate fire climatology one can use hindcast or
#' reanalysis data. Examples of the latter are available from Zenodo:
#' https://zenodo.org/communities/wildfire.
#' 
#' @return The function returns a RasterLayer with extent, resolution and
#' land-sea mask matching those of \code{r}. Values are the percentiles of
#' occurrence of the values.
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
#'   # Compute ranking
#'   x <- ranking(r, b)
#'
#'   # This plots nicely using rasterVis::levelplot(), see example on GWIS
#'   # (\url{https://gwis.jrc.ec.europa.eu}
#'   rasterVis::levelplot(x, col.regions = c("green", "yellow", "salmon",
#'                                           "orange", "red", "black"))
#' }
#'

ranking <- function(r, b){

  if (raster::compareRaster(r, b) == FALSE){
    stop("r and b are not comparable!")
  }

  # Get date from RasterLayer
  raster_date <- as.Date(gsub(pattern = "\\.", replacement = "-",
                              x = substr(x = names(r), start = 2, stop = 11)))

  # Extract layers corresponding to a given date
  r_sub <- .get_layers_for_clima(b = b, raster_date = raster_date)

  # Compute reference percentile maps
  prob_maps <- raster::calc(x = r_sub, fun = .quant_function)

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
  rat <- levels(ranking_map)[[1]]
  ids <- 1:6
  classes <- c("<=75", "75..85", "85..90",
               "90..95", "95..98", "98..100")
  if (is.null(rat)) {
    classes_2_use <- sort(unique(raster::getValues(ranking_map)))
    # Define a Raster Attribute Table (RAT)
    rat <- .create_rat(ids = ids, classes = classes)[classes_2_use, ]
  } else {
    classes_to_use <- unlist(levels(ranking_map))
    rat$Class = classes[classes_to_use]
  }
  levels(ranking_map) <- rat

  return(ranking_map)

}
