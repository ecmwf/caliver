#' @title ranking
#'
#' @description The ranking is applied to a forecast map \code{r} and provides
#' percentiles of occurrence of the values based on a given climatology
#' (see \code{clima}).
#'
#' @param r is the Raster layer to compare to the climatology.
#' @param clima RasterBrick containing the climatological information.
#'
#' @details More information on ranking is available here:
#' https://bit.ly/2Qvekz4. You can find fire climatology data for the FWI
#' indices in Zenodo: https://zenodo.org/communities/wildfire.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   r <- brick("cfwis_ffwi_20170101_1200_00.nc")[[1]]
#'   clima <- brick("fwi.nc")
#'   x <- ranking(r, clima)
#' }
#'

ranking <- function(r, clima){

  if (round(r@extent@xmin, 0) == 0) {

    # Rotate a Raster* object that has x coordinates (longitude) from 0 to 360,
    # to standard coordinates between -180 and 180 degrees.
    # Longitude between 0 and 360 is frequently used in data
    # from global climate models.
    r <- raster::rotate(r)

  }

  if (round(clima@extent@xmin, 0) == 0) {

    # Rotate a Raster* object that has x coordinates (longitude) from 0 to 360,
    # to standard coordinates between -180 and 180 degrees.
    # Longitude between 0 and 360 is frequently used in data
    # from global climate models.
    clima <- raster::rotate(clima)

  }

  # Set up default layer for comparison
  prob_maps <- raster::calc(x = clima, fun = .quant_function, progress = "text")

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

  return(ranking_map)

}

#' @title plot_ranking
#'
#' @description Plot ranking map as shown in GWIS (\url{https://bit.ly/2BbBfsm})
#'
#' @param ranking_map is the Raster layer, result of \code{ranking()}.
#' @param custom_palette palette to use (default is \code{viridis::viridis})
#' @param ... other plotting arguments, see \code{?raster::plot} function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   r <- brick("cfwis_ffwi_20170101_1200_00.nc")[[1]]
#'   clima <- brick("fwi.nc")
#'   ranking_map <- ranking(r, clima)
#'   # As shown in GWIS
#'   plot_ranking(ranking_map,
#'                custom_palette = c("green", "yellow", "salmon", "orange",
#'                                   "red", "black"))
#' }
#'

plot_ranking <- function(ranking_map, custom_palette = NULL, ...){

  breaks <- 1:6

  if (is.null(custom_palette)){
    # Define palette
    custom_palette <- rev(viridis::viridis(n = length(breaks)))
  }

  if (raster::nlayers(ranking_map) == 1){
    # place the legend outside the map
    par(xpd = FALSE)
    raster::plot(ranking_map,
                 addfun = .background_map_fun,
                 col = custom_palette,
                 breaks = breaks,
                 legend = FALSE,
                 ...)
    par(xpd = TRUE)
    legend(x = round(par("usr")[2] + (par("usr")[2] - par("usr")[1]) / 90, 0),
           y = round(mean(c(par("usr")[3], par("usr")[4])), 0),
           legend = c("<=75", "75..85", "85..90",
                      "90..95", "95..98", "98..100"),
           fill = custom_palette, bty = "n")
  }else{
    # place the legend beside each map
    raster::plot(ranking_map,
                 addfun = .background_map_fun,
                 col = custom_palette,
                 breaks = breaks,
                 lab.breaks = c("<=75", "75..85", "85..90",
                                "90..95", "95..98", "98..100"),
                 ...)
  }

}
