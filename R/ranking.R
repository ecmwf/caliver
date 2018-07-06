#' @title ranking
#'
#' @description This function calculates the percentile ranking of a forecast
#' layer
#'
#' @param fc is the forecast layer, a Raster* object.
#' @param clima RasterBrick containing the climatological information
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   fc <- brick("cfwis_ffwi_20170101_1200_00.nc")[[1]]
#'   clima <- brick("fwi.nc")
#'   x <- ranking(fc, clima)
#' }
#'

ranking <- function(fc, clima){

  # Set up default layer for comparison
  prob_maps <- raster::calc(x = clima, fun = quant_function, progress = "text")

  prob_maps <- raster::resample(prob_maps, fc, progress = "text")

  pbelow50 <- fc <= prob_maps[[1]]
  p50to75 <- (fc > prob_maps[[1]] & fc <= prob_maps[[2]])
  p75to85 <- (fc > prob_maps[[2]] & fc <= prob_maps[[3]])
  p85to90 <- (fc > prob_maps[[3]] & fc <= prob_maps[[4]])
  p90to95 <- (fc > prob_maps[[4]] & fc <= prob_maps[[5]])
  p95to98 <- (fc > prob_maps[[5]] & fc <= prob_maps[[6]])
  pabove98 <- fc > prob_maps[[6]]

  ranking_map <- fc
  ranking_map[] <- 0
  ranking_map[pbelow50] <- 1
  ranking_map[p50to75] <- 2
  ranking_map[p75to85] <- 3
  ranking_map[p85to90] <- 4
  ranking_map[p90to95] <- 5
  ranking_map[p95to98] <- 6
  ranking_map[pabove98] <- 7

  return(ranking_map)

}

plot_ranking <- function(r){

  breaks <- 1:7

  # Define palette
  heatcolors <- c("beige", "orange1", "orange2", "orange3", "orangered", "red",
                  "brown")

  # to place the legend outside the map
  par(xpd = FALSE)
  raster::plot(r, addfun = background_map_fun, col = heatcolors,
               breaks = breaks, legend = FALSE)
  par(xpd = TRUE)
  legend(x = 182, y = 25,
         legend = c("<=50", "50..75", "75..85",
                    "85..90", "90..95", "95..98", "98..100"),
         fill = heatcolors, bty = "n")

}
