#' @title ranking
#'
#' @description This function calculates the percentile ranking of a forecast layer
#'
#' @param fc is the forecast layer, a Raster* object.
#' @param clima RasterBrick containing the climatological information
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   fc <- brick("/hugetmp/fire/geff/forecast/fwi2017/ffwi_rotated/cfwis_ffwi_20170101_1200_00.nc")[[1]]
#'   clima <- brick("/hugetmp/fire/geff/reanalysis/fwi.nc")
#'   x <- ranking(fc, clima)
#' }
#'

ranking <- function(fc, clima){

  # Default probs
  probs <- c(0.50, 0.75, 0.85, 0.90, 0.95, 0.98)

  # Set up default layer for comparison
  pMaps <- calc(x = clima,
                fun = function(x) {quantile(x,
                                            probs = probs,
                                            na.rm=TRUE)} ,
                progress = "text")

  pMaps <- resample(pMaps, fc, progress = "text")

  pbelow50 <- fc <= pMaps[[1]]
  p50to75 <- (fc > pMaps[[1]] & fc <= pMaps[[2]])
  p75to85 <- (fc > pMaps[[2]] & fc <= pMaps[[3]])
  p85to90 <- (fc > pMaps[[3]] & fc <= pMaps[[4]])
  p90to95 <- (fc > pMaps[[4]] & fc <= pMaps[[5]])
  p95to98 <- (fc > pMaps[[5]] & fc <= pMaps[[6]])
  pabove98 <- fc > pMaps[[6]]

  rankingMap <- fc
  rankingMap[] <- 0
  rankingMap[pbelow50] <- 1
  rankingMap[p50to75] <- 2
  rankingMap[p75to85] <- 3
  rankingMap[p85to90] <- 4
  rankingMap[p90to95] <- 5
  rankingMap[p95to98] <- 6
  rankingMap[pabove98] <- 7

  return(rankingMap)

}

plot_ranking <- function(r){

  # Define a background map
  background_map <- rworldmap::getMap(resolution = "low")
  # We want to plot the background map on each layers of the stack, so we need
  # to create a function and pass it to the addfun argument
  # (see ?plot in the raster package)
  fun <- function() {

    plot(background_map, add = TRUE, border = "lightgray")

  }

  breaks <- 1:7

  # Define palette
  heatcolors <- c("beige", "orange1", "orange2", "orange3", "orangered", "red", "brown")
    # c("beige", "lightblue", "green", "yellow", "red", "purple")
    # rev(grDevices::heat.colors(n = length(breaks)))

  # to place the legend outside the map
  par(xpd = FALSE)
  raster::plot(r, addfun = fun, col = heatcolors, breaks = breaks,
                    legend = FALSE)
  par(xpd = TRUE)
  legend(x = 182, y = 25, legend = c("<=50", "50..75", "75..85",
                               "85..90", "90..95", "95..98", "98..100"),
         fill = heatcolors, bty="n")

}
