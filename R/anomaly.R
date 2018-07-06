#' @title anomaly
#'
#' @description This function calculates the anomaly (standardised deviation from the mean climatology) of a forecast layer
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
#'   x <- anomaly(fc, clima)
#' }
#'

anomaly <- function(fc, clima){

  # Get the forecast date
  forecast_date <- substr(x = names(fc), start = 7, stop = nchar(names(fc)))

  # Get the climatology dates
  clima_dates <- substr(x = names(clima), start = 7, stop = 11)
  # Get indices of forecast date
  clima_idx <- which(clima_dates == forecast_date)

  # Mean from clima
  meanClima <- raster::calc(x = clima[[clima_idx]],
                            fun = mean, progress = "text")
  meanClima <- raster::resample(x = meanClima, y = fc)
  # Standard deviation from clima
  sdClima <- raster::calc(x = clima[[clima_idx]], fun = sd, progress = "text")
  sdClima <- raster::resample(x = sdClima, y = fc)

  # Generate anomaly map
  anomalyMap <- (fc - meanClima) / sdClima

  a1 <- fc <= -3
  a2 <- (fc > -3 & fc <= -2)
  a3 <- (fc > -2 & fc <= -1.5)
  a4 <- (fc > -1.5 & fc <= -1)
  a5 <- (fc > -1 & fc <= -0.5)
  a6 <- (fc > -0.5 & fc <= +0.5)
  a7 <- (fc > +0.5 & fc <= +1)
  a8 <- (fc > +1 & fc <= +1.5)
  a9 <- (fc > +1.5 & fc <= +2)
  a10 <- (fc > +2 & fc <= +2.5)
  a11 <- (fc > +2.5 & fc <= +3)
  a12 <- fc > 3

  anomalyMap <- fc
  anomalyMap[] <- 0
  anomalyMap[a1] <- 1
  anomalyMap[a2] <- 2
  anomalyMap[a3] <- 3
  anomalyMap[a4] <- 4
  anomalyMap[a5] <- 5
  anomalyMap[a6] <- 6
  anomalyMap[a7] <- 7
  anomalyMap[a8] <- 8
  anomalyMap[a9] <- 9
  anomalyMap[a10] <- 10
  anomalyMap[a11] <- 11
  anomalyMap[a12] <- 12

  return(anomalyMap)

}

plot_anomaly <- function(r){

  # Define a background map
  background_map <- rworldmap::getMap(resolution = "low")
  # We want to plot the background map on each layers of the stack, so we need
  # to create a function and pass it to the addfun argument
  # (see ?plot in the raster package)
  fun <- function() {

    plot(background_map, add = TRUE, border = "lightgray")

  }

  breaks <- 1:13

  # Define palette: colorRamps::matlab.like(n = length(breaks))
  heatcolors <- c("#000099", "#0040CC", "#0080FF", "#00BFFF", "#33FFFF",
                  "#66FFCC", "#99FF99", "#CCFF66", "#FFFF33", "#FFBF00",
                  "#FF8000", "#CC4000", "#990000")

  # to place the legend outside the map
  par(xpd = FALSE)
  raster::plot(r, addfun = fun, col = heatcolors, breaks = breaks,
                    legend = FALSE)
  par(xpd = TRUE)
  legend(x = 182, y = 25, legend = c("<=-3.0", "-3.0..-2.0", "-2.0..-1.5",
                                     "-1.5..-1.0", "-1.0..-0.5",
                                     "-0.5..0.5", "0.5..1.0", "1.0..1.5",
                                     "1.5..2.0", "2.0..2.5", "2.5..3.0",
                                     ">3.0"),
         fill = heatcolors, bty="n")

}
