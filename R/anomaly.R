#' @title anomaly
#'
#' @description This function calculates the anomaly (standardised deviation
#' from the mean climatology) of a forecast layer
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
  mean_clima <- raster::calc(x = clima[[clima_idx]],
                            fun = mean, progress = "text")
  mean_clima <- raster::resample(x = mean_clima, y = fc)
  # Standard deviation from clima
  sd_clima <- raster::calc(x = clima[[clima_idx]], fun = sd, progress = "text")
  sd_clima <- raster::resample(x = sd_clima, y = fc)

  # Generate anomaly map
  anomaly_map <- (fc - mean_clima) / sd_clima

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

  anomaly_map <- fc
  anomaly_map[] <- 0
  anomaly_map[a1] <- 1
  anomaly_map[a2] <- 2
  anomaly_map[a3] <- 3
  anomaly_map[a4] <- 4
  anomaly_map[a5] <- 5
  anomaly_map[a6] <- 6
  anomaly_map[a7] <- 7
  anomaly_map[a8] <- 8
  anomaly_map[a9] <- 9
  anomaly_map[a10] <- 10
  anomaly_map[a11] <- 11
  anomaly_map[a12] <- 12

  return(anomaly_map)

}

plot_anomaly <- function(r){

  breaks <- 1:13

  # Define palette: colorRamps::matlab.like(n = length(breaks))
  heatcolors <- c("#000099", "#0040CC", "#0080FF", "#00BFFF", "#33FFFF",
                  "#66FFCC", "#99FF99", "#CCFF66", "#FFFF33", "#FFBF00",
                  "#FF8000", "#CC4000", "#990000")

  # to place the legend outside the map
  par(xpd = FALSE)
  raster::plot(r, addfun = background_map_fun, col = heatcolors,
               breaks = breaks, legend = FALSE)
  par(xpd = TRUE)
  legend(x = 182, y = 25, legend = c("<=-3.0", "-3.0..-2.0", "-2.0..-1.5",
                                     "-1.5..-1.0", "-1.0..-0.5",
                                     "-0.5..0.5", "0.5..1.0", "1.0..1.5",
                                     "1.5..2.0", "2.0..2.5", "2.5..3.0",
                                     ">3.0"),
         fill = heatcolors, bty = "n")

}
