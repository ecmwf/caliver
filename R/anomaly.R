#' @title anomaly
#'
#' @description This function calculates the anomaly (standardised deviation
#' from the mean climatology) of a forecast layer
#'
#' @param r is the Raster layer to compare to the climatology.
#' @param clima RasterBrick containing the climatological information.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   r <- brick("cfwis_ffwi_20170101_1200_00.nc")[[1]]
#'   clima <- brick("fwi.nc")
#'   x <- anomaly(r, clima)
#' }
#'

anomaly <- function(r, clima){

  # Get the forecast date
  forecast_date <- substr(x = names(r), start = 7, stop = nchar(names(r)))

  # Get the climatology dates
  clima_dates <- substr(x = names(clima), start = 7, stop = 11)
  # Get indices of forecast date
  clima_idx <- which(clima_dates == forecast_date)

  # Mean from clima
  mean_clima <- raster::calc(x = clima[[clima_idx]],
                            fun = mean, progress = "text")
  mean_clima <- raster::resample(x = mean_clima, y = r)
  # Standard deviation from clima
  sd_clima <- raster::calc(x = clima[[clima_idx]], fun = sd, progress = "text")
  sd_clima <- raster::resample(x = sd_clima, y = r)

  # Generate anomaly map
  anomaly_map <- (r - mean_clima) / sd_clima

  a1 <- r <= -3
  a2 <- (r > -3 & r <= -2)
  a3 <- (r > -2 & r <= -1.5)
  a4 <- (r > -1.5 & r <= -1)
  a5 <- (r > -1 & r <= -0.5)
  a6 <- (r > -0.5 & r <= +0.5)
  a7 <- (r > +0.5 & r <= +1)
  a8 <- (r > +1 & r <= +1.5)
  a9 <- (r > +1.5 & r <= +2)
  a10 <- (r > +2 & r <= +2.5)
  a11 <- (r > +2.5 & r <= +3)
  a12 <- r > 3

  anomaly_map <- r
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

#' @title plot_anomaly
#'
#' @description Plot anomaly map as shown in GWIS (\url{https://bit.ly/2BbBfsm})
#'
#' @param anomaly_map is the Raster layer, result of \code{anomaly()}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   r <- brick("cfwis_ffwi_20170101_1200_00.nc")[[1]]
#'   clima <- brick("fwi.nc")
#'   anomaly_map <- anomaly(r, clima)
#'   plot_ranking(anomaly_map)
#' }
#'

plot_anomaly <- function(anomaly_map){

  breaks <- 1:13

  # Define palette: colorRamps::matlab.like(n = length(breaks))
  heatcolors <- c("#000099", "#0040CC", "#0080FF", "#00BFFF", "#33FFFF",
                  "#66FFCC", "#99FF99", "#CCFF66", "#FFFF33", "#FFBF00",
                  "#FF8000", "#CC4000", "#990000")

  # to place the legend outside the map
  par(xpd = FALSE)
  raster::plot(anomaly_map, addfun = background_map_fun, col = heatcolors,
               breaks = breaks, legend = FALSE)
  par(xpd = TRUE)
  legend(x = round(par("usr")[2] + (par("usr")[2] - par("usr")[1])/90, 0),
         y = round(mean(c(par("usr")[3], par("usr")[4])), 0),
         legend = c("<=-3.0", "-3.0..-2.0", "-2.0..-1.5", "-1.5..-1.0",
                    "-1.0..-0.5", "-0.5..0.5", "0.5..1.0", "1.0..1.5",
                    "1.5..2.0", "2.0..2.5", "2.5..3.0", ">3.0"),
         fill = heatcolors, bty = "n")

}
