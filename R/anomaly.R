#' @title anomaly
#'
#' @description This function calculates the anomaly (a standard deviation
#' from the mean climatology) of a forecast layer
#'
#' @param r is the Raster layer to compare to the climatology.
#' @param clima RasterBrick containing the climatological information.
#'
#' @details More information on anomaly is available here:
#' https://bit.ly/2Qvekz4. You can find fire climatology data for the FWI
#' indices in Zenodo: https://zenodo.org/communities/wildfire.
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

  options(warn=-1)

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

  anomaly_map_cat <- r
  for (i in 1:raster::nlayers(r)){
    print(i)
    rx <- r[[i]]
    # Get the forecast dates
    forecast_date <- substr(x = names(rx), start = 7, stop = nchar(names(rx)))

    # Get the climatology dates
    clima_dates <- substr(x = names(clima), start = 7, stop = 11)
    # Get indices of forecast date
    clima_idx <- which(clima_dates == forecast_date)

    # Mean from clima
    mean_clima <- raster::calc(x = clima[[clima_idx]], fun = mean)
    mean_clima <- raster::resample(x = mean_clima, y = rx)
    # Standard deviation from clima
    sd_clima <- raster::calc(x = clima[[clima_idx]], fun = sd)
    sd_clima <- raster::resample(x = sd_clima, y = rx)

    # Generate anomaly map
    anomaly_map <- (rx - mean_clima) / sd_clima

    # Categorize the anomaly
    a1 <- anomaly_map <= -3
    a2 <- (anomaly_map > -3 & anomaly_map <= -2)
    a3 <- (anomaly_map > -2 & anomaly_map <= -1.5)
    a4 <- (anomaly_map > -1.5 & anomaly_map <= -1)
    a5 <- (anomaly_map > -1 & anomaly_map <= -0.5)
    a6 <- (anomaly_map > -0.5 & anomaly_map <= +0.5)
    a7 <- (anomaly_map > +0.5 & anomaly_map <= +1)
    a8 <- (anomaly_map > +1 & anomaly_map <= +1.5)
    a9 <- (anomaly_map > +1.5 & anomaly_map <= +2)
    a10 <- (anomaly_map > +2 & anomaly_map <= +3)
    a11 <- anomaly_map > 3

    if (raster::nlayers(r) > 1){
      # anomaly_map_cat <- anomaly_map
      anomaly_map_cat[[i]][] <- NA
      anomaly_map_cat[[i]][a1] <- 1
      anomaly_map_cat[[i]][a2] <- 2
      anomaly_map_cat[[i]][a3] <- 3
      anomaly_map_cat[[i]][a4] <- 4
      anomaly_map_cat[[i]][a5] <- 5
      anomaly_map_cat[[i]][a6] <- 6
      anomaly_map_cat[[i]][a7] <- 7
      anomaly_map_cat[[i]][a8] <- 8
      anomaly_map_cat[[i]][a9] <- 9
      anomaly_map_cat[[i]][a10] <- 10
      anomaly_map_cat[[i]][a11] <- 11
    }else{
      # anomaly_map_cat <- anomaly_map
      anomaly_map_cat[] <- NA
      anomaly_map_cat[a1] <- 1
      anomaly_map_cat[a2] <- 2
      anomaly_map_cat[a3] <- 3
      anomaly_map_cat[a4] <- 4
      anomaly_map_cat[a5] <- 5
      anomaly_map_cat[a6] <- 6
      anomaly_map_cat[a7] <- 7
      anomaly_map_cat[a8] <- 8
      anomaly_map_cat[a9] <- 9
      anomaly_map_cat[a10] <- 10
      anomaly_map_cat[a11] <- 11
    }
  }

  return(anomaly_map_cat)

}

#' @title plot_anomaly
#'
#' @description Plot anomaly map as shown in GWIS (\url{https://bit.ly/2BbBfsm})
#'
#' @param anomaly_map is the Raster layer, result of \code{anomaly()}.
#' @param custom_palette palette to use (default is \code{viridis::plasma})
#' @param ... other plotting arguments, see \code{?raster::plot} function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   r <- brick("cfwis_ffwi_20170101_1200_00.nc")[[1]]
#'   clima <- brick("fwi.nc")
#'   anomaly_map <- anomaly(r, clima)
#'   plot_anomaly(anomaly_map,
#'                custom_palette = colorRamps::matlab.like(n = length(breaks)))
#' }
#'

plot_anomaly <- function(anomaly_map, custom_palette = NULL, ...){

  breaks <- 1:11

  if (is.null(custom_palette)){
    # Define palette
    custom_palette <- rev(viridis::plasma(n = length(breaks)))
  }

  if (raster::nlayers(anomaly_map) == 1){
    # place the legend outside the map
    par(xpd = FALSE)
    raster::plot(anomaly_map,
                 addfun = .background_map_fun,
                 col = custom_palette,
                 breaks = breaks,
                 legend = FALSE,
                 ...)
    par(xpd = TRUE)
    legend(x = round(par("usr")[2] + (par("usr")[2] - par("usr")[1]) / 90, 0),
           y = round(mean(c(par("usr")[3], par("usr")[4])), 0),
           legend = c("<=-3.0", "-3.0..-2.0", "-2.0..-1.5", "-1.5..-1.0",
                      "-1.0..-0.5", "-0.5..0.5", "0.5..1.0", "1.0..1.5",
                      "1.5..2.0", "2.0..3.0", ">3.0"),
           fill = custom_palette, bty = "n")
  }else{
    # place the legend beside each map
    raster::plot(anomaly_map,
                 addfun = .background_map_fun,
                 col = custom_palette,
                 breaks = breaks,
                 lab.breaks = c("<=-3.0", "-3.0..-2.0", "-2.0..-1.5",
                                "-1.5..-1.0", "-1.0..-0.5", "-0.5..0.5",
                                "0.5..1.0", "1.0..1.5", "1.5..2.0", "2.0..3.0",
                                ">3.0"),
                 ...)
  }

}
