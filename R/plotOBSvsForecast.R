#' @title Plot observations versus forecast
#'
#' @param inDir folder containing forecast files
#' @param areaOfInterest SpatialPolygon* identifying the area affected by fires.
#' @param threshold danger threshold calculated using the function
#' \code{DangerLevels()}, usually the high danger level.
#' @param startDate FWI upper limit to visualise
#' (the default is the maximum FWI)
#' @param endDate named vector of values to plot as vertical lines
#' (this can be quantiles for comparison)
#' @param obsFilePath file path to observations (3D raster)
#' @param origin This is the rating system of interest:
#' fwi (default), mark5, nfdrs.
#' @param index This is the index to analyse
#' (default is fwi, belonging to fwi origin).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   plotOBSvsForecast(inDir = "/scratch/mo/moc0/fire/GEFF1.2/forecast", 
#'                     areaOfInterest = fireBBOX, 
#'                     threshold = 14, 
#'                     startDate = "2017-06-01", endDate = "2017-06-30",
#'                     obsFilePath = "/scratch/mo/moc0/fire/events/CAMS_2017-06-01_2017-06-19_frpfire_rotated.nc")
#' }
#'

plotOBSvsForecast <- function(inDir,
                              areaOfInterest,
                              threshold,
                              startDate,
                              endDate,
                              obsFilePath,
                              origin = "fwi",
                              index = "fwi"){

  myDates <- seq.Date(from = as.Date(startDate), 
                      to = as.Date(endDate),
                      by = "day")

  # Initialise the matrix to hold the values
  rastMean <- matrix(NA, nrow = length(myDates), ncol = length(myDates))

  message("Collating forecast information")

  # For each starting date and forecast date, calculate the percentage of
  # pixels exceeding the high danger level
  for (i in 1:length(myDates)) {

    # transform dates to strings to build file name
    startD <- gsub("-", "", as.character(myDates[i]))

    for (j in 1:length(myDates)) {

      # transform dates to strings to build file name
      endD <- gsub("-", "", as.character(myDates[j]))

      file2read <- file.path(inDir,
                             paste0(startD, "_", endD, "_ecfire_fc_",
                             origin, "_", index, ".nc"))

      if (file.exists(file2read)) {

        rasterMap <- raster::raster(file2read)

        if (round(rasterMap@extent@xmin,0) == 0) {

          rasterMap <- raster::rotate(rasterMap)

        }

        rM <- raster::crop(rasterMap, areaOfInterest)
        nTotal <- dim(rM)[1]*dim(rM)[2]
        perc <- length(rM[rM >= threshold])/nTotal
        rastMean[i,j] <- perc

      }

    }

  }

  # Check for empty rows
  rastMean <- rastMean[rowSums(is.na(rastMean)) != length(myDates), ]
  # Check for empty columns
  rastMean <- rastMean[, colSums(is.na(rastMean)) != dim(rastMean)[1]]
  ForecastDates <- myDates[1:dim(rastMean)[1]]
  ObservationDates <- myDates[1:dim(rastMean)[2]]

  # reshape the data.frame with forecast values
  x <- reshape2::melt(rastMean*100)

  frp <- raster::brick(obsFilePath)
  frpCROPPED <- maskcropsub(r = frp, p = areaOfInterest,
                            mask = TRUE, crop = FALSE)
  frpTS <- as.numeric(raster::cellStats(frpCROPPED, sum))
  dfFRP <- data.frame(date = ForecastDates[1:length(frpTS)],
                      frp_original = frpTS,
                      frp = plotrix::rescale(frpTS, c(0, length(frpTS))))
  labelsObs <- round(plotrix::rescale(1:length(frpTS),
                                      c(1, max(frpTS))), 0)[1:length(frpTS)]

  # Make the boxy forecast plot using ggplot2
  p <- ggplot(x, aes_string("Var2", "Var1")) +
    geom_tile(aes_string(fill = "value"), colour = "white") +
    scale_fill_distiller(palette = "Spectral", na.value = NA, limits = c(0,100),
                         name = "% of pixels\nexceeding the\nhigh danger level") +
    geom_line(data=dfFRP, aes(x=1:dim(dfFRP)[1], y=frp), linetype = 2, col="#47494c") +
    scale_color_manual("Fire radiative power",
                       values=c("gray", "#47494c"),
                       labels="Fire radiative power",
                       guide=guide_legend(direction="vertical",
                                          title.position="top",
                                          override.aes=list(shape = "A"))) +
    theme_bw() + labs(x = "Observation date", y = "Forecast date") +
    scale_x_continuous(expand = c(0, 0),
                       breaks = 1:length(ObservationDates),
                       labels = as.character(ObservationDates)) +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.major = element_blank(), legend.position = c(.08, .85)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = 1:length(ForecastDates),
                       labels = as.character(ForecastDates),
                       sec.axis = sec_axis(~.,
                                           name = "Fire radiative power [Wm-2]",
                                           breaks = 1:length(labelsObs),
                                           labels = labelsObs)) +
    theme(plot.title = element_text(hjust = 0.5))

  return(p)

}
