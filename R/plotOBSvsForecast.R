# Copyright 2016 European Centre for Medium-Range Weather Forecasts (ECMWF)
# This software is licensed under the terms of the Apache Licence Version 2.0 
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#' Plot observations versus forecast
#'
#' @param inDir folder containing forecast files
#' @param areaOfInterest SpatialPolygon* identifying the area affected by fires.
#' @param threshold danger threshold calculated using the function \code{DangerLevels()}, usually the high danger level.
#' @param startDate FWI upper limit to visualise (the default is the maximum FWI)
#' @param endDate named vector of values to plot as vertical lines (this can be quantiles for comparison)
#' @param obsFilePath file path to observations (3D raster)
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'   plotOBSvsForecast(inDir = "/scratch/mo/moc0/fire/GEFF1.2/forecast_fwi", 
#'                     areaOfInterest = fireBBOX, 
#'                     threshold = 20, 
#'                     startDate = "2017-01-01", endDate = "2017-01-31",
#'                     obsFilePath = "CAMS_2017-01-01_2017-01-31_frpfire_rotated.nc")
#' }
#'

plotOBSvsForecast <- function(inDir, 
                              areaOfInterest, 
                              threshold, 
                              startDate,
                              endDate,
                              obsFilePath){
  
  ForecastDates <- ObservationDates <- seq.Date(from = as.Date(startDate), 
                                                to = as.Date(endDate),
                                                by = "day")
  
  # Initialise the matrix to hold the values
  rastMean <- matrix(NA, 
                     nrow = length(ObservationDates), 
                     ncol = length(ForecastDates))
  
  message("Collating forecast information")
  
  # For each starting date and forecast date, calculate average the percentage of
  # pixels exceeding the high danger level
  for (i in 1:length(ObservationDates)){
    
    # transform dates to strings to build file name
    startD <- gsub("-", "", as.character(ObservationDates[i]))
    
    for (j in 1:length(ForecastDates)){
      
      # transform dates to strings to build file name
      endD <- gsub("-", "", as.character(ForecastDates[j]))
      
      file2read <- file.path(inDir, 
                             paste0(startD, "_", endD, "_ecfire_fwi_fwi.nc"))
      
      if (file.exists(file2read)){
        
        rasterMap <- raster::raster(file2read)
        if (round(rasterMap@extent@xmin,0) == 0){
          rasterMap <- raster::rotate(rasterMap)
        }
        rM <- raster::crop(rasterMap, areaOfInterest)
        nTotal <- dim(rM)[1]*dim(rM)[2]
        perc <- length(rM[rM >= threshold])/nTotal
        rastMean[i,j] <- perc
        
      }
      
    }
    
  }
  x <- reshape2::melt(rastMean*100)
  
  frp <- raster::brick(obsFilePath)
  frpCROPPED <- maskcropsub(r = frp, p = areaOfInterest, 
                            mask = TRUE, crop = FALSE)
  frpTS <- as.numeric(raster::cellStats(frpCROPPED, sum))
  dfFRP <- data.frame(date = ForecastDates, 
                      frp = plotrix::rescale(frpTS, c(0, length(ForecastDates))))
  labelsObs <- round(plotrix::rescale(1:length(ForecastDates), 
                                      c(min(frpTS), max(frpTS))),
                     0)[1:dim(dfFRP)[1]]
  
  # Make the boxy forecast plot using ggplot2
  p <- ggplot(x, aes_string("Var1", "Var2")) + 
    geom_tile(aes_string(fill = "value"), colour = "white") + 
    scale_fill_distiller(palette = "Spectral", na.value = NA,
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
                       breaks = 1:dim(dfFRP)[1], 
                       labels = as.character(ObservationDates)) +
    theme(axis.text.x = element_text(angle = 90), 
          panel.grid.major = element_blank(), legend.position = c(.08, .85)) +
    scale_y_continuous(expand = c(0, 0), 
                       breaks = 1:dim(dfFRP)[1], 
                       labels = as.character(ForecastDates),
                       sec.axis = sec_axis(~., 
                                           name = "Fire radiative power [Wm-2]", 
                                           breaks = 1:dim(dfFRP)[1],
                                           labels = labelsObs)) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  # ggsave(file="/scratch/mo/moc0/fire/ChileFRP.eps")
  
  return(p)
  
}
