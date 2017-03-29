#' Plot PDF of fire index
#'
#' @param fireIndex fire index to calculate the thresholds for (default is fwi = fire weather index)
#' @param countryName string describing the country name.
#' @param baseDir this is the directory where the reanalysis data are saved.
#' @param fireSeasonIndex vector of indices (same length as dataDates) with TRUE if the date falls in the fire season, FALSE otherwise.
#' @param thresholds thresholds calculated using the function \code{DangerLevels()}
#' @param upperLimit FWI upper limit to visualise (the default is the maximum FWI)
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'   plotPDF(countryName = "Spain", baseDir, fireSeasonIndex, thresholds)
#' }
#'

plotPDF <- function(fireIndex = "fwi",
                    countryName,
                    baseDir,
                    fireSeasonIndex,
                    thresholds,
                    upperLimit = NULL){
  
  firePalette <- c("#4DAF4A", "#FFFF33", "#FF7F00",
                   "#E41A1C", "#6b3535", "#403131")
  
  # DENSITY PLOT WITH THRESHOLDS
  filename <- file.path(baseDir, 
                        paste0(fireIndex, "_rotated_masked_", 
                               countryName, ".grd"))
  IDXcountry <- raster::brick(filename)
  
  # Zero means there are are no suitable conditions to generate a fire, 
  # therefore zeros should be masked.
  IDX <- na.omit(as.vector(raster::subset(IDXcountry, fireSeasonIndex)))
  IDXno0 <- IDX[IDX > 0]
  
  # percentiles corresponding to the danger thresholds: 
  # round(ecdf(IDX)(thresholds),2)
  cols <- c("VeryLow" = firePalette[1], 
            "Low" = firePalette[2],
            "Moderate" = firePalette[3], 
            "High" = firePalette[4],
            "Very high" = firePalette[5], 
            "Extreme" = firePalette[6])
  
  if (is.null(upperLimit)) {
    upperLimit <- ceiling(max(IDXno0))
  }
  dangerClasses <- c(0, thresholds, upperLimit)
  
  vdistance <- max(density(IDXno0)$y)/15
  hdistance <- max(dangerClasses)/100
  
  x1 <- x2 <- y1 <- y2 <- NULL # to avoid warning in check!
  
  df <- data.frame(x1 = dangerClasses[1:6], 
                   x2 = dangerClasses[2:7],
                   y1 = min(density(IDXno0)$y) - vdistance, 
                   y2 = min(density(IDXno0)$y) - vdistance,
                   DangerLevels = factor(x = firePalette,
                                         levels=c(firePalette)))
  
  ggplot(as.data.frame(IDXno0), aes(IDXno0)) + 
    geom_density(colour = "#6d6b6b", fill = "grey") + 
    geom_segment(aes(x = x1, y = y1, 
                     xend = x2, yend = y2, colour = df$DangerLevels),
                 data = df, size = 14) +
    scale_colour_manual(name=paste0("Danger classes (", countryName, ")"), 
                        values = firePalette, 
                        labels = c("Very Low", "Low", "Moderate", 
                                   "High", "Very high", "Extreme")) +
    geom_text(aes(x = x1 + hdistance, y = y1, 
                         label = round(x1,0)), 
              data = df[2:6,], size=5, colour = "#e1dbdb") +
    theme_bw() + xlab("FWI") + ylab("Density") +
    theme(legend.title = element_text(size=14, face="bold"),
          text = element_text(size=14, lineheight=.8),
          legend.justification=c(1,0), legend.position=c(0.95,0.5)) +
    scale_x_continuous(limits = c(0, upperLimit))
  
}
