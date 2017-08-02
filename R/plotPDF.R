#' @title Plot PDF of fire index
#'
#' @param fireIndex RasterBrick containing the fire index
#' @param thresholds thresholds calculated using the function
#' \code{DangerLevels()}
#' @param upperLimit FWI upper limit to visualise
#' (the default is the maximum FWI)
#' @param vLines named vector of values to plot as vertical lines
#' (this can be quantiles for comparison)
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   plotPDF(fireIndex, thresholds, upperLimit = 100)
#' }
#'

plotPDF <- function(fireIndex,
                    thresholds,
                    upperLimit = NULL,
                    vLines = NULL){

  firePalette <- c("#4DAF4A", "#FFFF33", "#FF7F00",
                   "#E41A1C", "#6b3535", "#403131")

  IDX <- na.omit(as.vector(fireIndex))
  IDXno0 <- IDX[IDX > 0]

  # percentiles corresponding to the danger thresholds:
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

  p <- ggplot(as.data.frame(IDXno0), aes(IDXno0)) +
    geom_density(colour = "#6d6b6b", fill = "grey") +
    geom_segment(aes(x = x1, y = y1,
                     xend = x2, yend = y2, colour = df$DangerLevels),
                 data = df, size = 14) +
    scale_colour_manual(name="Danger classes",
                        values = firePalette,
                        labels = c("Very Low", "Low", "Moderate",
                                   "High", "Very high", "Extreme")) +
    geom_text(aes(x = x1 + hdistance, y = y1,
                         label = round(x1,0)),
              data = df[2:6,], size=5, colour = "#e1dbdb") +
    theme_bw() + xlab("FWI") + ylab("Density") +
    theme(legend.title = element_text(size=10, face="bold"),
          text = element_text(size=10, lineheight=.8),
          legend.position = c(0.98, 0.98), legend.justification = c(1, 1)) +
    scale_x_continuous(limits = c(0, upperLimit))

  label <- value <- NULL # to avoid NOTE during check
  if (!is.null(vLines)) {

    dfv <- data.frame(label = names(vLines), value = as.numeric(vLines))
    p <- p + geom_vline(data=dfv, mapping=aes(xintercept=value),
                        color="darkgray", linetype = 2) +
      geom_text(data=dfv, mapping=aes(x=value, y=max(density(IDXno0)$y),
                                      label=label),
                angle=90, vjust=-0.4, colour = "#585858")

  }

  return(p)

}
