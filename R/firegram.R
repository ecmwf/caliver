#' @title firegram
#'
#' @description This function plots the distribution of the daily ensemble,
#' against the hres and previously calculated thresholds
#'
#' @param thresholds is a numeric vector containing the index thresholds
#' corresponding to: very low, low, moderate, high, very high, extreme danger
#' @param hres is the RasterBrick containing the high resolution forecasts
#' @param ens is the RasterBrick containing the ensemble forecasts
#' @param max_index is the upper limit of the y-axis
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   firegram(thresholds, hres, ens)
#' }
#'

firegram <- function(thresholds, hres, ens, max_index = 25){

  danger <- day <- fwi <- fwi1 <- fwi2 <- NULL

  # Calculate the median FWI over the region
  re_df <- data.frame(fwi1 = c(0, thresholds),
                      fwi2 = c(thresholds, max_index),
                      danger = c("No danger", "Very Low", "Low", "Moderate",
                                 "High", "Very High", "Extreme"),
                      stringsAsFactors = FALSE)
  re_df$danger <- factor(x = re_df$danger,
                         levels = c("No danger", "Very Low", "Low", "Moderate",
                                    "High", "Very High", "Extreme"))

  # HRES
  hres <- round(raster::cellStats(hres, stat = median), 0)
  hres_df <- data.frame(fwi = hres, day = 1:10)

  # ENS
  ens_df <- data.frame(matrix(NA, nrow = 0, ncol = 2))
  names(ens_df) <- c("fwi", "day")
  for (i in 1:51){
    startx <- seq(1, 510, 10)[i]
    endx <- seq(10, 510, 10)[i]
    # Calculate the median FWI over the region
    ensx <- round(raster::cellStats(ens[[startx:endx]], stat = median), 0)
    ensx <- data.frame(fwi = ensx, day = 1:10)
    ens_df <- rbind(ens_df, ensx)
  }

  ggplot() +
    geom_rect(data = re_df,
              mapping = aes(ymin = fwi1, ymax = fwi2, xmin = 0, xmax = 11,
                            fill = danger), alpha = 0.3) +
    geom_boxplot(data = ens_df, aes(x = day, y = fwi, group = day)) +
    geom_point(data = hres_df, aes(x = day, y = fwi, group = day),
               size = 3, color = "coral") +
    xlab("Day") + ylab("FWI") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(limits = c(0, 11), expand = c(0, 0),
                       breaks = round(seq(1, 10, by = 1), 0)) +
    scale_y_continuous(limits = c(0, max_index), expand = c(0, 0))

}
