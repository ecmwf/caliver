#' @title forecast_summary
#'
#' @description Plot observations versus forecast
#'
#' @param input_dir folder containing forecast files
#' @param r Raster* object containing the forecasts
#' @param p SpatialPolygon* identifying the area affected by fires.
#' @param threshold danger threshold calculated using the function
#' \code{DangerLevels()}, usually the high danger level.
#' @param start_date date when observations start
#' @param end_date date when observations end
#' @param obs observations, it can either be a file path or a RasterStack/Brick
#' @param origin This is the rating system of interest:
#' fwi (default, currently called cfwis), mark5, nfdrs.
#' @param index This is the index to analyse
#' (default is fwi, belonging to fwi origin).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   forecast_summary(input_dir = "forecast",
#'                     p = fireBBOX,
#'                     threshold = 14,
#'                     start_date = "2017-06-01", end_date = "2017-06-30",
#'                     obs = "CAMS_2017-06-01_2017-06-19_frpfire.nc")
#' }
#'

forecast_summary <- function(input_dir,
                             r = NULL,
                             p = NULL,
                             threshold,
                             start_date,
                             end_date,
                             obs = NULL,
                             origin = "FWI",
                             index = "fwi"){

  my_dates <- seq.Date(from = as.Date(start_date),
                      to = as.Date(end_date),
                      by = "day")

  message("Collating forecast information")

  if (is.null(r)){
    # For each starting date and forecast date, calculate the percentage of
    # pixels exceeding the high danger level
    r <- raster::stack()
    for (i in seq_along(my_dates)) {

      # transform dates to strings to build file name
      start_d <- gsub("-", "", as.character(my_dates[i]))

      file2read <- file.path(input_dir,
                             paste0("ECMWF_", origin,
                                    "_", start_d, "_1200_hr_", index, ".nc"))

      if (file.exists(file2read)) {
        r <- raster::stack(r, raster::brick(file2read))
      }

    }
  }

  if (round(r@extent@xmin, 0) == 0) {

    r <- raster::rotate(r)

  }

  if (!is.null(p)) {

    s_cropped <- mask_crop_subset(r, p)

  } else {

    s_cropped <- r

  }

  # reclassify the values into two groups
  # all values < threshold become 0, the others are 1
  m <- c(-Inf, threshold, 0,
         threshold, Inf, 1)
  rclmat <- matrix(m, ncol = 3, byrow = TRUE)
  s_reclassified <- raster::reclassify(s_cropped, rclmat)

  # Total number of non-NA cells
  n_total <- sum(as.vector(!is.na(s_cropped[[1]])))

  x <- round(raster::cellStats(s_reclassified, sum) / n_total, 2) * 100
  xmat <- matrix(x, ncol = 10, byrow = TRUE)

  # Initialise the matrix to hold the values
  raster_mean_matrix <- matrix(NA,
                               nrow = length(my_dates),
                               ncol = length(my_dates))

  j <- 1
  for (i in seq_along(my_dates)){
    if (j + 9 <= length(my_dates)){
      w <- 10
    }else{
      w <- length(my_dates) - min(length(my_dates), j) + 1
    }
    raster_mean_matrix[i, j:(j + w - 1)] <- xmat[i, 1:w]
    j <- j + 1
  }

  # Remove empty rows
  raster_mean_matrix <- raster_mean_matrix[rowSums(is.na(raster_mean_matrix)) !=
                                             length(my_dates), ]
  # Remove empty columns
  raster_mean_matrix <- raster_mean_matrix[,
                                           colSums(is.na(raster_mean_matrix)) !=
                                             dim(raster_mean_matrix)[1]]

  forecast_dates <- my_dates[1:dim(raster_mean_matrix)[1]]
  observation_dates <- my_dates[1:dim(raster_mean_matrix)[2]]

  # reshape the data.frame with forecast values
  x <- reshape2::melt(raster_mean_matrix)

  # Make the boxy forecast plot using ggplot2
  final_plot <- ggplot(x, aes_string("Var2", "Var1")) +
    geom_tile(aes_string(fill = "value"), colour = "white") +
    scale_fill_distiller(palette = "Spectral",
                         na.value = NA,
                         limits = c(0, 100),
                         name = paste0("% of pixels\nexceeding\nFWI = ",
                                       threshold)) +
    theme_bw() +
    labs(x = "Observation date", y = "Forecast date") +
    scale_x_continuous(expand = c(0, 0),
                       breaks = seq_along(observation_dates),
                       labels = as.character(observation_dates)) +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.direction = "vertical",
          legend.box = "horizontal",
          legend.box.just = c("top"),
          legend.background = element_rect(fill = NA))

  if (!is.null(obs)){

    message("Extract observations")

    # Check whether obs is a string or a brick
    if ("RasterBrick" %in% class(obs)) {
      frp <- obs
    }else{
      frp <- raster::brick(obs)
    }

    # Check whether I need to crop the observation
    if (!is.null(p)) {
      frp_cropped <- raster::mask(frp, p, progress = "text")
    }else{
      frp_cropped <- frp
    }

    if (nchar(names(frp_cropped)[1]) > 11){
      frp_aggregated <- raster::stack()
      # Aggregate to daily
      for (dayx in unique(substr(names(frp_cropped), 2, 11))){
        print(dayx)
        idx <- which(substr(names(frp_cropped), 2, 11) == dayx)
        frp_aggregated <- raster::stack(frp_aggregated,
                                        raster::calc(frp_cropped[[idx]], sum))
      }
    }

    frp_ts <- as.numeric(raster::cellStats(frp_aggregated, sum,
                                           progress = "text"))
    df_frp <- data.frame(date = forecast_dates[seq_along(frp_ts)],
                         frp_original = frp_ts,
                         frp = plotrix::rescale(frp_ts, c(0, length(frp_ts))))
    labels_obs <- round(plotrix::rescale(seq_along(frp_ts),
                                         c(1, max(frp_ts))),
                        0)[seq_along(frp_ts)]

    mylist <- list(shape = "A")
    myname <- "Fire radiative power [Wm-2]"
    final_plot <- final_plot +
      geom_line(data = df_frp,
                aes(x = 1:dim(df_frp)[1], y = frp),
                linetype = 2,
                col = "#47494c") +
      scale_color_manual("Fire radiative power",
                         values = c("gray", "#47494c"),
                         labels = "Fire radiative power",
                         guide = guide_legend(direction = "vertical",
                                              title.position = "top",
                                              override.aes = mylist)) +
      scale_y_continuous(expand = c(0, 0),
                         breaks = seq_along(forecast_dates),
                         labels = as.character(forecast_dates),
                         sec.axis = sec_axis(~.,
                                             name = myname,
                                             breaks = seq_along(labels_obs),
                                             labels = labels_obs))
  }else{
    final_plot <- final_plot +
      scale_y_continuous(expand = c(0, 0),
                         breaks = seq_along(forecast_dates),
                         labels = as.character(forecast_dates))
  }

  return(final_plot)

}
