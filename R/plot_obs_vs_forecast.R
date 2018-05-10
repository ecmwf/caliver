#' @title plot_obs_vs_forecast
#'
#' @description Plot observations versus forecast
#'
#' @param input_dir folder containing forecast files
#' @param p SpatialPolygon* identifying the area affected by fires.
#' @param threshold danger threshold calculated using the function
#' \code{DangerLevels()}, usually the high danger level.
#' @param start_date date when observations start
#' @param end_date date when observations end
#' @param obs_file_path file path to observations (3D raster)
#' @param forecast_type type of forecast: fc (high resolution/deterministic),
#' cf (control forecast), pf (perturbed forecast/probabilistic)
#' @param origin This is the rating system of interest:
#' fwi (default), mark5, nfdrs.
#' @param index This is the index to analyse
#' (default is fwi, belonging to fwi origin).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_obs_vs_forecast(input_dir = "/scratch/mo/moc0/fire/GEFF1.2/forecast",
#'                     p = fireBBOX,
#'                     threshold = 14,
#'                     start_date = "2017-06-01", end_date = "2017-06-30",
#'                     obs_file_path = "CAMS_2017-06-01_2017-06-19_frpfire.nc")
#' }
#'

plot_obs_vs_forecast <- function(input_dir,
                                 p = NULL,
                                 threshold,
                                 start_date,
                                 end_date,
                                 obs_file_path,
                                 forecast_type = "hr",
                                 origin = "cfwis",
                                 index = "ffwi"){

  my_dates <- seq.Date(from = as.Date(start_date),
                      to = as.Date(end_date),
                      by = "day")

  message("Collating forecast information")

  # For each starting date and forecast date, calculate the percentage of
  # pixels exceeding the high danger level
  s <- raster::stack()
  for (i in 1:length(my_dates)) {

    # transform dates to strings to build file name
    start_d <- gsub("-", "", as.character(my_dates[i]))

    file2read <- file.path(input_dir,
                           paste0(origin, "_", index, "_",
                                  start_d, "_1200_",
                                  forecast_type, ".nc"))

    if (file.exists(file2read)) {
      s <- raster::stack(s, raster::brick(file2read))
    }

  }

  if (round(s@extent@xmin, 0) == 0) {

    s <- raster::rotate(s)

  }

  if (!is.null(p)) {

    s_cropped <- mask_crop_subset(s, p)

  } else {

    s_cropped <- s

  }

  # reclassify the values into two groups
  # all values < threshold become 0, the others are 1
  m <- c(0, threshold, 0,
         threshold, 10000000000, 1)
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
    if (j+9 <= length(my_dates)){
      w <- 10
    }else{
      w <- length(my_dates) - min(length(my_dates), j) + 1
    }
    raster_mean_matrix[i, j:(j+w-1)] <- xmat[i, 1:w]
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

  frp <- raster::brick(obs_file_path)
  frp_cropped <- mask_crop_subset(r = frp, p = p, mask = TRUE, crop = FALSE)
  frp_ts <- as.numeric(raster::cellStats(frp_cropped, sum))
  df_frp <- data.frame(date = forecast_dates[1:length(frp_ts)],
                      frp_original = frp_ts,
                      frp = plotrix::rescale(frp_ts, c(0, length(frp_ts))))
  labels_obs <- round(plotrix::rescale(1:length(frp_ts),
                                      c(1, max(frp_ts))), 0)[1:length(frp_ts)]

  my_name <- "% of pixels\nexceeding the\nhigh danger level"
  # Make the boxy forecast plot using ggplot2
  final_plot <- ggplot(x, aes_string("Var2", "Var1")) +
    geom_tile(aes_string(fill = "value"), colour = "white") +
    scale_fill_distiller(palette = "Spectral",
                         na.value = NA,
                         limits = c(0, 100),
                         name = my_name) +
    geom_line(data = df_frp,
              aes(x = 1:dim(df_frp)[1], y = frp),
              linetype = 2,
              col = "#47494c") +
    scale_color_manual("Fire radiative power",
                       values = c("gray", "#47494c"),
                       labels = "Fire radiative power",
                       guide = guide_legend(direction = "vertical",
                                            title.position = "top",
                                            override.aes = list(shape = "A"))) +
    theme_bw() + labs(x = "Observation date", y = "Forecast date") +
    scale_x_continuous(expand = c(0, 0),
                       breaks = 1:length(observation_dates),
                       labels = as.character(observation_dates)) +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.major = element_blank(), legend.position = c(.08, .85)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = 1:length(forecast_dates),
                       labels = as.character(forecast_dates),
                       sec.axis = sec_axis(~.,
                                           name = "Fire radiative power [Wm-2]",
                                           breaks = 1:length(labels_obs),
                                           labels = labels_obs)) +
    theme(plot.title = element_text(hjust = 0.5))

  return(final_plot)

}
