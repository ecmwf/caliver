#' @title forecast_summary_clima
#'
#' @description Plot observations versus forecast using daily climatology
#' (EXPERIMENTAL, not exported yet).
#'
#' @param input_dir folder containing forecast files
#' @param r Raster* object containing the forecasts
#' @param p SpatialPolygon* identifying the area affected by fires
#' @param threshold percentile to be used as danger threshold (default is 0.95).
#' @param start_date date when observations start
#' @param end_date date when observations end
#' @param obs observations, it can either be a file path or a RasterStack/Brick
#' @param origin This is the rating system of interest:
#' fwi (default, currently called cfwis), mark5, nfdrs.
#' @param index This is the index to analyse
#' (default is fwi, belonging to fwi origin).
#' @param clima RasterBrick containing the climatological information.
#'
#' @examples
#' \dontrun{
#'   forecast_summary(input_dir = "forecast",
#'                    p = fireBBOX,
#'                    threshold = 0.95,
#'                    start_date = "2017-06-01",
#'                    end_date = "2017-06-30",
#'                    obs = "CAMS_2017-06-01_2017-06-19_frpfire.nc")
#' }
#'

forecast_summary_clima <- function(input_dir,
                                   r = NULL,
                                   p = NULL,
                                   threshold = 0.95,
                                   start_date,
                                   end_date,
                                   obs = NULL,
                                   origin = "FWI",
                                   index = "fwi",
                                   clima){

  my_dates <- seq.Date(from = as.Date(start_date),
                       to = as.Date(end_date),
                       by = "day")

  fire_clima <- daily_clima(r = clima,
                            dates = seq.Date(from = as.Date(start_date),
                                             to = as.Date(end_date) + 10,
                                             by = "day"),
                            probs = threshold)

  message("Collating forecast information")

  if (is.null(r)){
    # For each starting date and forecast date, calculate the percentage of
    # pixels exceeding the high danger level
    w <- raster::stack()
    for (i in seq_along(my_dates)) {
      print(my_dates[i])

      # transform dates to strings to build file name
      start_d <- gsub("-", "", as.character(my_dates[i]))

      file2read <- file.path(input_dir,
                             paste0("ECMWF_", origin,
                                    "_", start_d, "_1200_hr_", index, ".nc"))

      if (file.exists(file2read)) {
        r <- raster::brick(file2read)
      }

      if (round(r@extent@xmin, 0) == 0) {

        r <- raster::rotate(r)

      }

      if (!is.null(p)) {

        s_cropped <- mask_crop_subset(r, p)

      } else {

        s_cropped <- r

      }

      s_res <- raster::resample(x = s_cropped, y = fire_clima)
      for (j in 1:nlayers(s_res)){
        idx <- which(substr(names(fire_clima), 1, 11) == names(s_res[[j]]))
        s_res[[j]] <- s_res[[j]] > fire_clima[[idx]]
      }

      w <- raster::stack(w, s_res)

    }
  }

  # Total number of non-NA cells
  n_total <- sum(as.vector(!is.na(w[[1]])))

  x <- round(raster::cellStats(w, sum) / n_total, 2) * 100
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

  if (!is.null(obs)){
    if ("RasterBrick" %in% class(obs)) {
      frp <- obs
    }else{
      frp <- raster::brick(obs)
    }
    frp_cropped <- mask_crop_subset(r = frp, p = p, mask = TRUE, crop = FALSE)
    frp_ts <- as.numeric(raster::cellStats(frp_cropped, sum))
    frp_ts_length <- length(frp_ts)
    df_frp <- data.frame(date = forecast_dates[1:frp_ts_length],
                         frp_original = frp_ts,
                         frp = plotrix::rescale(frp_ts, c(0, frp_ts_length)))
    labels_obs <- round(plotrix::rescale(1:frp_ts_length,
                                         c(1, max(frp_ts))),
                        0)[1:frp_ts_length]
  }

  # Make the boxy forecast plot using ggplot2
  final_plot <- ggplot(x, aes_string("Var2", "Var1")) +
    geom_tile(aes_string(fill = "value"), colour = "white") +
    scale_fill_distiller(palette = "Spectral",
                         na.value = NA,
                         limits = c(0, 100),
                         name = paste0("% of pixels\nexceeding threshold",
                                       "\npercentile = ", threshold)) +
    theme_bw() + labs(x = "Observation date", y = "Forecast date") +
    scale_x_continuous(expand = c(0, 0),
                       breaks = seq_along(observation_dates),
                       labels = as.character(observation_dates)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq_along(forecast_dates),
                       labels = as.character(forecast_dates)) +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.major = element_blank(), legend.position = c(.08, .82)) +
    theme(plot.title = element_text(hjust = 0.5))

  if (!is.null(obs)){
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
      scale_y_continuous(sec.axis = sec_axis(~.,
                                             name = myname,
                                             breaks = seq_along(labels_obs),
                                             labels = labels_obs))
  }

  return(final_plot)

}
