#' @title make_forecast_summary
#'
#' @description Generate a summary table of deterministic forecasts compared
#' with daily climatology.
#' This function is EXPERIMENTAL, therefore not exported yet!
#'
#' @param input_dir folder containing forecast files
#' @param p SpatialPolygon* identifying the area affected by fires
#' @param event_dates date when observations end
#' @param obs file path to observations (optional input)
#' @param clima file path to dataset containing the climatological information.
#' @param origin This is the rating system of interest:
#' fwi (default, currently called cfwis), mark5, nfdrs.
#' @param index This is the index to analyse
#' (default is fwi, belonging to fwi origin).
#'
#' @examples
#' \dontrun{
#'   df <- make_forecast_summary(input_dir = "forecast",
#'                               p = fireBBOX,
#'                               event_dates = event_dates,
#'                               obs = "CAMS_frpfire.nc")
#' }
#'

make_forecast_summary <- function(input_dir,
                                  p = NULL,
                                  event_dates,
                                  obs = NULL,
                                  clima,
                                  origin = "FWI",
                                  index = "fwi"){

  message("Preparing climatology")
  leap_year <- seq.Date(from = as.Date("2000-01-01"),
                        to = as.Date("2000-12-31"),
                        by = "day")
  idx <- which(format(leap_year, "%m-%d") %in% format(event_dates, "%m-%d"))
  fire_clima <- raster::brick(clima)[[idx]]
  fire_clima <- mask_crop_subset(r = fire_clima, p = p,
                                 mask = TRUE, crop = TRUE)

  message("Stacking forecast information")
  # For each starting date and forecast date, calculate the percentage of
  # pixels exceeding the clima
  w <- raster::stack()
  for (i in seq_along(event_dates)) {
    print(event_dates[i])

    # transform dates to strings to build file name
    start_d <- gsub("-", "", as.character(event_dates[i]))

    file2read <- file.path(input_dir,
                           paste0("ECMWF_", origin,
                                  "_", start_d, "_1200_hr_", index, ".nc"))

    # Load file as brick
    if (file.exists(file2read)) {
      r <- raster::brick(file2read)
    }

    # Check extent
    if (is.null(raster::intersect(raster::extent(r),
                                  raster::extent(fire_clima)))) {
      r = raster::rotate(r)
    }

    # Crop, if necessary
    if (!is.null(p)) {
      r <- mask_crop_subset(r, p)
    }

    if (i == 1) {
      message("Resampling clima to match forecast (this is done only once!)")
      fire_clima <- raster::resample(x = fire_clima, y = r, method = "ngb")
    }

    # Stack forecasts
    for (j in 1:nlayers(r)){
      fc_date <- as.Date(gsub(pattern = "\\.", replacement = "-",
                              x = substr(names(r[[j]]), 2, 11)))
      idx <- which(event_dates == fc_date)
      if (length(idx) > 0){
        r[[j]] <- r[[j]] > fire_clima[[idx]]
      }else{
        r[[j]] <- 0
      }
    }
    w <- raster::stack(w, r)

  }

  # Total number of non-NA cells
  n_total <- sum(as.vector(!is.na(w[[1]])))

  # Calculate percentages
  x <- round(raster::cellStats(w, sum) / n_total, 2) * 100
  xmat <- matrix(x, ncol = 10, byrow = TRUE)

  # Initialise the matrix to hold the values
  raster_mean_matrix <- matrix(NA,
                               nrow = length(event_dates),
                               ncol = length(event_dates))

  # Populate the matrix
  j <- 1
  for (i in seq_along(event_dates)){
    if (j + 9 <= length(event_dates)){
      w <- 10
    }else{
      w <- length(event_dates) - min(length(event_dates), j) + 1
    }
    raster_mean_matrix[i, j:(j + w - 1)] <- xmat[i, 1:w]
    j <- j + 1
  }

  raster_mean_matrix <- data.frame(raster_mean_matrix)
  raster_mean_matrix$event_dates <- event_dates

  if (!is.null(obs)){

    message("Preparing observations")
    fire_obs <- raster::brick(obs)

    obs_dates <- as.Date(gsub(pattern = "\\.", replacement = "-",
                              x = substr(names(fire_obs),
                                         start = 2, stop = 11)))
    idx <- which(format(obs_dates, "%m-%d") %in% format(event_dates, "%m-%d"))
    fire_obs <- fire_obs[[idx]]

    if (is.null(intersect(extent(fire_obs), extent(fire_clima)))) {
      fire_obs <- raster::rotate(fire_obs)
    }

    if (!is.null(p)) {
      fire_obs <-  mask_crop_subset(r = fire_obs, p = p,
                                    mask = TRUE, crop = TRUE)
    }

    # Extract time series
    frp_ts <- as.numeric(raster::cellStats(fire_obs, sum))
    obs_factor <- (max(frp_ts) - min(frp_ts))/length(frp_ts)

    # Add observation data to data.frame
    raster_mean_matrix$obs <- frp_ts
    raster_mean_matrix$obs_rescaled <- frp_ts / obs_factor
    raster_mean_matrix$obs_index <- 1:length(frp_ts)

    # reshape the data.frame with forecast values
    df <- reshape2::melt(raster_mean_matrix,
                         id.vars = c("event_dates",
                                     "obs_index", "obs", "obs_rescaled"))

  }else{

    # reshape the data.frame with forecast values
    df <- reshape2::melt(raster_mean_matrix, id.vars = c("event_dates"))

  }

  df$variable <- as.numeric(gsub("X", "", df$variable))

  return(df)

}


#' @title plot_forecast_summary
#'
#' @description Generate a plot of deterministic forecasts compared
#' with daily climatology.
#' This function is EXPERIMENTAL, therefore not exported yet!
#'
#' @param df data.frame obtained from \code{make_forecast_summary()}.
#'
#' @examples
#' \dontrun{
#'   df <- make_forecast_summary(input_dir = "forecast",
#'                               p = fireBBOX,
#'                               event_dates = event_dates,
#'                               obs = "CAMS_frpfire.nc")
#'   plot_forecast_summary(df)
#' }
#'

plot_forecast_summary <- function(df){

  final_plot <- ggplot(df) +
    geom_tile(aes_string(x = "variable", y = "obs_index", fill = "value"),
              colour = "white") +
    scale_fill_distiller(palette = "Spectral",
                         na.value = NA,
                         limits = c(0, 100),
                         name = "% of pixels\nexceeding\nthreshold") +
    theme_bw() + labs(x = "Observation date", y = "Forecast issue date") +
    scale_x_continuous(expand = c(0, 0),
                       breaks = unique(df$variable),
                       labels = as.character(unique(df$event_dates))) +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5))

  if ("obs" %in% names(df)){

    obs_factor <- (max(df$obs) - min(df$obs))/max(df$obs_index)

    final_plot <- final_plot +
      geom_line(data = df,
                aes_string(x = "obs_index", y = "obs_rescaled"),
                linetype = 2,
                col = "#47494c") +
      scale_y_continuous(sec.axis = sec_axis(~ .*obs_factor,
                                             name = "Fire radiative power [Wm-2]"),
                         expand = c(0, 0),
                         breaks = unique(df$variable),
                         labels = as.character(unique(df$event_dates)))
  }else{
    final_plot <- final_plot +
      scale_y_continuous(expand = c(0, 0),
                         breaks = unique(df$variable),
                         labels = as.character(unique(df$event_dates)))
  }

  return(final_plot)

}


### Old version, to be deprecated

#' @title forecast_summary (TO BE DEPRECATED SOON!)
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
