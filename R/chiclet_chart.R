#' @title make_chiclet_chart
#'
#' @description Generate a summary table of deterministic forecasts.
#' Example here:
#' https://news.ucar.edu/9996/long-range-tornado-prediction-it-feasible
#'
#' @param forecasts either a list of Raster* objects or a string vector
#' containing forecast file paths. The order of the list/paths should correspond
#' to forecasts issued consecutively.
#' @param type this can be one of the following: "raw" (default), "clima".
#' @param p (optional) SpatialPolygon* identifying the area affected by fires
#' @param obs (optional) Raster* object containing 1 layer per day of observation
#' @param clima (optional) list of Raster* objects containing the climatological information. This is obtained using the function daily_clima() with suitable dates (to cover the full forecasted period) and its extent should contain the polygon `p`, if this is used.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   forecasts <- c("./ECMWF_FWI_20170101_1200_hr_fwi.nc",
#'                  "./ECMWF_FWI_20170102_1200_hr_fwi.nc",
#'                  "./ECMWF_FWI_20170103_1200_hr_fwi.nc",
#'                  "./ECMWF_FWI_20170104_1200_hr_fwi.nc",
#'                  "./ECMWF_FWI_20170105_1200_hr_fwi.nc",
#'                  "./ECMWF_FWI_20170106_1200_hr_fwi.nc",
#'                  "./ECMWF_FWI_20170107_1200_hr_fwi.nc",
#'                  "./ECMWF_FWI_20170108_1200_hr_fwi.nc",
#'                  "./ECMWF_FWI_20170109_1200_hr_fwi.nc",
#'                  "./ECMWF_FWI_20170110_1200_hr_fwi.nc")
#'   p <- raster::getData(name = "GADM", country = "IT", level = 0)
#'   obs <- raster::brick("CAMS_2017-01-01_2017-01-10_frpfire.nc")
#'   clima <- raster::brick("fwi_1981_2010_Europe.nc")
#'   df <- make_chiclet_chart(forecasts, p, event_dates, obs, clima)
#' }
#'

make_chiclet_chart <- function(forecasts,
                               type = "raw",
                               p = NULL,
                               obs = NULL,
                               clima = NULL){

  if (type == "clima" & is.null(clima)) {
    stop("When type = 'clima', please provide non null clima object.")
  }

  # Index on the diagonal
  fc_date <- c()
  for (i in seq_along(forecasts)){

    # Read forecasts
    fc <- forecasts[[i]]
    if (class(fc) == "character"){
      fc <- raster::brick(fc)
    }
    fc_dates <- raster::getZ(fc) # This is a RasterBrick
    fc_date <- c(fc_date, as.character(fc_dates[1]))

    message(paste("Processing forecast issued on", fc_dates[1]))


    if (!is.null(p)){

      # Check extent and rotate, if necessary
      if (is.null(raster::intersect(raster::extent(fc), raster::extent(p)))) {
        fc = raster::rotate(fc)
      }

      # Crop forecast to polygon, if needed
      fc <- mask_crop_subset(fc, p)
    }

    if (type == "raw"){
      fc_df <- data.frame(raster::cellStats(fc, mean))
    }

    if (type == "clima"){

      message("Processing climatology")

      clima_dates <- as.Date(names(clima), format = "%Y-%m-%d")

      subset_mean <- function(x){
        idx <-  which(format(clima_dates, "%m-%d") == format(fc_dates[x], "%m-%d"))
        clima_idx <- clima[[idx]]
        if (!is.null(p)) {clima_idx <- mask_crop_subset(clima_idx, p)}
        clima_res <- raster::resample(clima_idx, fc[[1]], method = "ngb")
        raster::calc(clima_res, mean)
      }

      if (all(fc_dates %in% clima_dates)){
        cmean <- raster::stack(lapply(seq_along(fc_dates), subset_mean))

        # Total number of non-NA cells
        n_total <- sum(as.vector(!is.na(fc[[1]])))

        fc_above <- fc > cmean
        # Calculate percentages
        fc_df <- data.frame(perc = raster::cellStats(fc_above, sum) / n_total * 100)
      } else {
        fc_df <- data.frame(perc = rep(NA, length(fc_dates)))
      }

    }

    # Format table
    fc_df$step_date <- fc_dates
    names(fc_df)[1] <- i
    row.names(fc_df) <- NULL

    # Append to list
    if (i == 1) {
      df <- fc_df
    }else{
      df <- merge(df, fc_df, by = "step_date", all = TRUE)
    }

  }

  df$step_index <- 1:dim(df)[1]

  # Add placeholder for observation data to data.frame
  df$obs <- NA

  if (!is.null(obs)){

    message("Processing observations")

    obs_dates <- raster::getZ(obs)

    if (is.null(raster::intersect(raster::extent(obs), raster::extent(p)))) {
      obs <- raster::rotate(obs)
    }

    if (!is.null(p)) {
      obs <-  mask_crop_subset(r = obs, p = p)
    }

    # Extract time series
    frp_ts <- as.numeric(raster::cellStats(obs, sum))

    # Add observation data to data.frame
    df$obs[1:length(frp_ts)] <- frp_ts

  }

  # reshape the data.frame with forecast values
  df_reshaped <- reshape2::melt(df,
                                id.vars = c("step_date", "step_index", "obs"))

  df_reshaped$variable <- as.numeric(df_reshaped$variable)
  names(df_reshaped)[names(df_reshaped) == "variable"] <- "fc_index"
  if (type == "clima") {
    df_reshaped$clima <- TRUE
  }else{
    df_reshaped$clima <- FALSE
  }

  df_reshaped$fc_date <- fc_date[df_reshaped$fc_index]

  # Re-order columns
  df_ordered <- df_reshaped[, c("fc_date", "fc_index",
                                "step_date", "step_index",
                                "value", "obs", "clima")]

  return(df_ordered)

}

#' @title plot_chiclet_chart
#'
#' @description Generate a plot of deterministic forecasts compared
#' with daily climatology.
#'
#' @param df data.frame obtained from \code{make_chiclet_chart()}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- make_chiclet_chart(input_dir = "forecast",
#'                               p = fireBBOX,
#'                               event_dates = event_dates,
#'                               obs = "CAMS_frpfire.nc")
#'   plot_chiclet_chart(df)
#' }
#'

plot_chiclet_chart <- function(df){

  if (df$clima[1] == TRUE){
    legend_title <- "% of pixels\nexceeding\nmean climatology"
    limits = c(0, 100)
  }else{
    legend_title <- ""
    limits = NULL
  }

  chiclet <- ggplot(df) +
    geom_tile(aes_string(x = "step_index", y = "fc_index", fill = "value"),
              colour = "white") +
    scale_fill_distiller(palette = "Spectral",
                         na.value = NA,
                         limits = limits,
                         name = legend_title) +
    theme_bw() + labs(x = "Days", y = "Forecast date") +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5))

  if (any(!is.na(df$obs))) {

    min_new_range <- min(df$fc_index)
    max_new_range <- max(df$fc_index)
    min_old_range <- min(df$obs, na.rm = TRUE)
    max_old_range <- max(df$obs, na.rm = TRUE)

    df$obs_rescaled <- (max_new_range - min_new_range) * (df$obs - min_old_range) / (max_old_range - min_old_range) + min_new_range

    chiclet <- chiclet +
      geom_line(data = df,
                aes_string(x = "step_index", y = "obs_rescaled"),
                linetype = 2, size = 0.75,
                col = "#47494c") +
      scale_y_continuous(sec.axis = sec_axis(~ scales::rescale(.,
                                                               to = range(df$fc_index)),
                                             name = "Fire radiative power [Wm-2]"),
                         expand = c(0, 0),
                         breaks = unique(df$fc_index),
                         labels = unique(df$fc_date)[unique(df$fc_index)])
  }else{
    chiclet <- chiclet +
      scale_y_continuous(expand = c(0, 0),
                         breaks = unique(df$fc_index),
                         labels = unique(df$fc_date)[unique(df$fc_index)])
  }

  if (length(unique(df$step_date)) <= 31) {
    chiclet <- chiclet +
      scale_x_continuous(expand = c(0, 0),
                         breaks = unique(df$step_index),
                         labels = as.character(unique(df$step_date))) +
      labs(x = "Valid time")
  }

  return(chiclet)

}
