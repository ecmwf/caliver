#' @title get_fire_danger_levels
#'
#' @description This function calculates the danger levels
#' (VeryLow-Low-Moderate-High-VeryHigh-Extreme) for a given country.
#'
#' @param fire_index RasterBrick containing the fire index to calculate the
#' thresholds for. Please note that names(fire_index) should contain dates.
#' @param ndays Number of days per year in which a fire is expected to occur.
#' By default this is 4 days.
#'
#' @return A numeric vector listing the thresholds.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#'   # Generate dummy brick
#'   set.seed(0)
#'   r <- raster(nrows = 2, ncols = 2,
#'               xmn = 0, xmx = 360, ymn = -90, ymx = 90, vals = 30)
#'   # Simulate a long reanalysis record
#'   b <- raster::brick(lapply(1:(365 * 3),
#'        function(i) raster::setValues(r, runif(n = raster::ncell(r),
#'                                               min = 0, max = 100))))
#'   names(b) <- seq.Date(from = as.Date("1993-01-01"),
#'                        to = as.Date("1995-12-31"),
#'                        by = "day")
#'   # Generate danger levels
#'   get_fire_danger_levels(fire_index = b)
#'
#' }
#'

get_fire_danger_levels <- function(fire_index, ndays = 4){

  if (all(is.na(as.vector(fire_index)))) {

    message("The raster brick only contains NAs")
    return(rep(NA, 6))

  } else {

    message("Calculating thresholds of danger levels")
    # Calculate extreme yearly danger
    years <- substr(x = names(fire_index), start = 2, stop = 5)

    # Calculate percentile related to the above assumption
    extreme_percentile <- floor(x = (1 - ndays / 365) * 100) / 100
    extreme_value <- c()

    for (fire_year in unique(years)) {

      year_idx <- which(years == fire_year)
      sub_fwi <- raster::subset(fire_index, year_idx)
      idx_year <- stats::quantile(na.omit(as.vector(sub_fwi)), extreme_percentile)
      extreme_value <- c(extreme_value, as.numeric(idx_year))

    }

    # Transform FWI threshold into Intensity (I)
    # see formula 31 and 32 in
    # https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19927.pdf
    f <- function(i_component_0, extreme_danger = median(extreme_value)) {

      log(0.289 * i_component_0) - 0.980 * (log(extreme_danger)) ^ 1.546

    }

    # Inspect f: curve(f, from = -10000, to = 1000000); abline(h = 0, lty = 3)
    ff <- try(uniroot(f = f, interval = c(0.1, 100000000000000)), silent = TRUE)
    if (class(ff) != "try-error") {

      i_component <- ff$root
      a <- i_component ^ (1 / 5)

      # We want to get 5 danger classes
      thresholds <- c()
      for (i in 1:5) {

        # Transform back into FWI
        thresholds[i] <- round(exp(1.013 * (log(0.289 * a ^ i)) ^ 0.647), 0)
        # If threshold is NA, return 0!
        if (is.na(thresholds[i])) thresholds[i] <- 0

      }

    } else {

      message("Thresholds are NA because no root was found!")
      thresholds <- rep(NA, 5)

    }

    return(thresholds)

  }

}
