#' @title Define Risk Threshold
#'
#' @description This function calculates the danger levels
#' (VeryLow-Low-Moderate-High-VeryHigh-Extreme) for a given country.
#'
#' @param fire_index RasterBrick containing the fire index to calculate the
#' thresholds for. Please note that names(fire_index) should contain dates.
#'
#' @return A numeric vector listing the thresholds.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   # Define period for Reanalysis
#'   dataDates <- seq.Date(from = as.Date(strptime(paste("1980", 1),
#'                                                 format="%Y %j")),
#'                         to = as.Date(strptime(paste("2016", 366),
#'                                                 format="%Y %j")),
#'                         by = "day")
#'
#'   # Create an index of fire season dates
#'   seasons <- get_fire_season(dataDates, emisphere = "north")
#'   fireSeasonIndex <- which(seasons == TRUE)
#'
#'   r <- raster::brick('FWI_1980-2016.nc')
#'   p <- raster::getData(name = "GADM", country = "Italy", level = 0)
#'   maskcrop(r, p, mask = TRUE, crop = TRUE)
#'
#'   # Mask and crop over country
#'   FWIcountry <- maskcrop(FWI, country, mask = TRUE, crop = TRUE)
#'
#'   # Subset based on fire season
#'   FWIseasonal <- raster::subset(FWIcountry, fireSeasonIndex)
#'
#'   # Generate thresholds for the country of interest
#'   countryThr <- get_fire_danger_levels(fire_index = FWIseasonal)
#'
#' }
#'

get_fire_danger_levels <- function(fire_index){

  if (is.na(sum(as.vector(raster::calc(x = fire_index, fun = sum)),
                na.rm = T))) {

    message("The raster brick only contains NAs")
    return(rep(NA, 6))

  } else {

    message("Calculating thresholds of danger levels")
    # Calculate extreme yearly danger
    years <- substr(x = names(fire_index), start = 2, stop = 5)

    # Number of days per year = extreme yearly danger (assumption)
    ndays <- 4

    # Calculate percentile related to the above assumption
    extreme_percentile <- floor(x = (1 - ndays / 365) * 100) / 100
    extreme_value <- c()

    for (FireYear in unique(years)) {

      year_idx <- which(years == FireYear)
      sub_fwi <- raster::subset(fire_index, year_idx)
      idx_year <- quantile(na.omit(as.vector(sub_fwi)), extreme_percentile)
      extreme_value <- c(extreme_value, as.numeric(idx_year))

    }

    # Transform FWI threshold into Intensity (I)
    # see formula 31 and 32 in
    # http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19927.pdf
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
