#' @title Define Risk Threshold
#'
#' @description This function calculates the danger levels 
#' (VeryLow-Low-Moderate-High-VeryHigh-Extreme) for a given country.
#'
#' @param fireIndex RasterBrick containing the fire index to calculate the
#' thresholds for. Please note that names(fireIndex) should contain dates.
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
#'   seasons <- getFireSeason(dataDates, emisphere = "north")
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
#'   countryThr <- FireDangerLevels(fireIndex = FWIseasonal)
#'
#' }
#'

FireDangerLevels <- function(fireIndex){

  if (is.na(sum(as.vector(raster::calc(x = fireIndex, fun = sum)),
                na.rm = T))) {

    message("The raster brick only contains NAs")
    return(rep(NA, 6))

  } else {

    message("Calculating thresholds of danger levels")
    # Calculate extreme yearly danger
    years <- substr(x = names(fireIndex), start = 2, stop = 5)

    # Number of days per year = extreme yearly danger (assumption)
    ndays <- 4

    # Calculate percentile related to the above assumption
    extremePercentile <- floor(x = (1 - ndays / 365) * 100) / 100
    extremeValues <- c()

    for (FireYear in unique(years)) {

      # print(FireYear)
      yearIDX <- which(years == FireYear)
      subFWI <- raster::subset(fireIndex, yearIDX)
      IDXyear <- quantile(na.omit(as.vector(subFWI)), extremePercentile)
      extremeValues <- c(extremeValues, as.numeric(IDXyear))

    }

    # Transform FWI threshold into Intensity (I)
    # see formula 31 and 32 in 
    # http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19927.pdf
    f <- function (Icomponent0, extremeDanger = median(extremeValues)) {

      log(0.289 * Icomponent0) - 0.980 * (log(extremeDanger)) ^ 1.546

    }

    # Inspect f: curve(f, from = -10000, to = 1000000); abline(h = 0, lty = 3)
    ff <- try(uniroot(f = f, interval = c(0.1, 100000000000000)), silent = TRUE)
    if (class(ff) != "try-error") {

      Icomponent <- ff$root
      a <- Icomponent ^ (1 / 5)

      # We want to get 5 danger classes
      nClasses <- 5
      thresholds <- c()
      for (i in 1:nClasses) {

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
