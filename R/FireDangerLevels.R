#' Define Risk Threshold
#'
#' @description This function calculates the danger levels (VeryLow-Low-Moderate-High-VeryHigh-Extreme) for a given country.
#'
#' @param fireIndex fire index to calculate the thresholds for (default is fwi = fire weather index)
#' @param areaOfInterest Raster* object or a Spatial* object
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
#'   # Get area of interest: Europe
#'   Europe <- getGFED4(varname = 'BasisRegions', region = 'EURO')
#'   
#'   # Load and subset FWI
#'   FWIall <- raster::brick("FWI.nc")
#'   FWI <- raster::subset(FWIall, fireSeasonIndex)
#'   
#'   # Calculate the thresholds
#'   thresholdFWI <- FireDangerLevels(fireIndex = "fwi", countryName = "Spain",
#'                           baseDir, dataDates, fireSeasonIndex)
#' }
#'

FireDangerLevels <- function(fireIndex, areaOfInterest = NULL){
  
  if (!is.null(areaOfInterest)){
    
    # Crop over the areas of interest
    if ('RasterLayer' %in% class(areaOfInterest)){
      areaOfInterest <- raster::rasterToPolygons(x = areaOfInterest)
    }
    
    if ('RasterLayer' %in% class(areaOfInterest)){
      areaOfInterest <- raster::rasterToPolygons(x = areaOfInterest)
    }
    
    message('Masking fire index over area of interest')
    FWImasked <- raster::mask(fireIndex, areaOfInterest, progress = 'text')
    
    message('Cropping fire index over area of interest')
    FWIareal <- raster::crop(FWImasked, areaOfInterest, progress = 'text')
    # This generates a raster brick
    
  }else{
    
    # The original fire index is a raster stack
    message('Convert stack of fire indices into a raster brick')
    FWIareal <- raster::brick(fireIndex, progress = 'text')
    
  }
  
  message("Calculating thresholds of danger levels")
  # Calculate extreme yearly danger
  years <- substr(x = names(FWIareal@data), start = 2, stop = 5)
  
  # Number of days per year = extreme yearly danger (assumption)
  ndays <- 4 
  
  # Calculate percentile related to the above assumption
  extremePercentile <- floor(x = (1-ndays/365)*100)/100
  extremeValues <- c()
  
  for (FireYear in unique(years)){
    # print(FireYear)
    yearIDX <- which(years == FireYear)
    subFWI <- raster::subset(FWIareal, yearIDX)
    IDXyear <- quantile(na.omit(as.vector(subFWI)), extremePercentile)
    extremeValues <- c(extremeValues, as.numeric(IDXyear))
  }
  
  # Transform FWI threshold into Intensity (I)
  # see formula 31 and 32 in http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19927.pdf
  f <- function (Icomponent, extremeDanger = median(extremeValues)){
    
    log(0.289*Icomponent) - 0.980*(log(extremeDanger))^1.546
    
  }
  
  # Inspect f: curve(f, from = 0, to = 1000000); abline(h = 0, lty = 3)
  Icomponent <- uniroot(f = f, interval = c(0, 100000))$root
  a <- Icomponent^(1/5)
  
  # We want to get 5 danger classes
  nClasses <- 5
  thresholds <- c()
  for (i in 1:nClasses){
    # Transform back into FWI
    thresholds[i] <- round(exp(1.013*(log(0.289*a^i))^0.647), 0)
    # If threshold is NA, return 0!
    if (is.na(thresholds[i])) thresholds[i] <- 0
  }
  
  return(thresholds)
  
}
