#' Define Risk Threshold
#'
#' @description This function calculates the danger levels (VeryLow-Low-Moderate-High-VeryHigh-Extreme) for a given country.
#'
#' @param fireIndex fire index to calculate the thresholds for (default is fwi = fire weather index)
#' @param countryName string describing the country name.
#' @param baseDir this is the directory where the reanalysis data are saved.
#' @param dataDates dates of the reanalysis data to use.
#' @param fireSeasonIndex vector of indices (same length as dataDates) with TRUE if the date falls in the fire season, FALSE otherwise.
#' @param returnExtremeValues logical value, by default set to FALSE. If this is set to TRUE, it returns the yearly extreme values (useful to calculate trends).
#' 
#' @return A numeric vector listing the thresholds.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#'   baseDir <- "."
#'   countryName <- "Spain"
#'   
#'   # Define period for Reanalysis
#'   dataDates <- seq.Date(from = as.Date(strptime(paste("1980", 1),
#'                                                 format="%Y %j")),
#'                         to = as.Date(strptime(paste("2016", 366),
#'                                                 format="%Y %j")),
#'                         by = "day")
#'                         
#'   # Create an index of fire season dates
#'   seasons <- getFireSeason(dataDates, 
#'                            FSS = NULL, FSE = NULL, emisphere = "north")
#'   fireSeasonIndex <- which(seasons == TRUE)
#'   
#'   # Calculate the thresholds
#'   thresholdFWI <- DangerLevels(fireIndex = "fwi", countryName = "Spain",
#'                           baseDir, dataDates, fireSeasonIndex)
#' }
#'

DangerLevels <- function(fireIndex = "fwi", 
                         countryName = "Spain",
                         baseDir = getwd(),
                         dataDates,
                         fireSeasonIndex,
                         returnExtremeValues = FALSE){
  
  filename <- file.path(baseDir, paste0(fireIndex, "_rotated.nc"))
  if (!file.exists(filename)){
    
    message("Merging reanalysis data")
    
    # Merge (rotated) files into a single NetCDF
    mergetime(dirs = paste0(baseDir, "/rotated/", fireIndex),
              varname = fireIndex,
              recursive = FALSE,
              outFile = filename,
              outDir = baseDir)
    
  }
  
  filename <- file.path(baseDir, paste0(fireIndex, "_rotated_masked.grd"))
  if (!file.exists(filename)){
    
    # Load single NetCDF into a single RasterBrick
    IDX <- raster::brick(file.path(baseDir, paste0(fireIndex, "_rotated.nc")))
    
    message("Masking reanalysis data using JRC's fuel map")
    
    # Mask using JRC fuel_map
    IDX_masked <- fuelmodelMask(IDX)
    raster::writeRaster(IDX_masked, 
                        filename=filename, 
                        bandorder='BIL', overwrite=TRUE, progress = 'text')
    rm(IDX)
    
  }else{
    
    IDX_masked <- raster::brick(filename)
    
  }
  
  filename <- file.path(baseDir, 
                        paste0(fireIndex, "_rotated_masked_", 
                               countryName, ".grd"))
  if (!file.exists(filename)){
    
    message("Masking and cropping reanalysis data over given country")
    
    # Mask & Crop over the country of interest
    IDXcountry <- countryMaskCrop(IDX_masked, countryName)
    raster::writeRaster(IDXcountry, 
                        filename = filename, 
                        bandorder='BIL', overwrite=TRUE, progress = 'text')
    
  }else{
    
    IDXcountry <- raster::brick(filename)
    
  }
  
  rm(IDX_masked)
  
  # Identify & remove non-prone areas
  # (do not apply this, IF there is not much difference!)
  # proneAreas <- raster::calc(x = IDXspain, fun = sum, progress = "text")
  # proneAreas[proneAreas == 0] <- NA
  # IDXspainProne <- raster::mask(IDXspain, proneAreas) # no changes!
  # w <- array(EUROIDXsummerProne)
  # w <- w[!is.na(w)]
  # plot(density(w), main = "IDX (removed non-prone areas) - summer - Europe")
  # round(quantile(w, c(0.50, 0.70, 0.80, 0.90, 0.99)), 1)
  
  # Identify & remove stationary areas 
  # (do not apply this, IF there is not much variability!)
  # s1 <- raster::stack() 
  # for (myYear in 1980:2016){
  #   print(myYear)
  #   idx <- which(years[fireSeasonIndex] == myYear)
  #   IDXIdx <- raster::subset(IDXspainProne, idx)
  #   s1 <- raster::stack(s1, raster::calc(x = IDXIdx, fun = sd)) 
  # }
  # statAreas <- raster::calc(x = s1, fun = sd)
  # hist(as.vector(statAreas))
  
  message("Calculating thresholds of danger levels")
  
  # Calculate extreme yearly danger
  years <- lubridate::year(dataDates[fireSeasonIndex])
  
  # Number of days per year = extreme yearly danger (assumption)
  ndays <- 4 
  
  # Calculate percentile related to the above assumption
  extremePercentile <- floor(x = (1-ndays/365)*100)/100
  extremeValues <- c()
  
  for (FireYear in unique(lubridate::year(dataDates))){
    print(FireYear)
    yearIDX <- which(years == FireYear) 
    sub1 <- raster::subset(IDXcountry, fireSeasonIndex)
    sub2 <- raster::subset(sub1, yearIDX)
    IDXyear <- quantile(na.omit(as.vector(sub2)), extremePercentile)
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
  
  if (returnExtremeValues == TRUE){
    
    return(list("thresholds" = thresholds, "extremeValues" = extremeValues))
    
  }else{
    
    return(thresholds)
    
  }
  
}
