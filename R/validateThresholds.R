#' Validate thresholds
#'
#' @description This function generate a data.frame to compare (and validate) observed burned cells and a fire danger threshold.
#'
#' @param obsMerged is the nc file name (or path) all the observations are saved, by default this is the file BurnedArea.nc the working directory. Observations can be obtained using the function \code{getBurnedAreas} and then merge them using the function \code{mergetime}.
#' @param reaMerged is the nc file name (or path) all the re-analysis are saved, by default this is the file FWI.nc the working directory. This can be generated using the function \code{mergetime}.
#' @param varname string with the name of the fire index
#' @param thresholdBurntArea Minimum percentage of area burned to consider a cell affected by fire. This is a numeric value in the range [0, 1], by default equal to 0.20 (20\%).
#' @param prob percentile corresponding to the threshold to validate. This is an integer in the range [1, 100], by default equal to 99.
#' @param region string with the name of the region of interest
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library("caliver")
#'   setwd("/var/tmp/moc0/geff/")
#'   validateThresholds(obsMerged = "BurnedArea.nc", reaMerged = "FWI.nc",
#'                      varname = "FWI", thresholdBurntArea = 20,
#'                      prob = 99, region = "EURO")
#' }
#'

validateThresholds <- function(obsMerged, reaMerged, varname = "FWI",
                               thresholdBurntArea = 20, prob = 99,
                               region = "EURO"){

  # READ IN THE RE-ANALYSIS DATA (RasterLayer)
  rea <- getGriddedCDF(ncfile = reaMerged, probs = prob, region = region,
                       mask = "fuel_model")

  # FIND CELLS WHERE THERE HAS BEEN A FIRE AFFECTING AT LEAST 20% OF THE AREA

  # 1. Generate a mask of the region of interest
  regMASK <- regionalMask(region)
  
  # 2. Read in the observation
  # Because the data comes from a multilayer single file we generate a brick
  # (we use stack if layers derive from different files)
  # Using brick over stack should result in better performances
  # (i.e. shorter processing time)
  obs <- raster::brick(obsMerged, progress = "text")

  # 3. Crop the observation brick using the mask
  message("Crop observations over region of interest")
  obsRegion <- raster::crop(obs, regMASK, progress = "text")

  # 4. Resample the cropped observation brick based on the reanalysis attributes
  message("Resample observations using fire index probability map")
  obsResampled <- raster::resample(obsRegion, rea, 
                                   method = "ngb", progress = "text")

  # 5. Calculate which cells were burned
  message("Calculate burned cells")
  burnedMAP <- raster::calc(x = obsResampled,
                            fun = function(x){ifelse(test = (any(x >= 0.20)),
                                                     yes = TRUE, no = FALSE)},
                            progress = "text")
  # NOTE: the above operation can generate NAs due to masking of fuel_model!

  # Compare burned cells versus fire index values in a data frame
  burnedDF <- data.frame(Burned = as.vector(burnedMAP))
  burnedDF$Index <- as.vector(rea)
  names(burnedDF)[2] <- varname
  
  # Remove NAs
  DF <- burnedDF[complete.cases(burnedDF),]

  return(DF)

}
