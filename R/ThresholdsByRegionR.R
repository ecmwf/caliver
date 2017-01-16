#' Validate thresholds
#'
#' @description This function generate a data.frame to compare (and validate) observed burned cells and a fire danger threshold.
#'
#' @param obsMerged is the nc file name (or path) all the observations are saved, by default this is the file BurnedArea.nc the working directory. Observations can be obtained using the function \code{getBurnedAreas} and then merge them using the function \code{mergetime}.
#' @param reaMerged is the nc file name (or path) all the re-analysis are saved, by default this is the file FWI.nc the working directory. This can be generated using the function \code{mergetime}.
#' @param region string with the name of the region of interest
#' @param varnames string with the name of the fire index
#' @param MinBurnedArea Minimum percentage of area burned to consider a cell affected by fire. This is a numeric value in the range [1, 100], by default equal to 20 (20\%).
#' @param probs percentile corresponding to the threshold to validate. This is an integer in the range [1, 100], by default equal to 99.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library("caliver")
#'   setwd("/var/tmp/moc0/geff/")
#'   ThresholdsByRegion(obsMerged = "BurnedArea.nc", 
#'                      reaMerged = "FWI.nc",
#'                      region = "EURO", varnames = "FWI", 
#'                      MinBurnedArea = 20, probs = 99)
#' }
#'

ThresholdsByRegion <- function(obsMerged, reaMerged, region = "EURO",
                               varnames = "FWI", MinBurnedArea = 20, 
                               probs = 99){
  
  for (varname in varnames){
    
    i <- which(varnames %in% varname)
    
    for (thresh in MinBurnedArea){
      
      for (prob in probs){
        
        # READ IN THE RE-ANALYSIS DATA (RasterLayer)
        rea <- getGriddedCDF(ncfile = reaMerged[i], 
                             probs = prob, 
                             region = region,
                             mask = "fuel_model")
        
        if (varname == varnames[1] & thresh == MinBurnedArea[1] & 
            prob == probs[1]){
          
          # FIND CELLS WHERE THERE HAS BEEN A FIRE AFFECTING THRESH% OF THE AREA
          # Note this tasks are only performed once but take 75% of the time!
          
          # 1. Generate a mask of the region of interest
          regMASK <- regionalMask(region)
          
          # 2. Read in the observation
          # Because the data comes from a multilayer single file we generate a 
          # brick (we use stack if layers derive from different files)
          # Using brick over stack should result in better performances
          # (i.e. shorter processing time)
          obs <- raster::brick(obsMerged, progress = "text")
          
          # 3. Crop the observation brick using the mask
          message("Crop observations over region of interest")
          obsRegion <- raster::crop(obs, regMASK, progress = "text")
          
          # 4. Resample the cropped observation brick based on reanalysis data
          message("Resample observations using fire index probability map")
          obsResampled <- raster::resample(obsRegion, rea, 
                                           method = "ngb", progress = "text")
          
          # 5. Calculate which cells were burned
          message("Calculate burned cells")
          burned <- raster::calc(x = obsResampled,
                                 fun = function(x){ifelse(any(x >= thresh/100),
                                                          yes = 1, no = 0)},
                                    progress = "text")
          # NOTE: this operation can generate NAs due to masking of fuel_model!
          
          # Compare burned cells versus fire index values in a data frame
          burnedDF <- data.frame(Burned = as.vector(burned))
          
        }
        
        # Compare burned cells versus fire index values in a data frame
        burnedDF <- cbind(burnedDF, as.vector(rea))
        names(burnedDF)[dim(burnedDF)[2]] <- paste0(varname, "_", 
                                                    prob, "_", thresh)
        
      }
      
    }
    
  }

  # Remove NAs
  # DF <- burnedDF[complete.cases(burnedDF),]

  return(burnedDF)

}
