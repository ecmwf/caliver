#' @title Validate Fire Danger Levels
#'
#' @description This function compares observed and modelled fire data and return a contingency table summarising the hit rates, false alarms, misses and correct negatives. The validation can be done using various thresholds and input data.
#'
#' @param fireIndex RasterBrick containing the fire index (only one variable)
#' @param observation RasterBrick containing the observation (only one variable)
#' @param fireThr threshold to use to select relevant fire indices
#' @param obsThr threshold to use to select relevant observations
#' @param areaOfInterest Raster* object or a Spatial* object
#' 
#' @return A contingency table
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' 
#'   setwd("/scratch/mo/moc0/fire/")
#' 
#'   # Load and subset FWI
#'   BurnedAreas <- raster::brick("GFED4_BurnedAreas/BurnedArea.grd")
#'   names(BurnedAreas) <- seq.Date(from = as.Date("2003-01-01"),
#'                                  to = as.Date("2015-12-31"), by = "day")
#'   FWI <- raster::brick('GEFF/reanalysis/FWI_1980-2016.nc')
#'   
#'   # Get area of interest: Europe
#'   Europe <- getGFED4(varname = 'BasisRegions', region = 'EURO')
#'   
#'   # Generate the contingency table
#'   ct <- ValidateFireDangerLevels(fireIndex = FWI, 
#'                                  observation = BurnedAreas,
#'                                  fireThr = 10, obsThr = 50, 
#'                                  areaOfInterest = Europe)
#'                           
#' }
#'

ValidateFireDangerLevels <- function(fireIndex, observation, fireThr, obsThr,
                                     areaOfInterest = NULL){
  
  if (!("RasterBrick" %in% class(fireIndex)) & 
      !("RasterStack" %in% class(fireIndex))){
    stop('Error: the fireIndex can only be a raster brick/stack')
  }
  
  if (!("RasterBrick" %in% class(observation)) & 
      !("RasterStack" %in% class(observation))){
    stop('Error: the observation can only be a raster brick/stack')
  }
  
  if ("RasterStack" %in% class(fireIndex)){
    message('Convert stack of fire indices into a raster brick')
    FWIbrick <- raster::brick(fireIndex, progress = 'text')
  }
  
  if ("RasterStack" %in% class(observation)){
    message('Convert stack of observations into a raster brick')
    OBSbrick <- raster::brick(observation, progress = 'text')
  }
  
  if ("RasterBrick" %in% class(fireIndex)){
    FWIbrick <- fireIndex
  }
  
  if ("RasterBrick" %in% class(observation)){
    OBSbrick <- observation
  }
  
  if (!is.null(areaOfInterest)){
    
    # Polygonise the areas of interest
    if ('RasterLayer' %in% class(areaOfInterest)){
      areaOfInterest <- raster::rasterToPolygons(x = areaOfInterest)
    }
    
    message('Masking fire index over area of interest')
    FWImasked <- raster::mask(FWIbrick, areaOfInterest, progress = 'text')
    
    message('Cropping fire index over area of interest')
    FWIareal <- raster::crop(FWImasked, areaOfInterest, progress = 'text')
    
    message('Masking observations over area of interest')
    OBSmasked <- raster::mask(OBSbrick, areaOfInterest, progress = 'text')
    
    message('Cropping observations over area of interest')
    OBSareal <- raster::crop(OBSmasked, areaOfInterest, progress = 'text')
    
  }else{
    
    # Keep the global extent
    FWIareal <- FWIbrick
    OBSareal <- OBSbrick
    
  }
  
  # Aggregate/Resample observations to match the resolution of the fire index
  fact <- round(dim(OBSareal)[1:2] / dim(FWIareal)[1:2], 0)
  message("Aggregate observations to match the resolution of the fire index")
  BurnedAreasA <- raster::aggregate(OBSareal, fact, fun = sum, 
                                    progress = 'text')
  message("Resample observations to match the resolution of the fire index")
  BAresampled <- raster::resample(BurnedAreasA, FWIareal, 
                                  method = "bilinear", progress = 'text')
  
  # Select only period in common
  fwiIndex <- which(names(FWIareal) %in% names(BAresampled))
  obsIndex <- which(names(BAresampled) %in% names(FWIareal))
  FWIseasonal <- raster::subset(FWIareal, fwiIndex)
  OBSseasonal <- raster::subset(BAresampled, obsIndex)
  # check: dim(FWIseasonal) == dim(OBSseasonal)
  
#   if (is.null(fireSeasonIndex)){
#     message("Subsetting FWI over the fire season")
#     FWIseasonal <- raster::subset(FWIareal, fireSeasonIndex)
#     OBSseasonal <- raster::subset(OBSareal, fireSeasonIndex)
#   }else{
#     FWIseasonal <- FWIareal
#     OBSseasonal <- OBSareal
#   }
  
  message("Calculating contingency table")
  # Transform BurnedAreas to binary (is BurnedAreas for AreaOfInterest > 50 hectares?)
  BA <- as.vector(OBSseasonal)
  BAlogic <- ifelse(test = (BA >= obsThr), yes = TRUE, no = FALSE)
  
  # Transform FWI to binary (Is FWI for AreaOfInterest > high danger level?)
  FWI <- as.vector(FWIseasonal)
  FWIlogic <- ifelse(test = (FWI >= fireThr), yes = TRUE, no = FALSE)
  
  # Contingency table
  # x <- data.frame(table(FWIlogic, BAlogic))
  # round(prop.table(table(FWIlogic, BAlogic)), 3)
  # hits <- x$Freq[which(x$FWIlogic == TRUE & x$BAlogic == TRUE)]
  # misses <- x$Freq[which(x$FWIlogic == FALSE & x$BAlogic == TRUE)]
  # falsealarms <- x$Freq[which(x$FWIlogic == TRUE & x$BAlogic == FALSE)]
  # correctneg <- x$Freq[which(x$FWIlogic == FALSE & x$BAlogic == FALSE)]
  # POD <- hits/(hits+misses)               # 66%
  # FAR <- falsealarms/(hits+falsealarms)   # 98%
  # FBI <- (hits+falsealarms)/(hits+misses) # 38%
  # TS <- hits/(hits+misses+falsealarms)    # 1.7%
  
  return(table(FWIlogic, BAlogic))
  
}
