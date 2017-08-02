#' @title Validate Fire Danger Levels
#'
#' @description This function compares observed and modelled fire data and
#' return a contingency table summarising the hit rates, false alarms, misses
#' and correct negatives. The validation can be done using various thresholds
#' and input data.
#'
#' @param fireIndex RasterBrick containing the fire index (only one variable)
#' @param observation RasterBrick containing the observation (only one variable)
#' @param fireThr threshold to use to select relevant fire indices
#' @param obsThr threshold to use to select relevant observations
#'
#' @return A contingency table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   setwd("/scratch/mo/moc0/fire/")
#'
#'   # Load and subset FWI
#'   BurnedAreas <- raster::brick("GFED4_BurnedAreas/BurnedArea.grd")
#'   names(BurnedAreas) <- seq.Date(from = as.Date("2003-01-01"),
#'                                  to = as.Date("2015-12-31"), by = "day")
#'   FWI <- raster::brick('GEFF/reanalysis/FWI_1980-2016.nc')
#'
#'   # Generate the contingency table
#'   ct <- ValidateFireDangerLevels(fireIndex = FWI, 
#'                                  observation = BurnedAreas,
#'                                  fireThr = 10, obsThr = 50)
#'
#' }
#'

ValidateFireDangerLevels <- function(fireIndex, observation, fireThr, obsThr){
  
  if (!("RasterBrick" %in% class(fireIndex)) &
      !("RasterStack" %in% class(fireIndex)) &
      !("RasterLayer" %in% class(fireIndex))) {

    stop('Error: the fireIndex can only be a raster brick/stack')

  }

  if (!("RasterBrick" %in% class(observation)) &
      !("RasterStack" %in% class(observation)) &
      !("RasterLayer" %in% class(observation))) {

    stop('Error: the observation can only be a raster brick/stack')

  }

  # RasterLayer
  if ("RasterLayer" %in% class(fireIndex)) {

    FWIbrick <- fireIndex

  }

  if ("RasterLayer" %in% class(observation)) {

    OBSbrick <- observation

  }

  # RasterStack
  if ("RasterStack" %in% class(fireIndex)) {

    message('Convert stack of fire indices into a raster brick')
    FWIbrick <- raster::brick(fireIndex, progress = 'text')

  }

  if ("RasterStack" %in% class(observation)) {

    message('Convert stack of observations into a raster brick')
    OBSbrick <- raster::brick(observation, progress = 'text')

  }

  # RasterBrick
  if ("RasterBrick" %in% class(fireIndex)) {

    FWIbrick <- fireIndex

  }

  if ("RasterBrick" %in% class(observation)) {

    OBSbrick <- observation

  }

  # Aggregate/Resample observations to match the resolution of the fire index
  fact <- round(dim(OBSbrick)[1:2]/dim(FWIbrick)[1:2], 0)
  message("Aggregate observations to match the resolution of the fire index")
  BurnedAreasA <- raster::aggregate(OBSbrick, fact, fun = sum,
                                    progress = 'text')
  message("Resample observations to match the resolution of the fire index")
  BAresampled <- raster::resample(BurnedAreasA, FWIbrick,
                                  method = "bilinear", progress = 'text')

  # Select only period in common
  fwiIndex <- which(names(FWIbrick) %in% names(BAresampled))
  obsIndex <- which(names(BAresampled) %in% names(FWIbrick))
  FWIseasonal <- raster::subset(FWIbrick, fwiIndex)
  OBSseasonal <- raster::subset(BAresampled, obsIndex)

  message("Calculating contingency table")
  # Transform BurnedAreas to binary (is BurnedAreas > 50 hectares?)
  BA <- as.vector(OBSseasonal)
  BAlogic <- ifelse(test = (BA >= obsThr), yes = TRUE, no = FALSE)

  # Transform FWI to binary (Is FWI > high danger level?)
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
