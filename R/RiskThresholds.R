#' Define Risk Threshold
#'
#' @description This function calculates the risk threshold given a vector of probabilities corresponding to Low-Medium-High-Extreme risk.
#'
#' @param probMaps is the RasterStack where all the pre-calculated probability maps are stored
#' 
#' @return A tibble (data.frame) listing thresholds based on regions (columns) and level of risk (percentile on the rows).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   thresholdFWI <- RiskThresholds(probMaps)
#' }
#'

RiskThresholds <- function(probMaps){
  
  # Set regions' names
  regions <- c("GLOB", "BONA", "TENA", "CEAM", "NHSA", "SHSA", "EURO", 
               "MIDE", "NHAF", "SHAF", "BOAS", "CEAS", "SEAS", "EQAS", "AUST")
  
  # Get probabilities from layer names
  layerNames <- names(probMaps)
  probs <- as.numeric(as.character(gsub("[^0-9]", "", layerNames)))
  
  # CDF curves can be created by averaging the percentiles over a given area
  thresholdDF <- data.frame(matrix(NA, 
                           ncol = length(regions) + 2, 
                           nrow = length(probs)))
  names(thresholdDF) <- c("Percentile", "Index", regions)
  thresholdDF$Percentile <- probs
  thresholdDF$Index <- gsub('[0-9]+', '', layerNames)
  
  for (region in regions){
    
    j <- which(names(thresholdDF) == region) # counter over the columns
    
    # Mask the percentile maps using regional masks
    for (prob in probs){
      
      i <- which(thresholdDF$Percentile == prob)
      globalMap <- probMaps[[i]]
      
      if (region == "GLOB"){  
        maskedMap <- globalMap
      }else{
        regionMap <- regionalMask(region)
        regionMap <- raster::resample(regionMap, globalMap, method = "ngb")
        maskedMap <- raster::mask(globalMap, regionMap)
      }
      
      thresholdDF[i, j] <- round(raster::cellStats(x = maskedMap, stat = 'mean'), 3)
      
    }
    
  }
  
  return(tibble::as_tibble(thresholdDF))
  
}
