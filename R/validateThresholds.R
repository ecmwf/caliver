#' Validate thresholds
#'
#' @description This function allows to validate the thresholds calculated by \code{RiskThresholds}.
#' 
#' @param obsMerged is the nc file name (or path) all the observations are saved, by default this is the file BurnedArea.nc the working directory. Observations can be obtained using the function \code{getBurnedAreas}.
#' @param riskTable is the table with all the thresholds
#' @param varnames string (or vector of strings) with name of the fire index
#' 
#' @return A data.frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library("caliver")
#'   setwd("/var/tmp/moc0/geff/")
#'   RiskTable <- readRDS("RiskTable.rds")
#'   validateThresholds(obsMerged = "BurnedArea.nc", riskTable = RiskTable,
#'                      varname = "FWI", thresholdBurntArea = 20, prob = 99,
#'                      region = "EURO")
#' }
#'

validateThresholds <- function(obsMerged, riskTable, varname = "FWI",
                               thresholdBurntArea = 20, prob = 99,
                               region = "EURO"){
  
  threshold <- as.numeric(riskTable[which(riskTable$Index == varname &
                                            riskTable$Percentile == prob),
                                    which(names(riskTable) == region)])
  
  # Because the data comes from a multilayer single file we generate a brick
  # (we use stack if layers derive from different files)
  # Using brick over stack should result in better performances 
  # (i.e. shorter processing time)
  obs <- raster::brick(obsMerged, progress = 'text')
  regMASK <- regionalMask(region)
  
  message("Crop observations over region of interest")
  obsRegion <- raster::crop(obs, regMASK, progress = 'text')
  
  whichBurned <- raster::calc(obsRegion, 
                              function(x){ifelse(test = (any(x >= 0.20)), 
                                                 yes = TRUE, no = FALSE)},
                              progree = 'text')
  
  # There might be NAs, therefore 
  idxBurned <- which(whichBurned == TRUE)
  idxNotBurned <- which(whichBurned == FALSE)
  
  
  for (varname in varnames){
    
    print(varname)
    
    for (prob in probs){
      
      print(prob)
      
      if (varname == varnames[1] & prob == probs[1]){
        
        ## Define the function to calculate whether each cell has ever burned
        burnedAreaCalc <- function(x) {
          if (all(is.na(x))) {
            y <- NA
          }else{
            if (any(na.omit(x) >= thresholdBurntArea/100)){
              y <- TRUE
            }else{
              y <- FALSE
            }
          }
          return(y)
        }
        burnedRaster <- raster::calc(x = obs, fun = burnedAreaCalc, 
                                     progress='text')
        
        DF <- data.frame(as.vector(burnedRaster), as.vector(idx))
        DFnames <- c("Burned", paste0(varname, prob))
        
      }else{
        
        # Load the fire index file corresponding to the first probability in probs
        idxDefaultFileName <- paste0(toupper(varname), "_", prob, ".nc")
        if(file.exists(idxDefaultFileName)){
          idx <- raster::raster(idxDefaultFileName)
        }else{
          message(paste0("File containing the index ", varname, "_", prob, 
                         " is missing. Generate it using getGriddedCDF"))
        }
        
        DF <- cbind(DF, as.vector(idx))
        DFnames <- c(DFnames, paste0(varname, prob))
        
      }
      
    }
    
  }
  
  names(DF) <- DFnames
  
  return(DF)
  
}
