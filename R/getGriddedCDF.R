#' Calculate CDF and percentiles
#'
#' @description This function calculates the CDF at each point of a grid
#'
#' @param ncfile is the name of the file(path) to read
#' @param probs numeric vector of probabilities with values in [0,1] listing which percentiles should be calculated
#' @param mask string identifying the name of the mask. By default it is set to NULL, which means no mask is applied. The only mask implemented at the moment is the "fuel_model" provided by JRC (containing non-vegetated areas).
#' @param region string identifying the name of the region of interest. By default it is set to provide global coverage ("GLOB") but it can also be used to focus on the reagion listed in \code{regionalMask}.
#' 
#' @return list containing all the maps of fwi percentiles
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   ncfile <- "/var/tmp/moc0/forestfire/outfile.nc"
#'   x <- getGriddedCDF(ncfile)
#' }
#'

getGriddedCDF <- function(ncfile, 
                          probs = c(50, 75, 90, 99),
                          mask = NULL,
                          region = "GLOB"){
  
  outList <- list()
  nameList <- c()
  fuelmodel <- NULL
  
  for (prob in probs){
    
    fileName <- tools::file_path_sans_ext(basename(ncfile))
    
    outFile <- paste0(getwd(), "/", fileName, "_", prob, ".nc")
    
    system(paste0("cdo timpctl,", prob, " ", ncfile, 
                  " -timmin ", ncfile, 
                  " -timmax ", ncfile, " ", outFile))
    
    outList[[which(probs == prob)]] <- raster::raster(outFile)
    
    nameList <- c(nameList, paste0(toupper(fileName), "_", 
                                   prob, "th_percentile"))
    
  }
  
  names(outList) <- nameList
  
  if (!is.null(mask)) {
    
    if (mask == "fuel_model"){
      
      # In this map, water-barren-marsh-Snow and Ice-Urban-Agriculture-NoData 
      # are identified by the codes 21-27. Therefore we assume all the values 
      # from 0 to 20 are valid, while 21 and above should be masked.
      load(system.file("data/fuelmodel.rda", package = "caliver"))
      fuelmodel[fuelmodel > 20] <- NA
      maskedMaps <- raster::mask(raster::stack(outList), fuelmodel)
      
    }else{
      message("The requested mask is not implemented yet, see documentation.")
    }
    
  }else{
    
    maskedMaps <- outList
    
  }
  
  if (region != "GLOB") {
    
    # Mask&Crop the global file to the region's extent
    regmaskedMaps <- raster::mask(maskedMaps, regionalMask(region))
    
    mapExtent <- regionalBBOX(region)
    
    if (mapExtent@xmin > mapExtent@xmax){
      mapExtent@xmin <- 0
      mapExtent@xmax <- 360
    }
    
    croppedMaps <- raster::crop(regmaskedMaps, mapExtent)
    
  }else{
    
    croppedMaps <- maskedMaps
    
  }
  
  return(croppedMaps)
  
}
