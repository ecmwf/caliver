#' @title makePercentileRaster
#'
#' @description This function calculates the given percentile at each grid point
#'
#' @param inFilePath is the name of the file(path) to read
#' @param probs numeric vector of probabilities with values in [0,100] listing which percentiles should be calculated
#' @param mask string identifying the name of the mask. By default it is set to "no mask", which means no mask is applied. The only mask implemented at the moment is the "fuelmodel" provided by JRC (containing non-vegetated areas).
#' @param region string identifying the name of the region of interest. By default it is set to provide global coverage (GLOB) but it can also be used to focus on the following 15 regions (see also \code{regionalMask}): 
#' \itemize{
#'   \item{"Global"}{or GLOB}
#'  \item{"Boreal North America"}{or BONA}
#'  \item{"Temperate North America"}{or TENA}
#'  \item{"Central America"}{or CEAM}
#'  \item{"Northern Hemisphere South America"}{or NHSA}
#'  \item{"Southern Hemisphere South America"}{or SHSA}
#'  \item{"Europe"}{or EURO}
#'  \item{"Middle East"}{or MIDE}
#'  \item{"Northern Hemisphere Africa"}{or NHAF}
#'  \item{"Southern Hemisphere Africa"}{or SHAF}
#'  \item{"Boreal Asia"}{or BOAS}
#'  \item{"Central Asia"}{or CEAS}
#'  \item{"Southeast Asia"}{or SEAS}
#'  \item{"Equatorial Asia"}{or EQAS}
#'  \item{"Australia and New Zealand"}{or AUST}}
#' @param outDir is the directory where the output nc files are saved, by default this is a temporary directory.
#' 
#' @return list containing all the maps of fwi percentiles
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   x <- makePercentileRaster(inFilePath = "./outfile.nc",
#'                             probs = c(50, 75, 90, 99),
#'                             mask = "",
#'                             region = "EURO",
#'                             outDir = getwd())
#' }
#'

makePercentileRaster <- function(inFilePath, probs, mask = "", 
                          region = "GLOB", outDir = tempdir()){
  
  if (mask == "fuelmodel"){
    
    # In this map, water-barren-marsh-Snow and Ice-Urban-Agriculture-NoData 
    # are identified by the codes 21-27. Therefore we assume all the values 
    # from 0 to 20 are valid, while 21 and above should be masked.
    fuelmodel <- raster::raster(system.file(file.path("extdata", 
                                                      "clim_fuelmodel.nc"), 
                                            package = "caliver"))
    fuelmodel[fuelmodel > 20] <- NA
    
  }
  
  if (region != "GLOB") {
    
    mapExtent <- regionalBBOX(region)
    
    if (mapExtent@xmin > mapExtent@xmax){
      mapExtent@xmin <- 0
      mapExtent@xmax <- 360
    }
  }
  
  stackedMaps <- raster::stack() 
  
  for (i in 1:length(probs)){
    
    prob <- probs[i]
    
    fileName <- tools::file_path_sans_ext(basename(inFilePath))
    
    outFile <- file.path(outDir, paste0(fileName, "_", prob, ".nc"))
    
    system(paste0("cdo timpctl,", prob, " ", inFilePath, 
                  " -timmin ", inFilePath, 
                  " -timmax ", inFilePath, " ", outFile))
    
    probRaster <- raster::raster(outFile)
    
    if (mask == "fuelmodel"){
      
      if (i == 1) {
        fuelmodel <- raster::resample(fuelmodel, probRaster, method = "ngb")
      }
      
      maskedProbRaster <- raster::mask(probRaster, fuelmodel)
      
    }else{
      
      # message("No vegetation mask has been used.")
      maskedProbRaster <- probRaster
      
    }
    
    if (region != "GLOB") {
      
      regionRaster <- raster::resample(regionalMask(region), 
                                       probRaster, method = "ngb")
      
      # Mask&Crop the global file to the region's extent
      regMaskedProbRaster <- raster::mask(maskedProbRaster, regionRaster)
      croppedMaps <- raster::crop(regMaskedProbRaster, mapExtent)
      
    }else{
      
      croppedMaps <- maskedProbRaster
      
    }
    
    varname <- names(ncdf4::nc_open(inFilePath)$var)
    names(croppedMaps) <- paste0(toupper(varname), prob)
    
    if (length(probs) > 1) {
      
      stackedMaps <- raster::stack(stackedMaps, croppedMaps)
      
    }else{
      
      stackedMaps <- croppedMaps
      
    }
    
  }
  
  return(stackedMaps)
  
}
