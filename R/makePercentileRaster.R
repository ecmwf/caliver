#' @title makePercentileRaster
#'
#' @description This function calculates the given percentile at each grid point
#'
#' @param inFilePath is the name of the file(path) to read
#' @param probs numeric vector of probabilities with values in [0,100] listing which percentiles should be calculated
#' @param outDir is the directory where the output nc files are saved, by default this is a temporary directory.
#' 
#' @return list containing all the generated percentile maps
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   x <- makePercentileRaster(inFilePath = "./outfile.nc",
#'                             probs = c(50, 75, 90, 99),
#'                             outDir = getwd())
#' }
#'

makePercentileRaster <- function(inFilePath, 
                                 probs,
                                 outDir = tempdir()){
  
  stackedMaps <- raster::stack() 
  
  for (i in 1:length(probs)){
    
    prob <- probs[i]
    
    fileName <- tools::file_path_sans_ext(basename(inFilePath))
    
    outFile <- file.path(outDir, paste0(fileName, "_", prob, ".nc"))
    
    system(paste0("cdo timpctl,", prob, " ", inFilePath, 
                  " -timmin ", inFilePath, 
                  " -timmax ", inFilePath, " ", outFile))
    
    probRaster <- raster::raster(outFile)
    
    varname <- names(ncdf4::nc_open(inFilePath)$var)
    names(probRaster) <- paste0(toupper(varname), prob)
    
    if (length(probs) > 1) {
      
      stackedMaps <- raster::stack(stackedMaps, probRaster)
      
    }else{
      
      stackedMaps <- probRaster
      
    }
    
  }
  
  return(stackedMaps)
  
}
