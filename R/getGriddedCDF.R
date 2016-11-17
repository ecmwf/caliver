#' Calculate CDF and percentiles
#'
#' @description This function calculates the CDF at each point of a grid
#'
#' @param ncfile is the name of the file(path) to read
#' @param probs numeric vector of probabilities with values in [0,1] listing which percentiles should be calculated
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
                          probs = c(50, 75, 90, 99)){
  
  outList <- list()
  nameList <- c()
  
  for (prob in probs){
    
    # fileName <- tools::file_path_sans_ext(ncfile)
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
  
  return(outList)
  
}
