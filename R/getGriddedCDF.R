#' Calculate CDF and percentiles
#'
#' @description This function calculates the CDF at each point of a grid
#'
#' @param ncfile is the name of the file to read
#' @param varname name of the variable to extract
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

getGriddedCDF <- function(ncfile, varname = "fwi", probs = c(50, 75, 90, 99)){
  
  fwi <- ncdf4::ncvar_get(ncdf4::nc_open(ncfile), varid = varname)
  
  tmpFolder <- tempdir()
  outList <- list()
  nameList <- c()
  
  for (prob in probs){
    
    system(paste0("cdo timpctl,", prob, " ", ncfile, 
                  " -timmin ", ncfile, 
                  " -timmax ", ncfile, " ", 
                  tmpFolder, "/outfile", prob, ".nc"))
    
    outList[[which(probs == prob)]] <- raster::raster(paste0(tmpFolder, 
                                                           "/outfile",
                                                           prob, ".nc"))
    
    nameList <- c(nameList, paste0(toupper(varname), "_", 
                                   prob, "th_percentile"))
    
  }
  
  names(outList) <- nameList
  
  return(outList)
  
}
