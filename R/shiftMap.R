#' Shift map
#'
#' @description This function shifts a netcdf file to be in the following bounding box: -180,180-90,90
#'
#' @param inFile input file to be shifted
#' @param outFile file path where to save the shifted file
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   shift(inFile = "temp.nc", outFile = "output.nc")
#' }
#'

shiftMap <- function(inFile, outFile = NULL){
  
  if (is.null(outFile)) {
    
    outFile <- paste0(tools::file_path_sans_ext(inFile),"_shifted.nc")
    
  }
  
  # message(paste0("Executing: \n", "cdo sellonlatbox,-180,180,-90,90 ", inFile, " ", outFile))
  
  system(paste0("cdo sellonlatbox,-180,180,-90,90 ", inFile, " ", outFile))
  
  message(paste0("The file has been shifted and the result is stored in: ", 
                 outFile))
  
  return(outFile)
  
}
