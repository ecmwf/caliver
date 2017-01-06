#' Shift map
#'
#' @description This function shifts a netcdf file to be in the following bounding box: -180,180-90,90
#'
#' @param inFile input file(s) to be shifted
#' @param outDir is the directory where the output nc file is saved, by default this is the working directory.
#' 
#' @return A shifted file for ech input file name
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   shiftMap(inFile = "temp.nc", outFile = "output.nc")
#' }
#'

shiftMap <- function(inFile, outDir = getwd()){
  
  outFilenames <- c()
  
  for (singleFile in inFile){
    
    outFile <- file.path(outDir, 
                         paste0(tools::file_path_sans_ext(basename(singleFile)),
                                "_shifted.nc"))
    
    system(paste0("cdo sellonlatbox,-180,180,-90,90 ", singleFile, " ", outFile))
    
    message(paste0("The file has been shifted and the result is stored in: ", 
                   outFile))
    
    outFilenames <- c(outFilenames, outFile)
    
  }
  
  return(outFilenames)
  
}
