#' Extract variable and merge all to one file
#'
#' @description This function calculates the CDF at each point of a grid
#'
#' @param dirs is the directory where all the files to read are stored
#' @param varname name of the variable to extract
#'
#' @return file containing only the selected variable
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   dirs <- "/var/tmp/moc0/geff"
#'   x <- getVar(dirs, varname = "fwi")
#' }
#'

getVar <- function(dirs, varname = "fwi"){
  
  tempFile <- tempfile()
  
  ifiles <- paste(list.files(path = dirs, 
                             recursive = TRUE, 
                             full.names = TRUE), 
                  collapse = " ")
  
  system(paste0("cdo select,name=", varname, " ", ifiles, " ", tempFile))
  
  outFile <- paste0(varname, ".nc")
  
  system(paste0("cdo sellonlatbox,-180,180,-90,90 ", tempFile, " ", outFile))
  
  message(paste0("The variable has been extracted from all files and ", 
                 "the result is stored in the working directory: \n", 
                 getwd(), "/", outFile))
  
  # x <- ncdf4::nc_open(outFile)
  
  # return(x)
  
}
