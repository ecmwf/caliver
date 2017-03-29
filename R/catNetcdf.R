#' @title Merge netcdf files on the time dimension
#'
#' @description This function merges all the netcdf files in a given directory over the time dimension. It saves the merged file in the working directory.
#'
#' @param dirs is the directory where all the files to read are stored
#' @param varname name of the variable to extract
#' @param startingString string defining the beginning of the netcdf filenames (needed to exclude other files)
#' @param recursive logical (TRUE by default). If set to TRUE it looks in folders and subfolders
#' @param outFileName output filename
#' @param outDir is the directory where outFileName is saved, by default this is the working directory.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'   # Mergetime using single variable nc files
#'   catNetcdf(dirs = "/var/tmp/moc0/forestfire",
#'             varname = NULL,
#'             startingString = "geff_reanalysis_an_fwis_fwi_",
#'             recursive = TRUE,
#'             outFileName = "outfile.nc",
#'             outDir = getwd())
#'             
#' }
#'

catNetcdf <- function(dirs = NULL, 
                      varname = NULL,
                      startingString = "",
                      recursive = FALSE,
                      outFileName = "outfile.nc",
                      outDir = getwd()){  
  
  outFilePath <- file.path(outDir, outFileName)
  
  if(Sys.which("cdo")[[1]] == "") {
    
    stop("cdo executable not found. Check PATH or install cdo.") 
    
  }
  
  if(is.null(dirs)) {
    
    stop("Please specify data folder 'dirs'!") 
    
  }
  
  if (startingString == "") {
    if (recursive == TRUE){
      ifiles <- paste(list.files(path = dirs, recursive = recursive, 
                                 full.names = TRUE), collapse = " ")
    }else{
      ifiles <- file.path(dirs, "*.nc")
    }
  }else{
    ifiles <- paste(list.files(path = dirs, 
                               pattern = paste0(startingString, ".*.nc$"),
                               recursive = recursive, full.names = TRUE), 
                    collapse = " ")
  }
  
  if (is.null(varname)){
    
    # Mergetime opens all the file to order them over time
    # system(paste0("cdo mergetime ", ifiles, " ", outFilePath))
    
    # Cat opens 1 file at the time because it assumes they are already ordered
    system(paste0("cdo cat ", ifiles, " ", outFilePath))
    
  }else{
    
    system(paste0("cdo select,name=", varname, " ", ifiles, " ", outFilePath))
    
  }
  
  return(outFilePath)
  
}
