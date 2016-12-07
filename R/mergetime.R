#' Merge netcdf files on the time dimension
#'
#' @description This function merges all the netcdf files in a given directory over the time dimension. It saves the merged file in the working directory.
#'
#' @param dirs is the directory where all the files to read are stored
#' @param varname name of the variable to extract
#' @param startingString string defining the beginning of the netcdf filenames (needed to exclude other files)
#' @param recursive logical (TRUE by default). If set to TRUE it looks in folders and subfolders
#' @param outFile filename where to store results
#' @param outDir is the directory where outFile is saved, by default this is the working directory.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Mergetime using single variable nc files
#'   mergetime(dirs = "/var/tmp/moc0/forestfire",
#'             varname = NULL,
#'             startingString = "geff_reanalysis_an_fwis_fwi_",
#'             recursive = TRUE,
#'             outFile = "outfile.nc",
#'             outDir = getwd())
#'             
#' }
#'

mergetime <- function(dirs = NULL, varname = NULL, startingString = "", 
                      recursive = TRUE, 
                      outFile = "outfile.nc", outDir = getwd()){  
  
  if(Sys.which("cdo")[[1]] == "") {
    
    stop("cdo executable not found. Check PATH or install cdo.") 
    
  }
  
  if(is.null(dirs)) {
    
    stop("Please specify data folder!") 
    
  }
  
  if (startingString == "") {
    if (recursive == TRUE){
      ifiles <- paste(list.files(path = dirs,
                                 recursive = recursive, 
                                 full.names = TRUE), 
                      collapse = " ")
    }else{
      ifiles <- paste0(dirs, "/*.nc")
    }
  }else{
    ifiles <- paste(list.files(path = dirs, 
                               pattern = paste0(startingString, ".*.nc$"),
                               recursive = recursive, 
                               full.names = TRUE), 
                    collapse = " ")
  }
  
  if (is.null(varname)){
    system(paste0("cdo mergetime ", 
                  ifiles, " ", outDir, "/", outFile))
  }else{
    # system(paste0("cdo mergetime -select,param=", varname, " ", 
    #               ifiles, " ", outDir, "/", outFile))
    system(paste0("cdo select,name=", varname, " ", ifiles, " ", outDir, "/", outFile))
  }
  
  message(paste0("The files have been merged and the result is stored in: \n", outDir, "/", outFile))
  
  return(paste0(outDir, "/", outFile))
  
}
