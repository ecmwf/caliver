#' Merge netcdf files on the time dimension
#'
#' @description This function merges all the netcdf files in a given directory over the time dimension. It saves the merged file in the working directory.
#'
#' @param dirs is the directory where all the files to read are stored
#' @param startingString string defining the bginning of the netcdf filenames (needed to exclude other files)
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'   dirs <- "/var/tmp/moc0/forestfire"
#'   mergetime(dirs)
#' }
#'

mergetime <- function(dirs, startingString = "geff_reanalysis_an_fwis_fwi_"){  
  
  if(Sys.which("cdo")[[1]] == "") {
    
    stop("cdo executable not found. Check PATH or install cdo.") 
    
  }
  
  ncfiles <- list.files(path = dirs, 
                        pattern = paste0(startingString, ".*.nc$"), 
                        full.names = TRUE)
  
  system(paste0("cdo mergetime ", 
                paste(ncfiles, collapse = " "), 
                " ", 
                dirs, 
                "/temp.nc"))
  
  system(paste0("cdo sellonlatbox,-180,180,-90,90 ", dirs, "/temp.nc ", dirs, "/outfile.nc"))
  system(paste0("rm ", dirs, "/temp.nc"))
  
  message(paste0("The files have been merged and the result is stored in ", dirs, "/outfile.nc"))
  
  return(paste0(dirs, "/outfile.nc"))
  
}
