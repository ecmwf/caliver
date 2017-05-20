# Copyright 2016 European Centre for Medium-Range Weather Forecasts (ECMWF)
# This software is licensed under the terms of the Apache Licence Version 2.0 
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#' @title Merge netcdf files on the time dimension
#'
#' @description This function merges all the netcdf files in a given directory over the time dimension. It saves the merged file in the working directory.
#'
#' @param inDir is the directory where all the files to read are stored
#' @param varname name of the variable to extract
#' @param pattern regular expression pattern to select a subset of files
#' @param recursive logical (TRUE by default). If set to TRUE it looks in folders and subfolders
#' @param outFile output filename (including path)
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'   # Mergetime using single variable nc files
#'   catNetcdf(inDir = "/var/tmp/moc0/forestfire",
#'             varname = NULL,
#'             pattern = "geff_reanalysis_an_fwis_fwi_",
#'             recursive = TRUE,
#'             outFile = "outfile.nc")
#'             
#' }
#'

catNetcdf <- function(inDir = NULL, 
                      varname = NULL,
                      pattern = NULL,
                      recursive = FALSE,
                      outFile = "outfile.nc"){
  
  if(Sys.which("cdo")[[1]] == "") {
    
    stop("cdo executable not found. Check PATH or install cdo.") 
    
  }
  
  if(is.null(inDir)) {
    
    stop("Please specify data folder 'inDir'!") 
    
  }
  
  if (is.null(pattern)) {
    
    if (recursive == TRUE){
      ifiles <- paste(list.files(path = inDir, recursive = recursive, 
                                 full.names = TRUE), collapse = " ")
    }else{
      ifiles <- file.path(inDir, "*.nc")
    }
    
  }else{
    
    ifiles <- paste(list.files(path = inDir,
                               pattern = pattern,
                               recursive = recursive, full.names = TRUE), 
                    collapse = " ")
    
  }
  
  if (is.null(varname)){
    
    # Mergetime opens all the file to order them over time
    # system(paste0("cdo mergetime ", ifiles, " ", outFile))
    
    # Cat is computational lighter than mergetime because it opens 1 file at the
    # time and assumes they are already ordered (e.g. due to naming convention)
    
    # For basic precision
    system(paste0("cdo cat ", ifiles, " ", outFile))
    
    # For higher precision
    # system(paste0("cdo -b F64 cat ", ifiles, " ", outFile))
    
  }else{
    
    # For basic precision
    system(paste0("cdo select,name=", varname, " ", ifiles, " ", outFile))
    
    # For higher precision (this generates larger files!)
    # system(paste0("cdo -b F64 select,name=", 
    #               varname, " ", ifiles, " ", outFile))
    
  }
  
  return(outFile)
  
}
