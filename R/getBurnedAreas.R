#' Get Burned Areas from the GFED4 database
#'
#' @description This function retrieves Burned Area files from the GFED4 database
#'
#' @param years is a vector containing the years to download
#' @param tempRes is the temporal resolution, it can be "daily" (default) or "monthly"
#' @param outDir is the directory where the downloaded files are saved, by default this is a temporary directory.
#' @param outFormat is the desired format for the output, by default it is "hdf5" but it can also be set equal to "netcdf".
#' @param varname name of the variable to extract, by default it keeps all the available variables.
#' @param merge logical (TRUE by default). If TRUE it merges all the file over the time dimension.
#' #@param keep logical (FALSE by default). If FALSE it deletes temporary files.
#' 
#' @note The conversion from hdf5 to netcdf gets stuck in RStudio, please use the basic console.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   getBurnedAreas(years = 2014, tempRes = "monthly", 
#'                  outDir = "/var/tmp/moc0/geff/GFED4_BurnedAreas",
#'                  outFormat = "netcdf", varname = "BurnedArea", 
#'                  merge = TRUE, keep = FALSE)
#' }
#'

getBurnedAreas <- function(years, tempRes = "daily", 
                           outDir = tempdir(), 
                           varname = NULL, outFormat = "hdf5",
                           merge = TRUE, keep = FALSE){
  
  baseURL <- "ftp://fuoco.geog.umd.edu/gfed4"
  
  if (tempRes == "monthly"){
    
    tmpDate <- seq.Date(from = as.Date(paste0(years[1],"-01-01")), 
                        to = as.Date(paste0(years[length(years)], "-12-31")),  
                        by = "month")
    
    myDate <- substr(x = gsub("-", "", as.character(tmpDate)), 
                     start = 1, stop = 6)
    
    dirURL <- paste0(baseURL, "/monthly/")
  
  }
  
  if (tempRes == "daily"){
    
    tmpDate <- seq.Date(from = as.Date(paste0(years[1],"-01-01")), 
                        to = as.Date(paste0(years[length(years)], "-12-31")),   
                        by = "day")
    
    dayOfYear <- lubridate::yday(tmpDate)
    dayOfYear3CHR <- stringr::str_pad(dayOfYear, 3, pad = "0")
    justYear <- substr(x = gsub("-", "", as.character(tmpDate)), 
                       start = 1, stop = 4)
    
    myDate <- paste0(justYear, dayOfYear3CHR)
    
    dirURL <- paste0(baseURL, "/daily/")
    
  }
  
  for (d in myDate){
    
    justYear <- substr(d, start = 1, stop = 4)
    
    if (tempRes == "monthly"){
      
      inFile <- paste0("GFED4.0_MQ_", d, "_BA.hdf")
      myURL <- paste0(dirURL, inFile)
      
    }
    
    if (tempRes == "daily"){
      
      inFile <- paste0("GFED4.0_DQ_", d, "_BA.hdf")
      myURL <- paste0(dirURL, justYear, "/", inFile)
      
    }
    
    try(x <- httr::GET(url = myURL, 
                       httr::authenticate(user = "fire", password = "burnt"), 
                       httr::write_disk(file.path(outDir, inFile), 
                                        overwrite = TRUE)), silent = FALSE)
    
    # Check for unavailable data (HTTP 404 status)
    if (x$status_code != 404) {
      
      # Check whether we need to save all the variables or only some of them
      if (!is.null(varname)){
        
        varOption <- paste0(" -v ", varname)
        
      }else{
        
        varOption <- ""
        
      }
      
      if (tolower(outFormat) == "netcdf"){
        
        string2call <- paste0("ncl_convert2nc ", file.path(outDir, inFile), 
                              varOption, " -o ", outDir, "/")
        
        # This can give problems in RStudio, but works fine in the console
        system(string2call)
        
        unlink(file.path(outDir, inFile))
        
      }
      
      if (merge == TRUE) {
        
        outFileName <- ifelse(is.null(varname), "GFED4.nc", 
                              paste0(varname,".nc"))
        
        mergetime(dirs = outDir, outFile = outFileName, outDir = getwd())
        
        # if (keep == FALSE) {
        #  unlink(file.path(outDir, gsub(inFile, ".hdf", ".nc")))
        # }
        
      }
      
    }else{
      
      unlink(file.path(outDir, inFile))
      
    }
    
  }
  
}
