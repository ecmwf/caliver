#' Get Burned Areas from the GFED4 database
#'
#' @description This function retrieves Burned Area files from the GFED4 database
#'
#' @param years is a vector containing the years to download
#' @param tempRes is the temporal resolution, it can be "daily" (default) or "monthly"
#' @param outDir is the directory where the downloaded files are saved, by default this is the working directory.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   getBurnedAreas(years = 2014:2015, tempRes = "daily", 
#'                  outDir = "/var/tmp/moc0/geff/GFED4_BurnedAreas_hdf5/daily")
#'                  
#'   getBurnedAreas(years = 2014:2015, tempRes = "monthly", 
#'                  outDir = "/var/tmp/moc0/geff/GFED4_BurnedAreas_hdf5/monthly")
#' }
#'

getBurnedAreas <- function(years, tempRes = "daily", outDir = getwd()){
  
  baseURL <- "ftp://fuoco.geog.umd.edu/gfed4"
  
  if (tempRes == "monthly"){
    
    tmpDate <- seq.Date(from = as.Date(paste0(years[1],"-01-01")), 
                        to = as.Date(paste0(years[length(years)], "-12-31")),        
                        by = "month")
    myDate <- substr(x = gsub("-", "", as.character(tmpDate)), 
                     start = 1, stop = 6)
    
    for (d in myDate){
      inFile <- paste("GFED4.0_DQ_", d, "_BA.hdf", sep="")
      
      myURL <- file.path(baseURL, "monthly", inFile)
      
      if(httr::http_error(myURL))
      
      try(httr::GET(url = myURL, 
                    httr::authenticate(user = "fire", password = "burnt"), 
                    httr::write_disk(file.path(outDir, inFile), 
                                     overwrite = TRUE)),
          silent = FALSE)
    }
  }
  
  if (tempRes == "daily"){
    
    tmpDate <- seq.Date(from = as.Date(paste0(years[1],"-01-01")), 
                        to = as.Date(paste0(years[length(years)], "-12-31")),     
                        by = "day")
    
    for (d in 1:length(tmpDate)){
      
      dd <- lubridate::yday(tmpDate[d])
      yy <- lubridate::year(tmpDate[d])
      myDay <- stringr::str_pad(dd, 3, pad = "0")
      
      inFile <- paste("GFED4.0_DQ_", yy, myDay, "_BA.hdf", sep="")
      
      myURL <- file.path(baseURL, "daily", substr(tmpDate[d], 1, 4), inFile)
      
      try(httr::GET(url = myURL, 
                    httr::authenticate(user = "fire", password = "burnt"), 
                    httr::write_disk(file.path(outDir, inFile), 
                                     overwrite = TRUE)),
          silent = FALSE)
    }
  }
  
}
