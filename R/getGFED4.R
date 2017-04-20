#' @title getGFED4
#'
#' @description Get data from the fourth-generation Global Fire Emissions Database (GFED4)
#'
#' @param years is a vector containing the years to download
#' @param tempRes is the temporal resolution, it can be "daily" (default) or "monthly"
#' @param varname name of the variable to extract, this can be one of the following:
#' \itemize{
#'   \item{"BasisRegions"}{}
#'   \item{"BurnedArea"}{}
#'   \item{"BurnedAreaUncertainty"}{}
#'   \item{"MeanBurnDateUncertainty"}{}
#'   \item{"source"}{}
#'   \item{"TreeCoverDist"}{}
#'   \item{"LandCoverDist"}{}
#'   \item{"PeatFraction"}{}
#' }
#' @param region string of characters describing the region. It can only assume the 15 values listed below:
#' \itemize{
#'   \item{"Global"}{or GLOB}
#'   \item{"Boreal North America"}{or BONA}
#'   \item{"Temperate North America"}{or TENA}
#'   \item{"Central America"}{or CEAM}
#'   \item{"Northern Hemisphere South America"}{or NHSA}
#'   \item{"Southern Hemisphere South America"}{or SHSA}
#'   \item{"Europe"}{or EURO}
#'   \item{"Middle East"}{or MIDE}
#'   \item{"Northern Hemisphere Africa"}{or NHAF}
#'   \item{"Southern Hemisphere Africa"}{or SHAF}
#'   \item{"Boreal Asia"}{or BOAS}
#'   \item{"Central Asia"}{or CEAS}
#'   \item{"Southeast Asia"}{or SEAS}
#'   \item{"Equatorial Asia"}{or EQAS}
#'   \item{"Australia and New Zealand"}{or AUST}
#' }
#' 
#' @note The conversion from hdf5 to netcdf gets stuck in RStudio (see \url{https://support.rstudio.com/hc/en-us/community/posts/200629578-system-call-from-RStudio-does-not-find-path-same-command-from-commandline-R-works-fine-}), please use the console.
#' 
#' @return A RasterBrick (if downloading observations) or spatial polygons (if downloading basis regions)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#'   # Basis regions
#'   BasisRegions <- getGFED4(varname = "BasisRegions")
#' 
#'   # Monthly burned areas
#'   BurnedAreas <- getGFED4(years = 1997:2015, tempRes = "monthly", 
#'                           varname = "BurnedArea")
#'                           
#'   # Daily burned areas
#'   DailyBurnedAreas <- getGFED4(years = 2003:2015, tempRes = "daily", 
#'                                varname = "BurnedArea")
#'            
#' }
#'

getGFED4 <- function(years = NULL, 
                     tempRes = "daily", 
                     varname = NULL,
                     region = "GLOB"){
  
  outDir <- tempdir()
  
  if (is.null(varname)) stop("Please enter valid varname")
  
  if(varname == "BasisRegions"){
    
    if (is.null(region)) stop("Please enter valid region")
    
    baseURL <- "http://www.falw.vu/~gwerf/GFED/GFED4/"
    fname <- "GFED4.1s_2015.hdf5"
    theURL <- paste0(baseURL, fname)
    
    if (httr::http_error(theURL)){
      
      stop("Server currently unavailable, please try again later.")
      
    }else{
      
      # Download the file
      download.file(url = theURL, destfile = file.path(outDir, fname), 
                    quiet = TRUE, cacheOK = TRUE)
      
      # Extract dataset with basis regions
      regions <- rhdf5::h5read(file.path(outDir, fname), 
                               "/ancill/basis_regions")
      
      # Convert hdf5 to raster
      regionsRaster <- raster::raster(regions)
      
      # Transform the raster
      # transpose
      regionsRasterT <- raster::t(regionsRaster)
      # set extent
      raster::extent(regionsRasterT) <- raster::extent(-180, 180, -90, 90)
      # Define zeros as NAs
      regionsRasterT[regionsRasterT == 0] <- NA
      # Assign projection
      x <- rgdal::make_EPSG()
      regionsRasterT@crs <- sp::CRS(x$prj4[which(x$code == "4326")])
      
      # remove hdf5 file
      unlink(file.path(outDir, fname))
      
      # This might need to be resampled using the attributes of the lower/higher
      # resolution of the other raster (e.g. rasterB):
      # GFEDregions <- raster::resample(regionsRasterT, rasterB, method = "ngb")
      
      if (!is.null(region)){
        if (region == "BONA") regionsRasterT[regionsRasterT != 1] <- NA
        if (region == "TENA") regionsRasterT[regionsRasterT != 2] <- NA
        if (region == "CEAM") regionsRasterT[regionsRasterT != 3] <- NA
        if (region == "NHSA") regionsRasterT[regionsRasterT != 4] <- NA
        if (region == "SHSA") regionsRasterT[regionsRasterT != 5] <- NA
        if (region == "EURO") regionsRasterT[regionsRasterT != 6] <- NA
        if (region == "MIDE") regionsRasterT[regionsRasterT != 7] <- NA
        if (region == "NHAF") regionsRasterT[regionsRasterT != 8] <- NA
        if (region == "SHAF") regionsRasterT[regionsRasterT != 9] <- NA
        if (region == "BOAS") regionsRasterT[regionsRasterT != 10] <- NA
        if (region == "CEAS") regionsRasterT[regionsRasterT != 11] <- NA
        if (region == "SEAS") regionsRasterT[regionsRasterT != 12] <- NA
        if (region == "EQAS") regionsRasterT[regionsRasterT != 13] <- NA
        if (region == "AUST") regionsRasterT[regionsRasterT != 14] <- NA
      }
      
      regionsPolygons <- raster::rasterToPolygons(x = regionsRasterT)
      
      return(regionsPolygons)
      
    }
    
  }else{
    
    if (is.null(years)) stop("Please enter valid years")
    if (is.null(tempRes)) stop("Please enter valid tempRes")
    
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
    
    # Create a tmp directory
    myTempDir <- tempdir()
    dir.create(myTempDir, showWarnings = FALSE)
    message(paste0("Downloading temporary files in: ", myTempDir))
    
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
                         httr::write_disk(file.path(myTempDir, inFile), 
                                          overwrite = TRUE)), silent = FALSE)
      
      # Check for unavailable data (HTTP 404 status)
      if (x$status_code == 404) {
        
        unlink(file.path(myTempDir, inFile))
        message("Data are currently unavailable")
        
      }else{
        
        # Check whether we need to save all the variables or only some of them
        if (is.null(varname)){
          
          varOption <- ""
          
        }else{
          
          varOption <- paste0(" -v ", varname)
          
        }
        
        string2call <- paste0("ncl_convert2nc ", file.path(myTempDir, inFile), 
                              varOption, " -o ", myTempDir, "/")
        
        # This can give problems in RStudio, but works fine in the console
        system(string2call)
        
        unlink(file.path(myTempDir, inFile))
        
      }
      
    }
    
    outFileName <- ifelse(is.null(varname), "GFED4.nc", 
                          paste0(varname,".nc"))
    
    # myTempDir only contains Burned Area files!
    catNetcdf(inDir = myTempDir, outFileName = outFileName)
    
    # The resulting raster brick is in a quater degree resolution but the 
    # extent and the coordinate system should be set manually
    mergedRaster <- raster::brick(outFileName)
    
    # Transform the rasterBrick, flipping it on the y direction
    message("Flipping the raster on the y-direction")
    regionsRasterT <- raster::flip(mergedRaster, direction='y', 
                                   progress = 'text')
    # Set extent
    raster::extent(regionsRasterT) <- raster::extent(-180, 180, -90, 90)
    
    # Assign projection
    x <- rgdal::make_EPSG()
    regionsRasterT@crs <- sp::CRS(x$prj4[which(x$code == "4326")])
    
    # TEST
    # y <- sum(regionsRasterT, na.rm = TRUE)
    # raster::plot(y)
    # backgroundMap <- rworldmap::getMap(resolution = "low")
    # raster::plot(backgroundMap, add = TRUE)
    
    message("Removing temporary files and folder")
    unlink(file.path(myTempDir))
    unlink(file.path(outDir, outFileName))
    
    return(regionsRasterT)
    
  }
  
}
