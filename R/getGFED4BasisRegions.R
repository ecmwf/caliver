#' Get GFED4 Basis Regions
#'
#' @description This function retrieves the GFED4 basis region from the fourth-generation global fire emissions database website (http://www.globalfiredata.org/data.html).
#' 
#' @param outDir is the directory where the converted file(s) are saved, by default this is a temporary directory.
#' 
#' @return A RasterLayer
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   GFED4BasisRegions <- getGFED4BasisRegions()
#' }
#'

getGFED4BasisRegions <- function(outDir = tempdir()){
  
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
    regions <- rhdf5::h5read(file.path(outDir, fname), "/ancill/basis_regions")
    
    # Convert hdf5 to raster
    regionsRaster <- raster::raster(regions)
    
    # Transform the raster
    # transpose
    regionsRasterT <- raster::t(regionsRaster)
    # set extent
    raster::extent(regionsRasterT) <- raster::extent(-180, 180, -90, 90)
    # rotate
    regionsRasterTR <- rotate_2_360(regionsRasterT)
    
    regionsRasterTR[regionsRasterTR == 0] <- NA
    # Assign projection
    x <- rgdal::make_EPSG()
    regionsRasterTR@crs <- sp::CRS(x$prj4[which(x$code == "4326")])
    
    # remove hdf5 file
    unlink(file.path(outDir, fname))
    
    # This might need to be resampled using the attributes of the lower/higher res raster
    # GFEDregions <- raster::resample(regionsRasterTR, OtherRaster, method = "ngb")
    
  }
  
  return(regionsRasterTR)
  
}
