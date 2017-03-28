#' Get GFED4 Basis Regions
#'
#' @description This function retrieves the GFED4 basis region from the fourth-generation global fire emissions database website (http://www.globalfiredata.org/data.html).
#' 
#' @param region name of the GFED4 basis region.
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

getGFED4BasisRegions <- function(region = NULL, outDir = tempdir()){
  
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
    # regionsRasterT <- rotate_2_360(regionsRasterT)
    
    regionsRasterT[regionsRasterT == 0] <- NA
    # Assign projection
    x <- rgdal::make_EPSG()
    regionsRasterT@crs <- sp::CRS(x$prj4[which(x$code == "4326")])
    
    # remove hdf5 file
    unlink(file.path(outDir, fname))
    
    # This might need to be resampled using the attributes of the lower/higher res raster
    # GFEDregions <- raster::resample(regionsRasterT, OtherRaster, method = "ngb")
    
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
    
    return(regionsRasterT)
    
  }
  
}
