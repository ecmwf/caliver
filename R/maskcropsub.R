# Copyright 2016 European Centre for Medium-Range Weather Forecasts (ECMWF)
# This software is licensed under the terms of the Apache Licence Version 2.0 
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#' @title maskcrop
#'
#' @description mask and/or crop a Raster* based on a Polygon.
#'
#' @param r Raster* object
#' @param p SpatialPolygon* object
#' @param mask logical, TRUE to apply mask (default) using p, FALSE otherwise
#' @param crop logical, TRUE to crop (default) using p, FALSE otherwise
#' @param i2subset vector of strings indicating the layer indices to subset
#' 
#' @return A Raster* object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#'   r <- raster::brick('FWI_1980-2016.nc') 
#'   p <- raster::getData(name = "GADM", country = "Italy", level = 0)        
#'   maskcropsub(r, p, mask = TRUE, crop = TRUE)
#'   
#' }
#'

maskcropsub <- function(r, p, mask = TRUE, crop = TRUE, i2subset = NULL){

  if (mask == TRUE) {
    
    message('Masking raster over polygon')
    rMasked <- raster::mask(r, p, progress = 'text')
    
  }else{
    
    rMasked <- r
    
  }
  
  if (crop == TRUE) {
    
    message('Cropping raster over polygon')
    rCropped <- raster::crop(rMasked, p, progress = 'text')
    
  }else{
    
    rCropped <- rMasked
    
  }
  
  if (!is.null(i2subset)) {
    
    message('Subsetting over time indices')
    rSubsetted <- raster::subset(rCropped, i2subset)
    
  }else{
    
    rSubsetted <- rCropped
    
  }
  
  if (!("RasterLayer" %in% class(rSubsetted)) &
      !("RasterBrick" %in% class(rSubsetted)) & 
      !("RasterStack" %in% class(rSubsetted))){
    stop('Error: the fireIndex can only be a raster brick/stack')
  }
  
  if ("RasterStack" %in% class(rSubsetted)){
    message('Convert stack of fire indices into a raster brick')
    rOutput <- raster::brick(rSubsetted, progress = 'text')
  }
  
  if ("RasterBrick" %in% class(rSubsetted) | 
      "RasterLayer" %in% class(rSubsetted)){
    rOutput <- rSubsetted
  }
  
  return(rOutput)
  
}
