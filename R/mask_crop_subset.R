#' @title mask_crop_subset
#'
#' @description mask and/or crop a Raster* based on a Polygon.
#'
#' @param r Raster* object
#' @param p SpatialPolygon* object
#' @param mask logical, TRUE to apply mask (default) using p, FALSE otherwise
#' @param crop logical, TRUE to crop (default) using p, FALSE otherwise
#' @param idx vector of strings indicating the layer indices to subset
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
#'   mask_crop_subset(r, p, mask = TRUE, crop = TRUE)
#'
#' }
#'

mask_crop_subset <- function(r, p, mask = TRUE, crop = TRUE, idx = NULL){

  if (mask == TRUE) {

    # message("Masking raster over polygon")
    r_masked <- raster::mask(r, p, progress = "text")

    # identify cells covering italy and set all remaining pixels to NA
    # https://goo.gl/22LwJt

  } else {

    r_masked <- r

  }

  if (crop == TRUE) {
    
    r_cropped <- raster::crop(r_masked, p, progress = "text")

  }else{

    r_cropped <- r_masked

  }

  if (!is.null(idx)) {

    r_subsetted <- raster::subset(r_cropped, idx)

  } else {

    r_subsetted <- r_cropped

  }

  if (!("RasterLayer" %in% class(r_subsetted)) &
      !("RasterBrick" %in% class(r_subsetted)) &
      !("RasterStack" %in% class(r_subsetted))) {

    stop("Error: r can only be a raster brick/stack")

  }

  if ("RasterStack" %in% class(r_subsetted)) {

    message("Convert stack of fire indices into a raster brick")
    r_output <- raster::brick(r_subsetted, progress = "text")

  }

  if ("RasterBrick" %in% class(r_subsetted) |
      "RasterLayer" %in% class(r_subsetted)) {

    r_output <- r_subsetted

  }

  return(r_output)

}
