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

    # identify cells covering italy and set all remaining pixels to NA
    # https://goo.gl/22LwJt
    # intersectingPixels <- raster::cellFromPolygon(rSubsetted, p,
    #                                       weights = TRUE)[[1]][, "cell"]
    # rSubsetted[][-intersectingPixels] <- NA

  } else {

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

  } else {

    rSubsetted <- rCropped

  }

  if (!("RasterLayer" %in% class(rSubsetted)) &
      !("RasterBrick" %in% class(rSubsetted)) &
      !("RasterStack" %in% class(rSubsetted))) {

    stop('Error: the fireIndex can only be a raster brick/stack')

  }

  if ("RasterStack" %in% class(rSubsetted)) {

    message('Convert stack of fire indices into a raster brick')
    rOutput <- raster::brick(rSubsetted, progress = 'text')

  }

  if ("RasterBrick" %in% class(rSubsetted) |
      "RasterLayer" %in% class(rSubsetted)) {

    rOutput <- rSubsetted

  }

  return(rOutput)

}
