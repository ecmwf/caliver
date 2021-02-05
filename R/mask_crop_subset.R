#' @title mask_crop_subset
#'
#' @description mask and/or crop a Raster* based on a Polygon.
#'
#' @param r Raster* object
#' @param p SpatialPolygon* object
#' @param mask logical, TRUE to apply mask (default) using p, FALSE otherwise
#' @param crop logical, TRUE to crop (default) using p, FALSE otherwise
#' @param idx vector of strings indicating the layer indices to subset
#' @param accurate logical, TRUE to apply more accurate cropping/masking,
#' FALSE otherwise
#' @param ... additional arguments as in writeRaster
#' (e.g. \code{progress = "text"})
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

mask_crop_subset <- function(r, p, mask = TRUE, crop = TRUE, idx = NULL,
                             accurate = FALSE, ...){

  if (!("RasterLayer" %in% class(r)) &
      !("RasterBrick" %in% class(r)) &
      !("RasterStack" %in% class(r))) {

    stop("Error: r can only be a raster layer/brick/stack")

  }

  if (!is.null(idx)) r <- raster::subset(r, idx)

  if (accurate == TRUE){
    # To keep cells along the border we cannot use mask(r, p) because only
    # cells with centroids falling in the polygon will be returned.
    # The solution is to identify cells covering the polygon and set all
    # remaining pixels to NA, see https://goo.gl/22LwJt
    temp_mask <- r[[1]]
    cls <- raster::cellFromPolygon(temp_mask,
                                   p, weights = TRUE)[[1]][, "cell"]
    temp_mask[][-cls] <- NA
    p <- temp_mask
  }

  if (mask == TRUE) r <- raster::mask(r, p, ...)

  if (crop == TRUE) r <- raster::crop(r, p, ...)

  if (mask == TRUE & crop == TRUE) r <- raster::trim(r)

  return(r)

}
