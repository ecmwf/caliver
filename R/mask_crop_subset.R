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

  if (!("RasterLayer" %in% class(r)) &
      !("RasterBrick" %in% class(r)) &
      !("RasterStack" %in% class(r))) {

    stop("Error: r can only be a raster brick/stack")

  }

  if (mask == TRUE | crop == TRUE) {

    # To keep cells along the border cannot use mask(r, p) because only cells
    # with center falling in the polygon will be returned.
    # The solution is to identify cells covering the polygon and set all
    # remaining pixels to NA, see https://goo.gl/22LwJt
    if (class(r) == "RasterLayer") {
      temp_mask <- r
    }else{
      temp_mask <- r[[1]]
    }
    cls <- raster::cellFromPolygon(temp_mask, p, weights = TRUE)[[1]][, "cell"]
    temp_mask[][-cls] <- NA

    if (mask == TRUE) {

      r_masked <- raster::mask(r, temp_mask)

    }else{

      r_masked <- r

    }

    if (crop == TRUE) {

      new_extent <- extent(trim(temp_mask))
      r_cropped <- raster::crop(r_masked, new_extent, progress = "text")

    }else{

      r_cropped <- r_masked

    }

    if (mask == TRUE & crop == TRUE) {

      r_cropped <- raster::trim(r_cropped)

    }

  }else{

    r_cropped <- r

  }

  if (!is.null(idx)) {

    r_subsetted <- raster::subset(r_cropped, idx)

  } else {

    r_subsetted <- r_cropped

  }

  if ("RasterStack" %in% class(r_subsetted)) {

    r_output <- raster::brick(r_subsetted, progress = "text")

  }

  if ("RasterBrick" %in% class(r_subsetted) |
      "RasterLayer" %in% class(r_subsetted)) {

    r_output <- r_subsetted

  }

  return(r_output)

}
