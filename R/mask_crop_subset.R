#' @title mask_crop_subset
#'
#' @description mask and/or crop a Raster* based on a Polygon.
#'
#' @param r Raster* object
#' @param p SpatialPolygon* object
#' @param idx vector of strings indicating the layer indices to subset.
#' @param ... additional arguments as in writeRaster
#' (e.g. \code{progress = "text"})
#' 
#' @details Please note that cells along the border with centroids falling
#' outside the polygon \code{p} will not be returned.
#' If cells along the border are needed, we suggest to identify cells covering
#' the polygon and set all remaining pixels to NA, as described in this post:
#' \url{https://goo.gl/22LwJt}.
#'
#' @return A Raster* object with resolution and land-sea mask matching those of
#' \code{r} and extent matching \code{p}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   # Define dummy polygon
#'   shape <- as(raster::extent(7, 18, 37, 40), "SpatialPolygons")
#'   
#'   # Read RISICO test data
#'   r_risico <- readRDS(system.file("extdata", "RISICO_raster.rds",
#'                                   package = "caliver"))
#'   
#'   mask_crop_subset(r = r_risico, p = shape)
#'
#' }
#'

mask_crop_subset <- function(r, p, idx = NULL, ...){

  if (!("RasterLayer" %in% class(r)) &
      !("RasterBrick" %in% class(r)) &
      !("RasterStack" %in% class(r))) {

    stop("Error: r can only be a raster layer/brick/stack")

  }

  if (!is.null(idx)) {
    message("Subsetting...")
    r <- raster::subset(r, idx, ...)
  }

  # if (accurate == TRUE){
  #   # To keep cells along the border we cannot use mask(r, p) because only
  #   # cells with centroids falling in the polygon will be returned.
  #   # The solution is to identify cells covering the polygon and set all
  #   # remaining pixels to NA, see https://goo.gl/22LwJt
  #   temp_mask <- r[[1]]
  #   cls <- raster::cellFromPolygon(temp_mask,
  #                                  p, weights = TRUE)[[1]][, "cell"]
  #   temp_mask[][-cls] <- NA
  #   p <- temp_mask
  # }

  message("Cropping...")
  r <- raster::crop(r, p, ...)

  message("Masking...")
  r <- raster::mask(r, p, ...)
  r <- raster::trim(r, ...)

  return(r)

}
