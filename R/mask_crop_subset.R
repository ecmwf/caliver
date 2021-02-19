#' @title mask_crop_subset
#'
#' @description mask and/or crop a Raster* based on a Polygon.
#'
#' @param r Raster* object
#' @param p SpatialPolygons* object
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
#'   # Define dummy polygon, as sf simple feature
#'   shape <- sf::st_bbox(c(xmin = 7, xmax = 18, ymax = 40, ymin = 18),
#'                        crs = sf::st_crs(4326))
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

    stop("Error: r can only be a RasterLayer/Brick/Stack")

  }

  if (!("SpatialPolygons" %in% class(p)) &
      !("SpatialPolygonsDataFrame" %in% class(p))) {

    stop("Error: p can only be a SpatialPolygons or SpatialPolygonsDataFrame")

  }

  if (!is.null(idx)) {
    message("Subsetting...")
    r <- raster::subset(r, idx, ...)
  }

  message("Cropping...")
  r <- raster::crop(r, p, ...)

  message("Masking...")
  r <- raster::mask(r, p, ...)

  return(r)

}
