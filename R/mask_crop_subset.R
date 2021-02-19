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
#'   # Define dummy polygon
#'   dummy_polygon <- as(raster::extent(7, 18, 37, 40), "SpatialPolygons")
#'   raster::crs(dummy_polygon) <- "+proj=longlat +datum=WGS84 +no_defs"
#'   
#'   # Read RISICO test data
#'   r_risico <- readRDS(system.file("extdata", "RISICO_raster.rds",
#'                                   package = "caliver"))
#'   
#'   mask_crop_subset(r = r_risico, p = dummy_polygon)
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
