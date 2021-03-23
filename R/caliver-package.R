#' caliver: CALIbration and VERification of gridded model outputs
#'
#' Utility functions for the post-processing, calibration and validation of
#' gridded model outputs. Initial test cases include the outputs of the
#' following forest fire models: GEFF and RISICO.
#'
#' @name caliver
#' @docType package
#'
#' @import ggplot2
#' @import ncdf4
#' @importFrom graphics plot legend par
#' @importFrom raster raster stack brick mask crop rotate extent extract t plot
#' @importFrom raster crs cellFromPolygon cellStats nlayers getData trim overlay
#' @importFrom raster intersect compareRaster
#' @importFrom reshape2 melt
#' @importFrom rworldmap getMap
#' @importFrom sp SpatialPolygons SpatialPolygonsDataFrame
#' @importFrom stats median na.omit sd uniroot density quantile
#' @importFrom utils tail
#'
NULL
