#' caliver: CALIbration and VERification of gridded model outputs
#'
#' Utility functions for the post-processing, calibration and validation of
#' gridded model outputs. Initial test cases include the outputs of the
#' following forest fire models: GEFF and RISICO.
#'
#' @name caliver
#' @docType package
#'
#' @import ncdf4
#' @import ggplot2
#' @importFrom lubridate is.Date
#' @importFrom raster raster stack brick mask crop rotate extent extract t plot
#' @importFrom raster crs cellFromPolygon cellStats nlayers getData trim overlay
#' @importFrom raster intersect compareRaster
#' @importFrom rworldmap getMap
#' @importFrom graphics plot legend par
#' @importFrom stats median na.omit sd uniroot density quantile
#'
NULL
