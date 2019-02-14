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
#' @importFrom rgdal readOGR
#' @importFrom raster raster stack mask crop rotate extent extract t plot
#' @importFrom raster cellFromPolygon cellStats nlayers getData trim overlay
#' @importFrom rworldmap getMap
#' @importFrom sp sp.lines CRS
#' @importFrom graphics plot
#' @importFrom httr GET authenticate write_disk http_error
#' @importFrom stringr str_pad
#' @importFrom lubridate yday
#' @importFrom rhdf5 h5read
#' @importFrom utils download.file untar
#' @importFrom stats median na.omit sd uniroot density quantile
#' @importFrom RCurl getURL getBinaryURL
#' @importFrom plotrix rescale
#' @importFrom reshape2 melt
#' @importFrom R.utils gunzip
#' @importFrom graphics legend par
#' @importFrom viridis viridis plasma inferno
#' @importFrom rasterVis levelplot
#' 
#'
NULL
