#' caliver: CALIbration and VERification of gridded model outputs
#'
#' Utility functions for the post-processing, calibration and validation of gridded model outputs. Initial test cases include the outputs of the following forest fire models: GEFF and RISICO.
#' 
#' @name caliver
#' @docType package
#'
#' @import rgdal
#' @import ncdf4
#' @importFrom raster raster stack mask crop rotate extent extract t plot
#' @importFrom grDevices heat.colors
#' @importFrom rworldmap getMap
#' @importFrom sp sp.lines CRS
#' @importFrom rasterVis levelplot
#' @importFrom latticeExtra layer
#' @importFrom graphics plot
#' @importFrom stats quantile
#' @importFrom httr GET authenticate write_disk http_error
#' @importFrom stringr str_pad
#' @importFrom lubridate yday
#' @importFrom rhdf5 h5read
#' @importFrom utils download.file
#'
NULL
