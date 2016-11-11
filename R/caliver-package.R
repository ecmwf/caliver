#' caliver: CALIbration and VERification of gridded model outputs
#'
#' Utility functions for the post-processing, calibration and validation of gridded model outputs. Initial test cases include the outputs of the following forest fire models: GEFF and RISICO.
#' 
#' @name caliver
#' @docType package
#'
#' @import rgdal
#' @import ncdf4
#' @importFrom grDevices heat.colors
#' @importFrom rworldmap getMap
#' @importFrom sp sp.lines
#' @importFrom raster stack  
#' @importFrom rasterVis levelplot
#' @importFrom latticeExtra layer
#'
NULL