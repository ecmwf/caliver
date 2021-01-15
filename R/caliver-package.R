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
#' @importFrom hdf5r h5file
#' @importFrom gdalUtils get_subdatasets gdal_translate
#' @importFrom raster raster stack brick mask crop rotate extent extract t plot
#' @importFrom raster crs cellFromPolygon cellStats nlayers getData trim overlay
#' @importFrom raster intersect
#' @importFrom rworldmap getMap
#' @importFrom graphics plot legend par
#' @importFrom stringr str_pad
#' @importFrom lubridate yday
#' @importFrom stats median na.omit sd uniroot density quantile
#' @importFrom RCurl getURL getBinaryURL
#' @importFrom plotrix rescale
#' @importFrom reshape2 melt
#' @importFrom utils download.file untar
#' @importFrom R.utils gunzip
#'
NULL
