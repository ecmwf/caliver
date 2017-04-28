#' caliver: CALIbration and VERification of gridded model outputs
#'
#' Utility functions for the post-processing, calibration and validation of gridded model outputs. Initial test cases include the outputs of the following forest fire models: GEFF and RISICO.
#' 
#' @name caliver
#' @docType package
#'
#' @import ncdf4
#' @importFrom rgdal make_EPSG
#' @importFrom ggplot2 ggplot aes element_text geom_density geom_segment 
#' @importFrom ggplot2 geom_text scale_colour_manual theme theme_bw 
#' @importFrom ggplot2 xlab ylab scale_x_continuous
#' @importFrom raster raster stack mask crop rotate extent extract t plot
#' @importFrom rworldmap getMap
#' @importFrom sp sp.lines CRS
#' @importFrom graphics plot
#' @importFrom httr GET authenticate write_disk http_error
#' @importFrom stringr str_pad
#' @importFrom lubridate yday
#' @importFrom rhdf5 h5read
#' @importFrom utils download.file
#' @importFrom stats median na.omit sd uniroot density quantile
#' @importFrom RCurl getURL
#' 
NULL
