# Copyright 2016 European Centre for Medium-Range Weather Forecasts (ECMWF)
# This software is licensed under the terms of the Apache Licence Version 2.0 
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#' caliver: CALIbration and VERification of gridded model outputs
#'
#' Utility functions for the post-processing, calibration and validation of gridded model outputs. Initial test cases include the outputs of the following forest fire models: GEFF and RISICO.
#' 
#' @name caliver
#' @docType package
#'
#' @import ncdf4
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
