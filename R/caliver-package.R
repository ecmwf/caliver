#' caliver: CALIbration and VERification of gridded model outputs
#'
#' Utility functions for the post-processing, calibration and validation of gridded model outputs. Initial test cases include the outputs of the following forest fire models: GEFF and RISICO.
#' 
#' @name caliver
#' @docType package
#'
#' @import rgdal
#' @import ncdf4
#' @importFrom tibble as_tibble
#' @importFrom grDevices heat.colors
#' @importFrom rworldmap getMap
#' @importFrom sp sp.lines CRS
#' @importFrom raster raster stack mask crop rotate extent extract t
#' @importFrom rasterVis levelplot
#' @importFrom latticeExtra layer
#' @importFrom graphics plot
#' @importFrom stats quantile
#' @importFrom httr GET authenticate write_disk 
#' @importFrom stringr str_pad
#' @importFrom lubridate yday
#' @importFrom rhdf5 h5read
#' @importFrom utils download.file
#'
NULL

#' Data set: Fuel model
#' 
#' @description Fuel model 
#' @name fuelmodel
#' @docType data
#' @format The format is: Formal class 'RasterLayer' [package "raster"] with 12
#' slots ..@ file :Formal class '.RasterFile' [package "raster"] with 13 slots
#' .. .. ..@ name : chr "/var/tmp/moc0/geff/clim_fuelmodel.nc_ggN256" .. .. ..@
#' datanotation: chr "INT4S" .. .. ..@ byteorder : chr "little" .. .. ..@
#' nodatavalue : num -Inf .. .. ..@ NAchanged : logi FALSE .. .. ..@ nbands :
#' int 1 .. .. ..@ bandorder : chr "BIL" .. .. ..@ offset : int 0 .. .. ..@
#' toptobottom : logi TRUE .. .. ..@ blockrows : int 1 .. .. ..@ blockcols :
#' int 512 .. .. ..@ driver : chr "gdal" .. .. ..@ open : logi FALSE ..@ data
#' :Formal class '.SingleLayerData' [package "raster"] with 13 slots .. .. ..@
#' values : logi(0) .. .. ..@ offset : num 0 .. .. ..@ gain : num 1 .. .. ..@
#' inmemory : logi FALSE .. .. ..@ fromdisk : logi TRUE .. .. ..@ isfactor :
#' logi FALSE .. .. ..@ attributes: list() .. .. ..@ haveminmax: logi TRUE ..
#' .. ..@ min : num -2.15e+09 .. .. ..@ max : num 2.15e+09 .. .. ..@ band : int
#' 1 .. .. ..@ unit : chr "" .. .. ..@ names : chr "clim_fuelmodel.nc_ggN256"
#' ..@ legend :Formal class '.RasterLegend' [package "raster"] with 5 slots ..
#' .. ..@ type : chr(0) .. .. ..@ values : logi(0) .. .. ..@ color : logi(0) ..
#' .. ..@ names : logi(0) .. .. ..@ colortable: logi(0) ..@ title : chr(0) ..@
#' extent :Formal class 'Extent' [package "raster"] with 4 slots .. .. ..@
#' xmin: num -0.352 .. .. ..@ xmax: num 360 .. .. ..@ ymin: num -89.8 .. .. ..@
#' ymax: num 89.8 ..@ rotated : logi FALSE ..@ rotation:Formal class
#' '.Rotation' [package "raster"] with 2 slots .. .. ..@ geotrans: num(0) .. ..
#' ..@ transfun:function () ..@ ncols : int 512 ..@ nrows : int 256 ..@ crs
#' :Formal class 'CRS' [package "sp"] with 1 slot .. .. ..@ projargs: chr
#' "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" ..@ history :
#' list() ..@ z : list()
#' @keywords datasets
#' @examples
#' 
#' data(fuelmodel)
#' 
"fuelmodel"
