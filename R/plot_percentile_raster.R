#' @title plot_percentile_raster
#'
#' @description This function plots the maps of percentiles
#'
#' @param maps is the result of get_percentile_raster()
#' @param region string of characters describing the region.
#' @param ... additional graphical parameters inherited from plot() in the
#' raster package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_percentile_raster(maps)
#'   # Use custom color palette
#'   plot_percentile_raster(maps,
#'                          col = rev(viridis::inferno(n = 10)))
#' }
#'

plot_percentile_raster <- function(maps, region = "GLOB", ...){

  if (region != "GLOB") {

    BasisRegions <- readRDS(system.file("extdata", "GFED4_BasisRegions.rds",
                                        package = "caliver"))
    mask_map <- BasisRegions[BasisRegions$Region == region, ]
    cropped_map <- raster::trim(raster::mask(maps, mask_map))

  } else {

    cropped_map <- maps

  }

  raster_max <- max(raster::cellStats(cropped_map, stat = "max", na.rm = TRUE))

  breaks <- round(seq(from = 0, to = raster_max, length.out = 10), 0)
  breaks <- unique(breaks)

  raster::plot(cropped_map, addfun = .background_map_fun, breaks = breaks, ...)

}
