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
#' }
#'

plot_percentile_raster <- function(maps,
                                   region = "GLOB", ...){

  if (round(maps@extent@xmin, 0) == 0) {

    # Rotate a Raster* object that has x coordinates (longitude) from 0 to 360,
    # to standard coordinates between -180 and 180 degrees.
    # Longitude between 0 and 360 is frequently used in data
    # from global climate models.
    rotated_map <- raster::rotate(maps)

  } else {

    rotated_map <- maps

  }

  if (region != "GLOB") {

    mask_map <- get_gfed4(varname = "BasisRegions", region = region)
    cropped_map <- raster::trim(raster::mask(rotated_map, mask_map))

  } else {

    cropped_map <- rotated_map

  }

  raster_max <- max(raster::cellStats(cropped_map, stat = "max", na.rm = TRUE))

  breaks <- round(seq(from = 0, to = raster_max, length.out = 10), 0)
  breaks <- unique(breaks)

  # Define palette
  heatcolors <- rev(grDevices::heat.colors(n = length(breaks)))

  raster::plot(cropped_map, addfun = .background_map_fun,
               col = heatcolors, breaks = breaks, ...)

}
