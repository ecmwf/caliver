#' @title plot_percentile_raster
#'
#' @description This function plots the maps of percentiles
#'
#' @param maps is the result of getGriddedCDF()
#' @param rotate_map logical, if TRUE it uses a background map
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

plot_percentile_raster <- function(maps, rotate_map = FALSE, region = "GLOB", ...){

  if (rotate_map == TRUE) {

    # Rotate a Raster* object that has x coordinates (longitude) from 0 to 360,
    # to standard coordinates between -180 and 180 degrees.
    # Longitude between 0 and 360 is frequently used in data
    # from global climate models.
    rotated_map <- raster::rotate(maps)

  } else {

    rotated_map <- maps

  }

  if (region != "GLOB") {

    mask_map <- get_gfed4(varname = 'BasisRegions', region = region)
    cropped_map <- raster::trim(raster::mask(rotated_map, mask_map))

  } else {

    cropped_map <- rotated_map

  }

  # Define a background map
  backgroundMap <- rworldmap::getMap(resolution = "low")
  # We want to plot the background map on each layers of the stack, so we need 
  # to create a function and pass it to the addfun argument 
  # (see ?plot in the raster package)
  fun <- function() {

    plot(backgroundMap, add = TRUE, border = 'lightgray')

  }

  raster_min <- min(raster::cellStats(cropped_map, stat = 'min', na.rm = TRUE))
  raster_max <- max(raster::cellStats(cropped_map, stat = 'max', na.rm = TRUE))

  # Define palette (from: rev(grDevices::heat.colors(n = 10)))
  heatcolors <- c("#FFFFBFFF", "#FFFF40FF", "#FFFF00FF", "#FFDB00FF",
                  "#FFB600FF", "#FF9200FF", "#FF6D00FF", "#FF4900FF",
                  "#FF2400FF", "#FF0000FF")

  raster::plot(cropped_map,
               addfun = fun,
               col = heatcolors,
               breaks = round(seq(from = raster_min,
                                  to = raster_max,
                                  length.out = 10), 0))

}
