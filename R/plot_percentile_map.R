#' @title plot_percentile_map
#'
#' @description This function plots the maps of percentiles
#'
#' @param maps is the result of get_percentile_map()
#' @param region string of characters describing the region.
#' @param add_background logical, TRUE (default) to show background map.
#' This only works if longitudes of maps are in the range [-180, +180]
#' @param col custom color palette (default is `dput(rev(RColorBrewer::brewer.pal(n = 10, name = "RdYlGn")))`)
#' @param ... additional graphical parameters inherited from plot() in the
#' raster package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Generate dummy RasterLayer
#'   r <- raster(nrows = 2, ncols = 2, xmn = 0, xmx = 360, ymn = -90, ymx = 90, vals = 30)
#'   # Generate dummy RasterBrick
#'   b <- raster::brick(lapply(1:(365 * 3),
#'                      function(i) raster::setValues(r,
#'                      runif(n = raster::ncell(r), min = 0, max = 100))))
#'   # Get percentile maps
#'   maps <- get_percentile_map(b, probs = c(0.50, 0.75, 0.90, 0.99))
#'  
#'   # Use default palette
#'   plot_percentile_map(maps)
#' }
#'

plot_percentile_map <- function(maps, region = "GLOB", add_background = FALSE,
                                col = NULL, ...){

  if (region != "GLOB") {

    BasisRegions <- readRDS(system.file("extdata", "GFED4_BasisRegions.rds",
                                        package = "caliver"))
    mask_map <- BasisRegions[BasisRegions$Region == region, ]
    cropped_map <- raster::trim(raster::mask(maps, mask_map))

  } else {

    cropped_map <- maps

  }

  raster_max <- max(raster::cellStats(cropped_map, stat = "max", na.rm = TRUE))
  # Round UP to the nearest 10th
  raster_max <- .round.choose(raster_max, 5, 1)
  
  breaks <- round(seq(from = 0, to = raster_max, length.out = 10), 0)
  breaks <- unique(breaks)
  
  if (is.null(col)){col = c("#006837", "#1A9850", "#66BD63", "#A6D96A",
                            "#D9EF8B", "#FEE08B", "#FDAE61", "#F46D43",
                            "#D73027", "#A50026")}

  if (add_background == TRUE) {
    raster::plot(cropped_map, addfun = .background_map_fun, breaks = breaks,
                 col = col, ...)
  } else {
    raster::plot(cropped_map, breaks = breaks, col = col, ...)
  }

}
