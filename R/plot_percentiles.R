#' @title plot_percentiles
#'
#' @description This function plots the maps of percentiles
#'
#' @param maps is the result of get_percentile_raster()
#' @param region string of characters describing the region.
#' @param col custom color palette (default is `dput(rev(RColorBrewer::brewer.pal(n = 10, name = "RdYlGn")))`)
#' @param ... additional graphical parameters inherited from plot() in the
#' raster package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Use default palette
#'   plot_percentiles(maps)
#'   
#'   # Use custom color palette
#'   plot_percentiles(maps, col = rev(viridis::inferno(n = 10)))
#' }
#'

plot_percentiles <- function(maps, region = "GLOB", col = NULL, ...){

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

  raster::plot(cropped_map, addfun = .background_map_fun, breaks = breaks,
               col = col, ...)

}
