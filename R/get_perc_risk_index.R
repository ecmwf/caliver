#' @title get_perc_risk_index
#'
#' @description Generates the mean of the values over a certain percentile
#' threshold for the portion of the Raster* that intersects a polygon
#'
#' @param b RasterLayer/Brick/Stack containing the historical observations or a
#' proxy (typically a reanalysis).
#' @param p SpatialPolygons* object
#' @param perc_val is the percentile value used as a threshold
#' @param mod defines if the values considered for the mean are above (gt) or
#' below (lt) the threshold
#' 
#' @return The function returns a numeric value (for each layer in \code{b}),
#' corresponding to the mean of the values of \code{b} above/below a given
#' percentile of the historical observations.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Read RISICO test data
#'   r_risico <- readRDS(system.file("extdata", "RISICO_raster.rds",
#'                                   package = "caliver"))
#'   # Set missing crs
#'   raster::crs(r_risico) <- "+proj=longlat +datum=WGS84 +no_defs"
#'   
#'   # Define dummy polygon, as sf simple feature
#'   shape <- sf::st_bbox(c(xmin = 7, xmax = 18, ymax = 40, ymin = 18),
#'                        crs = sf::st_crs(4326))
#'   
#'   get_perc_risk_index(b = r_risico, p = shape, perc_val = 75, mod = "gt")
#'   
#' }
#'

get_perc_risk_index <- function(b, p, perc_val = 75, mod = "gt"){

  # Crop and mask
  r_vals <- mask_crop_subset(b, p)

  # Percentile per layer
  ppl <- raster::quantile(x = r_vals, probs = perc_val / 100, na.rm = TRUE)

  for (i in 1:raster::nlayers(r_vals)){

    r_vals_x <- r_vals[[i]]
    # Keep only cells above (below) the threshold
    if (mod == "gt") {

      r_vals_x[r_vals_x <= ppl[i]] <- NA

    } else if (mod == "lt") {

      r_vals_x[r_vals_x >= ppl[i]] <- NA

    } else {

      stop("mod should be 'lt' or 'gt'")

    }

    if (i == 1){

      index_val <- raster::cellStats(x = r_vals_x, stat = mean, na.rm = TRUE)

    }else{

      index_val <- c(index_val,
                     raster::cellStats(x = r_vals_x, stat = mean, na.rm = TRUE))

    }

  }

  return(index_val)

}
