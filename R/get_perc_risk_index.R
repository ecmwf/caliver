#' @title get_perc_risk_index
#'
#' @description Generates the mean of the values over a certain percentile
#' threshold for the portion of the Raster* that intersects a polygon
#'
#' @param r_stack is the raster or raster stack
#' @param p_shape is the shapefile on which to aggregate the values
#' @param perc_val is the percentile value used as a threshold
#' @param mod defines if the values considered for the mean are above (gt) or
#' below (lt) the threshold
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   r.index <- get_perc_risk_index(r_stack = r_stack, p_shape = poly,
#'                               perc_val = 50, mod = "lt")
#' }
#'

get_perc_risk_index <- function(r_stack, p_shape, perc_val = 75, mod = "gt"){

  # Crop and mask
  r_vals <- mask_crop_subset(r_stack, p_shape)

  # Percentile per layer
  ppl <- raster::quantile(x = r_vals, probs = perc_val/100, na.rm = TRUE)

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
