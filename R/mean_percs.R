#' @title Calculate average percentile
#'
#' @description Calculate average percentile
#'
#' @param vals is the a raster object
#' @param perc.val is the percentile value used as a threshold
#' @param mod defines if the values considered for the mean are above ("gt") or
#' below ("lt") the threshold
#'

mean_percs <- function(vals, perc.val, mod){

  p_val <- stats::quantile(vals, perc.val/100, na.rm = T)

  if (mod == "gt") {

    v_perc <- vals[vals >= p_val]

  } else if(mod == "lt") {

    v_perc <- vals[vals <= p_val]

  } else {

    stop("mod should be 'lt' or 'gt'")

  }

  return(mean(v_perc, na.rm = T))

}
