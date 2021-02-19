#' @title relative_humidity
#'
#' @description Calculate relative humidity from 2m temperature (in Celsius) and
#' 2m dew point temperature (in Celsius).
#'
#' @param t2m 2m temperature (in Celsius)
#' @param d2m 2m dew point temperature (in Celsius)
#' @param unit can be "Celsius" (default) or "Kelvin"
#' @param method can be "August-Roche-Magnus" (default) or "Clausius-Clapeyron"
#' 
#' @return The function returns a numeric, with length equal to t2m (and d2m).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   relative_humidity(t2m = 30, d2m = 25,
#'                     unit = "Celsius",
#'                     method = "August-Roche-Magnus")
#' }
#'

relative_humidity <- function(t2m, d2m,
                              unit = "Celsius",
                              method = "August-Roche-Magnus"){
  
  # Constraint: t2m is always bigger than d2m
  if (t2m < d2m){
    
    message("Caution: t2m should be bigger than d2m!")
    rh <- NULL
    
  } else {
    # Both methods use temperature in Celsius.
    if (unit == "Kelvin") {
      t2m <- t2m - 273.15
      d2m <- d2m - 273.15
    }
    
    if (method == "August-Roche-Magnus"){
      # Values are calculated using the August-Roche-Magnus approximation.
      numerator <- exp((17.625 * (d2m)) / (243.04 + (d2m)))
      denominator <- exp((17.625 * (t2m)) / (243.04 + (t2m)))
      rh <- (numerator * 100)/denominator
    }
    
    if (method == "Clausius-Clapeyron"){
      # Values are calculated using the Clausius-Clapeyron relation.
      numerator <- 6.11 * 10 ^ (7.5 * ((d2m)/(237.7 + d2m)))
      denominator <- 6.11 * 10 ^ (7.5 * ((t2m)/(237.7 + t2m)))
      rh <- (numerator * 100)/denominator
    }
  }
  
  return(rh)
  
}
