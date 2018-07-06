background_map_fun <- function(){

  # We want to plot the background map on each layers of the stack, so we need
  # to create a function and pass it to the addfun argument
  # (see ?plot in the raster package)

  # Define a background map
  background_map <- rworldmap::getMap(resolution = "low")

  # Plotting function
  plot(background_map, add = TRUE, border = "lightgray")

}

quant_function <- function(x){

  # Default probs
  default_probs <- c(0.50, 0.75, 0.85, 0.90, 0.95, 0.98)

  quantile(x, probs = default_probs, na.rm = TRUE)

}
