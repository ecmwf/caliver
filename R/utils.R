# To plot the background map on each layers of a stack we need to create a
# function and pass it to the addfun argument of raster::plot()
.background_map_fun <- function(){

  # Define a background map
  background_map <- rworldmap::getMap(resolution = "low")

  # Plotting function
  plot(background_map, add = TRUE, border = "lightgray")

}

# Calculate default quantiles for single hazard assessment
.quant_function <- function(x){

  # Default probs
  default_probs <- c(0.75, 0.85, 0.90, 0.95, 0.98)

  raster::quantile(x, probs = default_probs, na.rm = TRUE)

}

# Utility functions for multi-hazard assessments

# Classify the danger level of an hazard
.classify_hazard <- function(x, y1, y2, y3, y4, y5){
  ifelse(x <= y1, 1,
         ifelse(x <= y2, 2,
                ifelse(x <= y3, 3,
                       ifelse(x <= y4, 4,
                              ifelse(x <= y5, 5, 6)))))
}

# Styling the UTCI layer
.utci_classification <- function(rtp){

  x <- raster::getValues(rtp)

  rtp$colour <- base::cut(x,
                          breaks = c(-Inf, -40, -27, -13, 0,
                                     9, 26, 32, 38, 46, +Inf),
                          labels = c("#000080",
                                     "#0000C0",
                                     "#0000FF",
                                     "#0060FF",
                                     "#00C0FF",
                                     "#00C000",
                                     "#f98436", # original was #FF6600
                                     "#FF3300", # original was
                                     "#910000", # original was #CC0000
                                     "#4c0000")) # original was #800000

  rtp$label <- base::cut(x,
                         breaks = c(-Inf, -40, -27, -13, 0,
                                    9, 26, 32, 38, 46, +Inf),
                         labels = c("< -40",
                                    "-40 - -27",
                                    "-27 - -13",
                                    "-13 - 0",
                                    "0 - 9",
                                    "9 - 26",
                                    "26 - 32",
                                    "32 - 38",
                                    "38 - 46",
                                    "> 46"))

  rtp$label2 <- base::cut(x,
                          breaks = c(-Inf, -40, -27, -13, 0,
                                     9, 26, 32, 38, 46, +Inf),
                          labels = c("Extreme cold stress",
                                     "Very strong cold stress",
                                     "Strong cold stress",
                                     "Moderate cold stress",
                                     "Slight cold stress",
                                     "No thermal stress",
                                     "Moderate heat stress",
                                     "Strong heat stress",
                                     "Very strong heat stress",
                                     "Extreme heat stress"))

  return(rtp)

}

# Transform the raster
.transform_raster <- function(raster_in, variable_name){

  if (variable_name == "BasisRegions"){
    # Transform the rasterBrick, transposing it
    raster_out <- raster::t(raster_in)
  }else{
    # Transform the rasterBrick, flipping it on the y direction
    raster_out <- raster::flip(raster_in, direction = "y")
  }
  # Set extent
  raster::extent(raster_out) <- raster::extent(-180, 180, -90, 90)
  # Assign CRS (WGS84)
  raster::crs(raster_out) <- "+proj=longlat +datum=WGS84 +no_defs"

  return(raster_out)

}
