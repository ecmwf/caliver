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

.create_rat <- function(ids, classes){

  # Define a Raster Attribute Table (RAT)
  rat <- data.frame(id = ids, danger = classes, stringsAsFactors = FALSE)
  rat$id <- factor(x = rat$id, levels = ids)
  rat$category <- factor(x = rat$danger, levels = classes)
  names(rat) <- c("ID", "Class")

  return(rat)

}

.get_layers_for_clima <- function(r, single_date){

  # which indices correspond to day j?
  idx <- which(substr(names(r), 7, 11) ==
                 gsub("-", ".", substr(as.character(single_date), 6, 10)))

  # Do not take the single day but the period spanning 4 days before and
  # 4 days after the given date
  idx_vector <- c()
  for (k in seq_along(idx)){
    idx_vector <- c(idx_vector, (idx[k] - 4):(idx[k] + 4))
  }
  if (any(idx_vector <= 0)){
    elements2remove <- which(idx_vector <= 0)
    idx_vector <- idx_vector[-elements2remove]
  }
  if (any(idx_vector > raster::nlayers(r))){
    elements2remove <- which(idx_vector > raster::nlayers(r))
    idx_vector <- idx_vector[-elements2remove]
  }

  idx_vector <- sort(unique(idx_vector))

  if (length(idx_vector) < length(idx) * 9){
    message(paste0("Caution: climatology for the ", single_date,
                   " is calculated using ", length(idx_vector),
                   " days rather then ", length(idx) * 9, "!"))
  }

  # Collection of layers spanning the date of interest +/- 4 days & 37 years
  r_sub <- r[[idx_vector]]

  return(r_sub)

}

# Convert longitudes from -180,+180 to 0,360 range
.convert_long_from_180_to_360 <- function(long){
  long <- ifelse(long < 0, 360 + long, long)
}

# Center the palette on zero
# p <- levelplot(fwi_anomaly)
.diverge0 <- function(p, ramp = 'RdBu', len_legend = 20) {
  # p: a trellis object resulting from rasterVis::levelplot
  # ramp: the name of an RColorBrewer palette (as character), a character
  #       vector of colour names to interpolate, or a colorRampPalette.
  require(RColorBrewer)
  require(rasterVis)
  if(length(ramp)==1 && is.character(ramp) && ramp %in%
     row.names(brewer.pal.info)) {
    ramp <- suppressWarnings(colorRampPalette(rev(brewer.pal(11, ramp))))
  } else if(length(ramp) > 1 && is.character(ramp) && all(ramp %in% colors())) {
    ramp <- colorRampPalette(ramp)
  } else if(!is.function(ramp))
    stop('ramp should be either the name of a RColorBrewer palette, ',
         'a vector of colours to be interpolated, or a colorRampPalette.')
  rng <- range(p$legend[[1]]$args$key$at)
  s <- seq(-max(abs(rng)), max(abs(rng)), len=len_legend)
  i <- findInterval(rng[which.min(abs(rng))], s)
  zlim <- switch(which.min(abs(rng)), `1`=i:(len_legend), `2`=1:(i+1))
  p$legend[[1]]$args$key$at <- s[zlim]
  p$par.settings$regions$col <- ramp(len_legend - 1)[zlim[-length(zlim)]]
  p
}
