# https://www.ncl.ucar.edu/Document/Graphics/ColorTables/BlAqGrWh2YeOrReVi22.shtml
ncar_palette <- c("#1204F3", "#1F04F3", "#3548F5", "#549DF6",
                  "#6FEAB5", "#5DCA61", "#4FAC23", "#50A612",
                  "#74C417", "#99DD19", "#FFFFFF", "#FFFFFF",
                  "#F9FE24", "#EADF1F", "#E4C31B", "#DCA31A",
                  "#CB6013", "#C12D11", "#BE0010", "#860016",
                  "#47003D", "#28005E")

# Palette used to plot FWI in EFFIS
effis_palette <- c("#80FF7F", "#FAFF40", "#F8B002", "#F64F02", "#B40E00", "#280523")

# Round ANY number Up/Down to ANY interval
# https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
.round.choose <- function(x, roundTo, dir = 1) {
  if(dir == 1) {  ##ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if(dir == 0) {  ##ROUND DOWN
      x - (x %% roundTo)
    }
  }
}

# To plot the background map on each layers of a stack we need to create a
# function and pass it to the addfun argument of raster::plot()
.background_map_fun <- function(){

  # Define a background map
  background_map <- rworldmap::getMap(resolution = "low")

  # Plotting function
  raster::plot(background_map, add = TRUE, border = "darkgray")

}

# Calculate default quantiles for single hazard assessment
.quant_function <- function(x, probs = NULL){

  # Default probs
  if (is.null(probs)) {probs <- c(0.75, 0.85, 0.90, 0.95, 0.98)}

  raster::quantile(x, probs = probs, na.rm = TRUE)

}

# Convert longitudes from -180,+180 to 0,360 range
.convert_long_from_180_to_360 <- function(long){
  long <- ifelse(long < 0, 360 + long, long)
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
  rat$danger <- factor(x = rat$danger, levels = classes)
  names(rat) <- c("ID", "Class")
  
  return(rat)
  
}

.get_layers_for_clima <- function(b, raster_date = NULL, expand = TRUE){
  
  # which indices correspond to day j?
  idx <- which(substr(names(b), 7, 11) ==
                 gsub("-", ".", substr(as.character(raster_date), 6, 10)))
  
  if (expand == TRUE){
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
    if (any(idx_vector > raster::nlayers(b))){
      elements2remove <- which(idx_vector > raster::nlayers(b))
      idx_vector <- idx_vector[-elements2remove]
    }
    
    idx_vector <- sort(unique(idx_vector))
    
    if (length(idx_vector) < length(idx) * 9){
      message(paste0("Caution: climatology for the ",
                     format(raster_date, "%B %d"),
                     " is calculated using ", length(idx_vector),
                     " days rather then ", length(idx) * 9, "!"))
    }
  } else {
    idx_vector <- idx
  }
  
  # Collection of layers spanning the date of interest +/- 4 days & all years
  clima_sub <- b[[idx_vector]]
  
  return(clima_sub)
  
}

# EXTRA utility functions for multi-hazard assessments

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

# EXTRA utility functions for multi-hazard assessments

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
