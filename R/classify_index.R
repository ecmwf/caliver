#' @title classify_index
#'
#' @description This function classifies a fire danger index based on user
#' defined danger thresholds. If \code{index} is one of GEFF's indices (fwi,
#' ffmc, dmc, dc, isi, bui), the user does not need to specify thresholds and
#' labels. Default thresholds and labels are those defined by JRC for Europe:
#' https://effis.jrc.ec.europa.eu/about-effis/technical-background/fire-danger-forecast/
#' For instance if index = "fwi", default thresholds and labels are:
#' very Low = 0-5.2, low = 5.2-11.2, moderate = 11.2-21.3, high = 21.3-38.0,
#' very high = 38-50.0, extreme = > 50.
#' However, if threshold != NULL, the default values will be overwritten by the
#' custom values.
#'
#' @param r is the Raster* object to classify.
#' @param index this is the code of the fire danger index.
#' @param thresholds numeric vector containing 5 thresholds.
#' @param labels string of characters to be used as labels
#'
#' @return The function returns a Raster* object of the same dimensions of
#' \code{r} but the values are categorical from 1 to 6 (corresponding to
#' very low, low, moderate, high, very high and extreme danger, respectively).
#' Some cells may contain NAs, this happens typically over the sea.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   r <- readRDS(system.file("extdata", "RISICO_raster.rds", package = "caliver"))
#'   x <- classify_index(r, index = "fwi")
#'
#'   # This can be plotted using rasterVis::levelplot()
#'   rasterVis::levelplot(x)
#' }
#'

classify_index <- function(r, index = NULL, thresholds = NULL, labels = NULL){

  if (index == "fwi" & (is.null(thresholds) | is.null(labels))){
    thresholds <- c(5.2, 11.2, 21.3, 38, 50)
    labels <- c("Very low", "Low", "Moderate", "High", "Very high", "Extreme")
  }
  
  if (index == "ffmc" & (is.null(thresholds) | is.null(labels))){
    thresholds <- c(82.7, 86.1, 89.2, 93)
    labels <- c("Very low", "Low", "Moderate", "High", "Very high")
  }
  
  if (index == "dmc" & (is.null(thresholds) | is.null(labels))){
    thresholds <- c(15.7, 27.9, 53.1, 140.7)
    labels <- c("Very low", "Low", "Moderate", "High", "Very high")
  }
  
  if (index == "dc" & (is.null(thresholds) | is.null(labels))){
    thresholds <- c(256.1, 334.1, 450.6, 749.4)
    labels <- c("Very low", "Low", "Moderate", "High", "Very high")
  }
  
  if (index == "isi" & (is.null(thresholds) | is.null(labels))){
    thresholds <- c(3.2, 5.0, 7.5, 13.4)
    labels <- c("Very low", "Low", "Moderate", "High", "Very high")
  }
  
  if (index == "bui" & (is.null(thresholds) | is.null(labels))){
    thresholds <- c(24.2, 40.7, 73.3, 178.1)
    labels <- c("Very low", "Low", "Moderate", "High", "Very high")
  }
  
  if (is.null(index) | is.null(thresholds) | is.null(labels)){
    stop("Please insert correct values for index, thresholds, labels")
  }
  
  index_class <- raster::cut(r, breaks = c(-Inf, thresholds, Inf))
  # Which indices are actually present in the raster?
  j <- sort(unique(raster::getValues(index_class)))

  # Ratify raster
  # Note there is a problem with the ID = 0, make sure ID starts from 1!
  r_out <- raster::ratify(index_class)
  
  # Define a Raster Attribute Table (RAT)
  rat <- data.frame(ID = seq_along(j), Index = j, Class = labels[j], stringsAsFactors = FALSE)
  
  # Assign levels
  suppressWarnings(levels(r_out) <- rat) # hide expected warning!
  
  return(r_out)
}
