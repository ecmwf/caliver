#' @title validate_fire_danger_levels
#'
#' @description This function compares observed and modelled fire data and
#' return a contingency table summarising the hit rates, false alarms, misses
#' and correct negatives. The validation can be done using various thresholds
#' and input data.
#'
#' @param fire_index RasterBrick containing the fire index (only one variable)
#' @param observation RasterBrick containing the observation (only one variable)
#' @param fire_threshold threshold to use to select relevant fire indices
#' @param obs_threshold threshold to use to select relevant observations
#'
#' @return A contingency table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   setwd("/scratch/mo/moc0/fire/")
#'
#'   # Load and subset FWI
#'   BurnedAreas <- raster::brick("GFED4_BurnedAreas/BurnedArea.grd")
#'   names(BurnedAreas) <- seq.Date(from = as.Date("2003-01-01"),
#'                                  to = as.Date("2015-12-31"), by = "day")
#'   FWI <- raster::brick('GEFF/reanalysis/FWI_1980-2016.nc')
#'
#'   # Generate the contingency table
#'   ct <- validate_fire_danger_levels(fire_index = FWI,
#'                                  observation = BurnedAreas,
#'                                  fire_threshold = 10, obs_threshold = 50)
#'
#' }
#'

validate_fire_danger_levels <- function(fire_index,
                                        observation,
                                        fire_threshold,
                                        obs_threshold){

  if (!("RasterBrick" %in% class(fire_index)) &
      !("RasterStack" %in% class(fire_index)) &
      !("RasterLayer" %in% class(fire_index))) {

    stop("Error: the fire_index can only be a raster brick/stack")

  }

  if (!("RasterBrick" %in% class(observation)) &
      !("RasterStack" %in% class(observation)) &
      !("RasterLayer" %in% class(observation))) {

    stop("Error: the observation can only be a raster brick/stack")

  }

  # RasterLayer
  if ("RasterLayer" %in% class(fire_index)) {

    fwi_brick <- fire_index

  }

  if ("RasterLayer" %in% class(observation)) {

    obs_brick <- observation

  }

  # RasterStack
  if ("RasterStack" %in% class(fire_index)) {

    message("Convert stack of fire indices into a raster brick")
    fwi_brick <- raster::brick(fire_index, progress = "text")

  }

  if ("RasterStack" %in% class(observation)) {

    message("Convert stack of observations into a raster brick")
    obs_brick <- raster::brick(observation, progress = "text")

  }

  # RasterBrick
  if ("RasterBrick" %in% class(fire_index)) {

    fwi_brick <- fire_index

  }

  if ("RasterBrick" %in% class(observation)) {

    obs_brick <- observation

  }

  # Aggregate/Resample observations to match the resolution of the fire index
  fact <- round(dim(obs_brick)[1:2] / dim(fwi_brick)[1:2], 0)
  message("Aggregate observations to match the resolution of the fire index")
  burned_areas <- raster::aggregate(obs_brick, fact, fun = sum,
                                    progress = "text")
  message("Resample observations to match the resolution of the fire index")
  burned_areas_resampled <- raster::resample(burned_areas, fwi_brick,
                                  method = "bilinear", progress = "text")

  # Select only period in common
  fwi_index <- which(names(fwi_brick) %in% names(burned_areas_resampled))
  obs_index <- which(names(burned_areas_resampled) %in% names(fwi_brick))
  seasonal_fwi <- raster::subset(fwi_brick, fwi_index)
  seasonal_obs <- raster::subset(burned_areas_resampled, obs_index)

  message("Calculating contingency table")
  # Transform BurnedAreas to binary (is BurnedAreas > 50 hectares?)
  seasonal_obs_vector <- as.vector(seasonal_obs)
  seasonal_obs_vector_logical <- ifelse(test = (seasonal_obs_vector >=
                                                  obs_threshold),
                                        yes = TRUE, no = FALSE)

  # Transform FWI to binary (Is FWI > high danger level?)
  seasonal_fwi_vector <- as.vector(seasonal_fwi)
  seasonal_fwi_vector_logical <- ifelse(test = (seasonal_fwi_vector >=
                                                  fire_threshold),
                                        yes = TRUE, no = FALSE)

  return(table(seasonal_fwi_vector_logical, seasonal_obs_vector_logical))

}
