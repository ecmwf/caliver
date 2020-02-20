#' @title point_inspection
#'
#' @description This function inspects reanalysis and forecasts for a given set
#' of points.
#'
#' @param issue_date x
#' @param points x
#' @param maxleadtime x
#' @param fc_hres x
#' @param fc_ens x
#' @param rea_hres x
#' @param rea_ens x
#'
#' @return A list of named arrays returning the reanalysis and forecast values.
#'
#' @examples
#' \dontrun{
#'   # Before using this function make sure points and all the rasters overlap!
#'   # Very often points have the longitude in the range -180, +180
#'   # while rasters have longitudes in the range 0, 360.
#'
#'   # Define spatial points
#'   points <- sf::st_as_sf(x = data.frame(long = c(282.88, 171.58, 354.93,
#'                                                  105.18, 347.52),
#'                                         lat = c(-12.02, -41.74, 7.73,
#'                                                 -5.1, 22.75)),
#'                          coords = c("long", "lat"), crs = 4326)
#'   point_inspection(issue_date = as.Date("2017-01-01"),
#'                    points,
#'                    maxleadtime = 10,
#'                    fc_hres = "/hugetmp/forecasts/hres",
#'                    fc_ens = "/hugetmp/forecasts/ens",
#'                    rea_hres = "/hugetmp/reanalysis/hres",
#'                    rea_ens = "/hugetmp/reanalysis/ens")
#'
#' }
#'

point_inspection <- function(issue_date, points, maxleadtime = 10,
                             fc_hres = NULL, fc_ens = NULL,
                             rea_hres = NULL, rea_ens = NULL){

  list_to_be_returned <- list()

  # Get fc_hres
  if (!is.null(fc_hres)){
    f <- list.files(path = fc_hres,
                    pattern = gsub("-", "", issue_date),
                    full.names = TRUE)
    HRES <- raster::brick(f)
    list_to_be_returned[["fc_hres"]] <- raster::extract(x = HRES, y = points)
  }

  # Get fc_ens
  if (!is.null(fc_ens)){
    arr_fc_ens <- array(NA, dim = c(dim(points)[1], maxleadtime, 51))
    for (j in 1:51){
      f <- list.files(path = fc_ens,
                      pattern = gsub("-", "", issue_date),
                      full.names = TRUE)[j]
      ENS <- raster::brick(f)[[1:maxleadtime]]
      arr_fc_ens[, , j] <- raster::extract(x = ENS, y = points)
    }
    list_to_be_returned[["fc_ens"]] <- arr_fc_ens
  }

  # Get rea_hres
  if (!is.null(rea_hres)){
    df_rea <- matrix(NA, nrow = dim(points)[1], ncol = maxleadtime)
    for (leadtime in 1:maxleadtime){
      lead_day <- issue_date + leadtime - 1
      f <- list.files(path = rea_hres,
                      pattern = gsub("-", "", lead_day),
                      full.names = TRUE)
      ERA5_HRES <- raster::brick(f)
      df_rea[, leadtime] <- raster::extract(x = ERA5_HRES, y = points)
    }
    list_to_be_returned[["rea_hres"]] <- df_rea
  }

  # Get rea_ens
  if (!is.null(rea_ens)){
    arr_rea_ens <- array(NA, dim = c(dim(points)[1], maxleadtime, 10))
    for (leadtime in 1:maxleadtime){
      lead_day <- issue_date + leadtime - 1
      f <- list.files(path = rea_ens,
                      pattern = gsub("-", "", lead_day),
                      full.names = TRUE)
      ERA5_ENS <- raster::stack(f)
      arr_rea_ens[, leadtime,] <- raster::extract(x = ERA5_ENS, y = points)
    }
    list_to_be_returned[["rea_ens"]] <- arr_rea_ens
  }

  return(list_to_be_returned)

}
