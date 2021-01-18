#' @title stack_with_rat
#'
#' @description This function converts the Raster* object that contains
#' categorical values to a stack and associates a Raster Attribute Table (rat).
#' This is useful if plotting using the \code{rasterVis::levelplot()} function.
#'
#' @param r is the categorical RasterLayer object (does not work with bricks!).
#' @param ids numeric vector containing the categorical levels.
#' @param classes character vector containing the categorical labels.
#'
#' @return The function returns a RasterStack of the same dimensions of
#' \code{r} but with associated rat.
#' 
#' @details Note there is a problem with the ID = 0, always start ID from 1!
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   x <- stack_with_rat(r, ids = 1:3, classes = c("Low", "Moderate", "High")
#' }
#'

stack_with_rat <- function(r, ids, classes){

  # Define a Raster Attribute Table (RAT)
  rat <- .create_rat(ids, classes)

  # Ratify
  r_out <- raster::ratify(r)

  # Assign levels
  levels(r_out) <- rat

  return(r_out)

}
