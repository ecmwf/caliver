#' @title stack_with_rat
#'
#' @description This function converts the Raster* object that contains
#' categorical values to a stack and associates a Raster Attribute Table (rat).
#' This is useful if plotting using the \code{rasterVis::levelplot()} function.
#'
#' @param r is the categorical Raster* object.
#' @param ids numeric vector containing the categorical levels.
#' @param classes character vector containing the categorical labels.
#'
#' @return The function returns a RasterStack of the same dimensions of
#' \code{r} but with associated rat.
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
  rat <- data.frame(id = ids, danger = classes, stringsAsFactors = FALSE)
  rat$id <- factor(x = rat$id, levels = ids)
  rat$danger <- factor(x = rat$danger, levels = classes)
  names(rat) <- c("ID", "Danger")

  # Transform the brick into a categorical stack of layers
  index_stack <- raster::stack()
  for (i in 1:nlayers(r)){
    # there is a problem with the ID = 0, they are resetted to start from 1
    if (cellStats(r[[i]], min) == 0){
      ri <- r[[i]] + 1
      tmp <- raster::ratify(ri)
    }else{
      tmp <- raster::ratify(r[[i]])
    }
    levels(tmp) <- rat
    index_stack <- raster::stack(index_stack, tmp)
  }

  return(index_stack)

}
