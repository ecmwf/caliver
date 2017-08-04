#' @title read_risico_binary
#'
#' @description Reads a RISICO output file and returns a raster map
#'
#' @param filename is the file to read
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   r_values <- read_risico_binary("data/RISICO2015_VPPF_201611140300")
#' }
#'

read_risico_binary <- function(filename){
  
  myFile <- gzfile(filename, "rb")
  grid_type <- readBin(myFile, integer(), 1)

  if (grid_type == 1) {

    #regular grid
    grid_size <- readBin(myFile, "integer", 2, size = 4)
    n_values <- grid_size[1]* grid_size[2]

    lats <- readBin(myFile, "double", 2, size = 4)
    lons <- readBin(myFile, "double", 2, size = 4)	

    values_vect <- readBin(myFile, "double", n_values, size = 4)
    values_vect[values_vect == -9999] <- NA

    vals <- matrix(values_vect, nrow = grid_size[1], byrow = T)
    vals <- vals[nrow(vals):1, 1:ncol(vals)]

    r_values <- raster::raster(vals, xmn = lons[1], xmx = lons[2],
                               ymn = lats[1], ymx = lats[2])

  }

  close(myFile)

  return(r_values)

}
