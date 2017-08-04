#' @title import_geff_data_from_tar
#'
#' @description This function imports GEFF data (reanalysis or realtime) from a
#' tar file into a raster stack. Depending on the user's query to the web
#' portal, the stack can contain different variables.
#'
#' @param archive file path to the tar file
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   s <- import_geff_data_from_tar(archive = "test.tar")
#' }
#'

import_geff_data_from_tar <- function(archive){

  # From .tar to .gz, and finally to .nc
  my_temp_dir <- tempdir()
  utils::untar(tarfile = archive, exdir = my_temp_dir)

  list_of_gzfiles <- list.files(path = my_temp_dir,
                                pattern = "*.gz", full.names = TRUE)

  # Decompress any gz files
  for (i in seq_along(list_of_gzfiles)){

    R.utils::gunzip(list_of_gzfiles[i])

  }

  s <- raster::stack(list.files(path = my_temp_dir,
                                pattern = "*.nc",
                                full.names = TRUE))

  unlink(my_temp_dir)

  closeAllConnections()

  return(raster::brick(s))

}
