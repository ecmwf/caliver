#' @title import_geff_data_from_tar
#'
#' @description This function imports GEFF data (reanalysis or realtime) from a
#' tar file into a raster stack. Depending on the user's query to the web
#' portal, the stack can contain different variables.
#'
#' @param archive file path to the tar file
#' @param stack_ncfiles logical (TRUE by default) variable to decided wheteher
#' decompressed nc files should be stacked or not.
#'
#' @return If \code{stack_ncfiles} is TRUE, the function returns a RasterBrick.
#' If \code{stack_ncfiles} is FALSE, it returns the list of uncompressed files.
#'
#' @examples
#' \dontrun{
#'   s <- import_geff_data_from_tar(archive = "test.tar")
#' }
#'

import_geff_data_from_tar <- function(archive, stack_ncfiles = TRUE){

  # From .tar to .gz, and finally to .nc
  my_temp_dir <- tempdir()
  list_of_gzfiles <- file.path(my_temp_dir, utils::untar(tarfile = archive,
                                                         exdir = my_temp_dir,
                                                         list = TRUE))

  # Untar files
  utils::untar(tarfile = archive, exdir = my_temp_dir)

  # Decompress any gz files
  for (i in seq_along(list_of_gzfiles)){

    R.utils::gunzip(list_of_gzfiles[i])

  }

  if (stack_ncfiles == TRUE){

    s <- raster::stack(tools::file_path_sans_ext(list_of_gzfiles))

    return(raster::brick(s))

  } else {

    return(tools::file_path_sans_ext(list_of_gzfiles))

  }

}
