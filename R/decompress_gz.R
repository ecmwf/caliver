#' @title decompress_gz
#'
#' @description This function decompresses all .gz files in a given folder
#'
#' @param input_dir directory path where all the files to read have been stored
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   decompress_gz(input_dir = "/var/tmp/moc0/forestfire/")
#' }
#'

decompress_gz <- function(input_dir = getwd()){

  list_of_gzfiles <- list.files(path = input_dir,
                                pattern = "*.gz", full.names = TRUE)

  # Decompress any gz files
  for (i in seq_along(list_of_gzfiles)){

    R.utils::gunzip(list_of_gzfiles[i])

  }

}
