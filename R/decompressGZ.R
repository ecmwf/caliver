#' @title decompressGZ
#'
#' @description This function decompresses all .gz files in a given folder
#'
#' @param inDir directory path where all the files to read have been stored
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   decompressGZ(inDir = "/var/tmp/moc0/forestfire/")
#' }
#'

decompressGZ <- function(inDir = getwd()){

  list_of_gzfiles <- list.files(path = inDir,
                                pattern = "*.gz", full.names = TRUE)

  # Decompress any gz files
  for (i in seq_along(list_of_gzfiles)){

    R.utils::gunzip(list_of_gzfiles[i])

  }

}
