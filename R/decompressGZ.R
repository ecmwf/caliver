#' @title decompressGZ
#'
#' @description This function decompresses all .gz files in a given folder
#'
#' @param inDir is the directory path where all the files to read have been previously stored
#' @param keep logical, if TRUE it keeps the .gz files, removes them otherwise
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   decompressGZ(inDir = "/var/tmp/moc0/forestfire/")
#' }
#'

decompressGZ <- function(inDir = getwd(), keep = FALSE){

  # Decompress any gz files but keep originals
  for (i in list.files(path = inDir, pattern = "*.gz", full.names = TRUE)){

    if (substr(i, nchar(i) - 2, nchar(i)) == ".gz" &
        Sys.which("gunzip")[[1]] != ""){

      if (keep == TRUE){

        system(paste("gunzip -k", i))

      }else{

        system(paste("gunzip", i))

      }

    }
  }

}
