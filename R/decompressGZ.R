#' @title decompressGZ
#'
#' @description This function decompresses all .gz files in a given folder
#'
#' @param dirs is the directory where all the files to read are stored
#' @param keep logical, if TRUE it keeps the .gz files, removes them otherwise
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   decompressGZ(dirs = "/var/tmp/moc0/forestfire/")
#' }
#'

decompressGZ <- function(dirs = getwd(), keep = FALSE){
  
  # Decompress any gz files but keep originals
  for (i in list.files(path = dirs, pattern = "*.gz", full.names = TRUE)){
    
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
