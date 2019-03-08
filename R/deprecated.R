#' @title decompress_gz (DEPRECATED)
#'
#' @description This function decompresses all .gz files in a given folder
#'
#' @param input_dir directory path where all the files to read have been stored
#'
#' @examples
#' \dontrun{
#'   decompress_gz(input_dir = "/var/tmp/moc0/forestfire/")
#' }
#'

decompress_gz <- function(input_dir = getwd()){
  
  .Deprecated("decompress_gz()")
  message("NOTE: This function will be removed in the next release.")
  
  list_of_gzfiles <- list.files(path = input_dir,
                                pattern = "*.gz", full.names = TRUE)
  
  # Decompress any gz files
  for (i in seq_along(list_of_gzfiles)){
    
    R.utils::gunzip(list_of_gzfiles[i], overwrite = TRUE)
    
  }
  
}


#' @title import_geff_data_from_tar (DEPRECATED)
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
  
  .Deprecated("import_geff_data_from_tar()")
  message("NOTE: This function will be removed in the next release.")
  
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


#' @title mean_percs (DEPRECATED)
#'
#' @description Calculate average percentile
#'
#' @param vals is the a raster object
#' @param perc_val is the percentile value used as a threshold
#' @param mod defines if the values considered for the mean are above ("gt") or
#' below ("lt") the threshold
#'

mean_percs <- function(vals, perc_val, mod){
  
  .Deprecated("mean_percs()")
  message("NOTE: This function will be removed in the next release.")
  
  .Deprecated(msg = "This function is deprecated.")
  
  p_val <- stats::quantile(vals, perc_val / 100, na.rm = TRUE)
  
  if (mod == "gt") {
    
    v_perc <- vals[vals >= p_val]
    
  } else if (mod == "lt") {
    
    v_perc <- vals[vals <= p_val]
    
  } else {
    
    stop("mod should be 'lt' or 'gt'")
    
  }
  
  return(mean(v_perc, na.rm = TRUE))
  
}


#' @title stack_netcdf_files (DEPRECATED)
#'
#' @description This function merges all the netcdf files in a given directory
#' over the time dimension. It saves the merged file in the working directory.
#'
#' @param input_dir is the directory where all the files to read are stored
#' @param varname name of the variable to extract
#' @param pattern regular expression pattern to select a subset of files
#' @param recursive logical (TRUE by default). If set to TRUE it looks in
#' folders and subfolders
#' @param output_file output filename (including path)
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Mergetime using single variable nc files
#'   stack_netcdf_files(input_dir = "/var/tmp/moc0/forestfire",
#'                      varname = NULL,
#'                      pattern = "geff_reanalysis_an_fwis_fwi_",
#'                      recursive = TRUE,
#'                      output_file = "outfile.nc")
#' }
#'

stack_netcdf_files <- function(input_dir = NULL,
                               varname = NULL,
                               pattern = NULL,
                               recursive = FALSE,
                               output_file = NULL){
  
  .Deprecated("mean_percs()")
  message("NOTE: This function will be removed in the next release.")
  
  if (is.null(input_dir)) stop("Please specify data folder 'input_dir'!")
  
  ifiles <- list.files(path = input_dir,
                       pattern = pattern,
                       recursive = recursive,
                       full.names = TRUE)
  
  if (is.null(output_file)) output_file <- tempfile(fileext = ".nc")
  
  if (is.null(varname)) varname <- ""
  s <- raster::stack(x = ifiles, varname = varname)
  
  if (!is.null(output_file)) {
    raster::writeRaster(s, filename = output_file,
                        format = "CDF", overwrite = TRUE)
  }
  
  return(s)
  
}
