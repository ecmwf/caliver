#' @title stack_netcdf_files
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
#'                      output_file = "~/outfile.nc")
#' }
#'

stack_netcdf_files <- function(input_dir = NULL,
                               varname = NULL,
                               pattern = NULL,
                               recursive = FALSE,
                               output_file = NULL){

  if (is.null(input_dir)) {

    stop("Please specify data folder 'input_dir'!")

  }

  ifiles <- paste(list.files(path = input_dir,
                             pattern = pattern,
                             recursive = recursive,
                             full.names = TRUE),
                  collapse = " ")

  if (is.null(output_file)) {

    output_file <- file.path(getwd(), "outfile.nc")

  }

  if (is.null(varname)) {

    # Mergetime opens all the file to order them over time, while
    # Cat is computational lighter than mergetime because it opens 1 file at the
    # time and assumes they are already ordered (e.g. due to naming convention)

    system(paste0("cdo cat ", ifiles, " ", output_file))

  } else {

    system(paste0("cdo select,name=", varname, " ", ifiles, " ", output_file))

  }

  return(output_file)

}
