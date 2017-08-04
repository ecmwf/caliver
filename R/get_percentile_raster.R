#' @title get_percentile_raster
#'
#' @description This function calculates the given percentile at each grid point
#'
#' @param input_file_path is the name of the file(path) to read
#' @param probs numeric vector of probabilities with values in [0,100] listing
#' which percentiles should be calculated
#' @param outDir is the directory where the output nc files are saved, by
#' default this is a temporary directory.
#' 
#' @return list containing all the generated percentile maps
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   x <- get_percentile_raster(input_file_path = "./outfile.nc",
#'                             probs = c(50, 75, 90, 99),
#'                             outDir = getwd())
#' }
#'

get_percentile_raster <- function(input_file_path,
                                 probs,
                                 outDir = tempdir()){

  stackedMaps <- raster::stack() 

  for (i in 1:length(probs)) {

    prob <- probs[i]

    fileName <- tools::file_path_sans_ext(basename(input_file_path))

    output_file <- file.path(outDir, paste0(fileName, "_", prob, ".nc"))

    system(paste0("cdo timpctl,", prob, " ", input_file_path,
                  " -timmin ", input_file_path,
                  " -timmax ", input_file_path, " ", output_file))

    probRaster <- raster::raster(output_file)

    varname <- names(ncdf4::nc_open(input_file_path)$var)
    names(probRaster) <- paste0(toupper(varname), prob)

    if (length(probs) > 1) {

      stackedMaps <- raster::stack(stackedMaps, probRaster)

    } else {

      stackedMaps <- probRaster

    }

  }

  return(stackedMaps)

}
