#' @title get_percentile_raster
#'
#' @description This function calculates percentile(s) at each grid point.
#'
#' @param probs numeric vector of probabilities with values in [0,100] listing
#' which percentiles should be calculated.
#' @param r Raster* object (either RasterStack or RasterBrick).
#' @param input_file is the name of the file(path) to read.
#' @param output_dir is the directory where the output nc files are saved, by
#' default this is a temporary directory.
#'
#' @return list containing all the generated percentile maps
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   x <- get_percentile_raster(probs = c(50, 75, 90, 99),
#'                              input_file = "./outfile.nc",
#'                              output_dir = getwd())
#' }
#'

get_percentile_raster <- function(probs,
                                  r = NULL,
                                  input_file = NULL,
                                  output_dir = tempdir()){

  if (all(is.null(r), is.null(input_file))) {

    stop("Please define either an r or input_file")

  }

  if (all(!is.null(r), !is.null(input_file))) {

    stop(paste("Please define either an r or input_file,",
               "the other must be NULL"))

  }

  if (!is.null(r)) {

    fun <- function(x) {

      quantile(x, probs = probs / 100, na.rm = TRUE)

    }

    stacked_maps <- raster::calc(r, fun)

    names(stacked_maps) <- paste0("FWI", probs)

    return(stacked_maps)

  } else if (!is.null(input_file)) {

    # Use cdo to stack rasters

    stacked_maps <- raster::stack()

    for (i in 1:length(probs)) {

      prob <- probs[i]

      file_name <- tools::file_path_sans_ext(basename(input_file))

      output_file <- file.path(output_dir, paste0(file_name, "_", prob, ".nc"))

      system(paste0("cdo timpctl,", prob, " ", input_file,
                    " -timmin ", input_file,
                    " -timmax ", input_file, " ", output_file))

      probability_raster <- raster::raster(output_file)

      varname <- names(ncdf4::nc_open(input_file)$var)
      names(probability_raster) <- paste0(toupper(varname), prob)

      if (length(probs) > 1) {

        stacked_maps <- raster::stack(stacked_maps, probability_raster)

      } else {

        stacked_maps <- probability_raster

      }

    }

    if (length(probs) > 1) {

      return(raster::brick(stacked_maps))

    } else {

      return(stacked_maps)

    }

  }

}
