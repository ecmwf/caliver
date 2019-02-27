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

    input_file <- NULL
    message("You have defined r and input_file, ignoring input_file!")

  }

  if (is.null(r) & !is.null(input_file)) r <- raster::brick(input_file)

  if (!is.null(r)) {

    if (all(probs > 1)) probs <- probs / 100
    quant_fun <- function(x) quantile(x, probs = probs, na.rm = TRUE)

    perc_maps <- raster::calc(r, fun = quant_fun, progress = "text")

    return(perc_maps)

  }

}
