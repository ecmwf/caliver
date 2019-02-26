#' @title get_gfed4
#'
#' @description Get data from the fourth-generation Global Fire Emissions
#' Database (GFED4)
#'
#' @param start_date first date to download (string)
#' @param end_date last date to download (string)
#' @param temporal_resolution temporal resolution, it can be "daily" (default)
#' or "monthly"
#' @param varname name of the variable to extract.
#' This can be one of the following:
#' \itemize{
#'   \item{"BasisRegions"}{}
#'   \item{"BurnedArea"}{}
#'   \item{"BurnedAreaUncertainty"}{}
#'   \item{"MeanBurnDateUncertainty"}{}
#'   \item{"source"}{}
#'   \item{"TreeCoverDist"}{}
#'   \item{"LandCoverDist"}{}
#'   \item{"PeatFraction"}{}
#' }
#' @param region string of characters describing the region.
#' It can only be one of the 15 values listed below:
#' \itemize{
#'   \item{"Global"}{or GLOB}
#'   \item{"Boreal North America"}{or BONA}
#'   \item{"Temperate North America"}{or TENA}
#'   \item{"Central America"}{or CEAM}
#'   \item{"Northern Hemisphere South America"}{or NHSA}
#'   \item{"Southern Hemisphere South America"}{or SHSA}
#'   \item{"Europe"}{or EURO}
#'   \item{"Middle East"}{or MIDE}
#'   \item{"Northern Hemisphere Africa"}{or NHAF}
#'   \item{"Southern Hemisphere Africa"}{or SHAF}
#'   \item{"Boreal Asia"}{or BOAS}
#'   \item{"Central Asia"}{or CEAS}
#'   \item{"Southeast Asia"}{or SEAS}
#'   \item{"Equatorial Asia"}{or EQAS}
#'   \item{"Australia and New Zealand"}{or AUST}
#' }
#'
#' @note The conversion from hdf5 to netcdf gets stuck in RStudio
#' (see \url{https://goo.gl/Rcvp9x}), please use the console.
#'
#' @return A RasterBrick (if downloading observations) or spatial polygons
#' (if downloading basis regions)
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   # Basis regions
#'   BasisRegions <- get_gfed4(varname = "BasisRegions")
#'
#'   # Only BONA region
#'   BONA <- get_gfed4(varname = "BasisRegions", region = "BONA")
#'
#'   # Monthly burned areas
#'   BurnedAreas <- get_gfed4(start_date = "2003-01-01",
#'                            end_date = "2003-01-31",
#'                            temporal_resolution = "monthly",
#'                            varname = "BurnedArea")
#'
#'   # Daily burned areas
#'   DailyBurnedAreas <- get_gfed4(start_date = "2003-01-01",
#'                                 end_date = "2003-01-02",
#'                                 temporal_resolution = "daily",
#'                                 varname = "BurnedArea")
#'
#' }
#'

get_gfed4 <- function(start_date = NULL,
                      end_date = NULL,
                      temporal_resolution = "daily",
                      varname = NULL,
                      region = "GLOB"){

  # Create a tmp directory
  my_temp_dir <- tempdir()
  if (is.null(varname)) stop("Please enter valid varname")
  if (is.null(region)) stop("Please enter valid region")
  if (varname == "BasisRegions") {

    if (is.null(region)) stop("Please enter valid region")

    base_url <- "http://www.geo.vu.nl/~gwerf/GFED/GFED4/"
    fname <- "GFED4.1s_2015.hdf5"
    the_url <- paste0(base_url, fname)
    x <- try(RCurl::getURL(the_url), silent = TRUE)

    if (class(x) == "try-error") {

      stop("Server currently unavailable, please try again later.")

    } else {

      # Download the file
      download.file(url = the_url, destfile = file.path(my_temp_dir, fname),
                    quiet = TRUE, cacheOK = TRUE, mode = "wb")

      # Extract dataset with basis regions
      file_h5 <- hdf5r::h5file(file.path(my_temp_dir, fname))
      # lets look at the content: file.h5$ls(recursive=TRUE)
      # Extract dataset
      br <- file_h5[["ancill/basis_regions"]]

      # Convert hdf5 to raster
      regions_raster <- raster::raster(br[, ])

      # Transform the raster
      regions_raster_t <- .transform_raster(regions_raster, varname)

      # Define zeros as NAs
      regions_raster_t[regions_raster_t == 0] <- NA

      if (!is.null(region)) {
        if (region == "BONA") regions_raster_t[regions_raster_t != 1] <- NA
        if (region == "TENA") regions_raster_t[regions_raster_t != 2] <- NA
        if (region == "CEAM") regions_raster_t[regions_raster_t != 3] <- NA
        if (region == "NHSA") regions_raster_t[regions_raster_t != 4] <- NA
        if (region == "SHSA") regions_raster_t[regions_raster_t != 5] <- NA
        if (region == "EURO") regions_raster_t[regions_raster_t != 6] <- NA
        if (region == "MIDE") regions_raster_t[regions_raster_t != 7] <- NA
        if (region == "NHAF") regions_raster_t[regions_raster_t != 8] <- NA
        if (region == "SHAF") regions_raster_t[regions_raster_t != 9] <- NA
        if (region == "BOAS") regions_raster_t[regions_raster_t != 10] <- NA
        if (region == "CEAS") regions_raster_t[regions_raster_t != 11] <- NA
        if (region == "SEAS") regions_raster_t[regions_raster_t != 12] <- NA
        if (region == "EQAS") regions_raster_t[regions_raster_t != 13] <- NA
        if (region == "AUST") regions_raster_t[regions_raster_t != 14] <- NA
      }

      # Trim outer NAs
      regions_raster_t_no_na <- raster::trim(regions_raster_t)

      # Convert to polygons
      regions_polygons <- raster::rasterToPolygons(x = regions_raster_t_no_na)

      return(regions_polygons)

    }

  } else {

    if (is.null(start_date)) stop("Invalid start_date")
    if (is.null(end_date)) stop("Invalid valid end_date")
    if (is.null(temporal_resolution)) stop("Invalid temporal_resolution")

    base_url <- "ftp://fuoco.geog.umd.edu/gfed4"

    if (temporal_resolution == "monthly"){

      lookuptable <- data.frame(id = 1:7,
                                varnames = c("BurnedArea",
                                             "BurnedAreaUncertainty",
                                             "source",
                                             "TreeCoverDist",
                                             "LandCoverDist",
                                             "FirePersistence",
                                             "PeatFraction"),
                                factor = c(0.01, 0.01, 1, 1, 1, 0.01, 1),
                                stringsAsFactors = FALSE)

      seq_of_dates <- seq.Date(from = as.Date(start_date),
                               to = as.Date(end_date),
                               by = "month")
      my_dates <- substr(x = gsub("-", "", as.character(seq_of_dates)),
                        start = 1, stop = 6)
      # Assemble file names
      fnms <- paste0(base_url, "/", temporal_resolution, "/",
                     "GFED4.0_MQ_", my_dates, "_BA.hdf")

    }

    if (temporal_resolution == "daily") {

      lookuptable <- data.frame(id = 1:7,
                                varnames = c("BurnedArea",
                                             "BurnedAreaUncertainty",
                                             "MeanBurnDateUncertainty",
                                             "source",
                                             "TreeCoverDist",
                                             "LandCoverDist",
                                             "PeatFraction"),
                                factor = c(0.01, 0.01, 1, 1, 1, 1, 1),
                                stringsAsFactors = FALSE)

      seq_of_dates <- seq.Date(from = as.Date(start_date),
                               to = as.Date(end_date),
                               by = "day")
      day_of_year <- lubridate::yday(seq_of_dates)
      day_of_year_3_chr <- stringr::str_pad(day_of_year, 3, pad = "0")
      just_year <- substr(x = gsub("-", "", as.character(seq_of_dates)),
                          start = 1, stop = 4)
      # Assemble file names
      fnms <- paste0(base_url, "/", temporal_resolution, "/", just_year, "/",
                     "GFED4.0_DQ_", just_year, day_of_year_3_chr, "_BA.hdf")

    }

    # Initialise empty stack
    s <- raster::stack()
    # Loop through dates to populate the stack
    for (i in seq_along(fnms)) {

      print(fnms[i])
      # Download the file
      input_file_path <- file.path(my_temp_dir, basename(fnms[i]))
      x <- try(RCurl::getBinaryURL(fnms[i],
                                   userpwd = "fire:burnt",
                                   ftp.use.epsv = FALSE,
                                   connecttimeout = 1000),
               silent = FALSE)

      if (class(x) == "try-error") {

        stop("Server currently unavailable, please try again later.")

      } else {

        # Download the file
        input_file_path <- tempfile(fileext = ".hdf")
        print(input_file_path)
        writeBin(x, con = input_file_path)
        # Get subdataset names
        sds <- gdalUtils::get_subdatasets(input_file_path)
        print(sds)
        # Get subdataset index corresponding to my variable
        idx <- lookuptable$id[lookuptable$varname == varname]
        factorx <- lookuptable$factor[lookuptable$varname == varname]
        # Translate subdataset from hdf file to tiff
        # this is needed because no direct translation to nc is available
        temp_tif_file <- tempfile(fileext = ".tif")
        print(temp_tif_file)
        gdalUtils::gdal_translate(src_dataset = sds[idx],
                                  dst_dataset = temp_tif_file)
        s <- raster::stack(s, temp_tif_file)
        print(s)

      }

    }

    raster::crs(s) <- "+proj=longlat +datum=WGS84 +no_defs"

    # Apply scaling factor
    if (factorx != 1) s <- s * factorx

    # Transform the raster
    regions_raster_t <- .transform_raster(s, varname)

    if (raster::nlayers(regions_raster_t) == 1){
      regions_raster_t <- regions_raster_t[[1]]
    }

    return(regions_raster_t)

  }

}
