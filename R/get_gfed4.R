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
                    quiet = TRUE, cacheOK = TRUE, mode= "wb")

      # Extract dataset with basis regions
      file_h5 <- hdf5r::h5file(file.path(my_temp_dir, fname))
      # lets look at the content: file.h5$ls(recursive=TRUE)
      # Extract dataset
      br <- file_h5[["ancill/basis_regions"]]

      # Convert hdf5 to raster
      regions_raster <- raster::raster(br[, ])

      # Transform the raster
      # transpose
      regions_raster_t <- raster::t(regions_raster)
      # set extent
      raster::extent(regions_raster_t) <- raster::extent(-180, 180, -90, 90)
      # define zeros as NAs
      regions_raster_t[regions_raster_t == 0] <- NA
      # assign CRS (WGS84)
      raster::crs(regions_raster_t) <- "+proj=longlat +datum=WGS84 +no_defs"

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

  }else{

    if (is.null(start_date)) stop("Please enter valid start_date")
    if (is.null(end_date)) stop("Please enter valid end_date")

    if (varname %in% c("BurnedArea", "BurnedAreaUncertainty",
                       "MeanBurnDateUncertainty", "source", "TreeCoverDist",
                       "LandCoverDist", "PeatFraction")){

      if (is.null(start_date) | is.null(end_date)) {
        stop("Please enter valid dates")
      }

      if (is.null(temporal_resolution)) {
        stop("Please enter valid temporal_resolution")
      }

      base_url <- "ftp://fuoco.geog.umd.edu/gfed4"

      if (temporal_resolution == "monthly"){

        tmp_date <- seq.Date(from = as.Date(start_date), to = as.Date(end_date),
                             by = "month")
        my_date <- substr(x = gsub("-", "", as.character(tmp_date)),
                          start = 1, stop = 6)

        dir_url <- paste0(base_url, "/", "monthly", "/")

        pattern0 <- "GFED4.0_MQ_"

      }

      if (temporal_resolution == "daily") {

        tmp_date <- seq.Date(from = as.Date(start_date), to = as.Date(end_date),
                             by = "day")
        day_of_year <- lubridate::yday(tmp_date)
        day_of_year_3_chr <- stringr::str_pad(day_of_year, 3, pad = "0")
        just_year <- substr(x = gsub("-", "", as.character(tmp_date)),
                            start = 1, stop = 4)
        my_date <- paste0(just_year, day_of_year_3_chr)
        dir_url <- paste0(base_url, "/", "daily", "/")
        pattern0 <- "GFED4.0_DQ_"

      }

      for (d in my_date) {

        just_year <- substr(d, start = 1, stop = 4)

        if (temporal_resolution == "monthly") {

          input_file <- paste0(pattern0, d, "_BA.hdf")
          my_url <- paste0(dir_url, input_file)

        }

        if (temporal_resolution == "daily") {

          input_file <- paste0(pattern0, d, "_BA.hdf")
          my_url <- paste0(dir_url, just_year, "/", input_file)

        }

        x <- try(RCurl::getBinaryURL(my_url,
                                     userpwd = "fire:burnt",
                                     ftp.use.epsv = FALSE),
                 silent = FALSE)

        if (class(x) == "try-error") {

          message("Either the data or the server are unavailable.")
          stop("Please check whether your dates are valid or try again later.")

        }else{

          writeBin(x, con = file.path(my_temp_dir, input_file))

          # Check whether we need to save all the variables or only some of them
          if (is.null(varname)){

            var_option <- ""

          }else{

            var_option <- paste0(" -v ", varname)

          }

          # This approach uses ncl (dependency) and can give problems in RStudio
          # but works fine in the console
          string2call <- paste0("ncl_convert2nc ", file.path(my_temp_dir,
                                                             input_file),
                                var_option, " -o ", my_temp_dir)
          system(string2call)

        }

      }

      list_of_files <- list.files(my_temp_dir,
                                  pattern = paste0("^", pattern0),
                                  full.names = TRUE)

      # my_temp_dir only contains Burned Area files!
      if (length(list.files(my_temp_dir)) == 0) {

        stop("No files have been downloaded, please check your connection.")

      }

      merged <- raster::stack(list_of_files)

      # The resulting raster object is in a quater degree resolution but
      # the extent and the coordinate system should be set manually

      # Transform the rasterBrick, flipping it on the y direction
      regions_raster_t <- raster::flip(merged, direction = "y")

      # Set extent
      raster::extent(regions_raster_t) <- raster::extent(-180, 180, -90, 90)
      # Assign CRS (WGS84)
      raster::crs(regions_raster_t) <- "+proj=longlat +datum=WGS84 +no_defs"
      
      if (raster::nlayers(regions_raster_t) == 1){
        regions_raster_t <- regions_raster_t[[1]]
      }

      return(regions_raster_t)

    }else{
      stop("The varname provided is not valid, please enter valid varname")
    }

  }

}
