#' @title mask_with_fuelmodel
#'
#' @description This function masks a Raster* object using JRC's fuelmodel.
#'
#' @param x Raster* to mask
#' @param fuelmap Custom fuel map as RasterLayer.
#' @param codes The value of the cells in fuelmap used to mask x.
#' @param value The value of cells masked out by the fuel map (NA by default).
#'
#' @details In absence of vegetation the risk of ignition reduces considerably,
#' regardless of the state of the soil. These areas are mapped in the JRC's
#' fuelmodel (in use in 2018). This package contains a cached version of this
#' map (matching crs and resolution of ERA-Interim),
#' stored in the 'inst/extdata' folder of the package. For evaluating fire
#' danger we use this map to mask deserts, glaciers, urban areas, etc. These
#' areas have codes > 20. The optional argument `fuelmap` allows to input a
#' custom fuelmap and costum `codes` to use for masking.
#' Please note that
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   xmasked <- mask_with_fuelmodel(x)
#' }
#'

mask_with_fuelmodel <- function(x, fuelmap = NULL, codes = NULL, value = NA){

  if (is.null(fuelmap)){
    # Load JRC fuelmodel map (longitudes in range [0, 360])
    fuelmodel_file_path <- system.file(file.path("extdata",
                                                 "clim_fuelmodel.nc"),
                                       package = "caliver")
    fuelmap <- raster::raster(fuelmodel_file_path)
    # Deserts, glaciers, urban areas, etc. in the dummy fuelmap have codes 21:26
    codes <- 21:26
  }

  # map_legend <- raster::levels(fuelmap)
  # if (!is.null(map_legend) & ("data.frame" %in% class(map_legend))) {
  #   # Save legend in data.frame
  #   df <- as.data.frame(map_legend)
  #   levels(fuelmap) <- df[1:18,]
  # }

  # Compare rasters
  if (raster::compareRaster(x, fuelmap)) {

    # Remove areas with specific codes
    for (code in codes){
      fuelmap[fuelmap == code] <- NA
    }

    # Mask x using fuelmap
    xmasked <- raster::mask(x, fuelmap, updatevalue = value, progress = "text")

  }else{
    message("Please ensure x and fuelmap have the same extent, crs, etc.")

    # Get extent of the raster* object
    # ext <- raster::extent(x)

    # Transform map to raster and rotate longitudes to the range
    # [-180, +180], if x's extent is in this range
    # if (round(ext[1], 0) == -180 & round(ext[2], 0) == +180) {
    #
    #   fuelmodel <- raster::rotate(fuelmodel)
    #
    # }

    # Fuel and x should have same extent and numer of rows/cols
    # fuel <- raster::resample(fuelmodel, x, method = "ngb")
  }

  return(xmasked)

}
