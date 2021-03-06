% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mask_with_fuelmodel.R
\name{mask_with_fuelmodel}
\alias{mask_with_fuelmodel}
\title{mask_with_fuelmodel}
\usage{
mask_with_fuelmodel(x, fuelmap = NULL, codes = NULL, value = NA)
}
\arguments{
\item{x}{Raster* to mask}

\item{fuelmap}{Custom fuel map as RasterLayer.}

\item{codes}{The value of the cells in fuelmap used to mask x.}

\item{value}{The value of cells masked out by the fuel map (NA by default).}
}
\description{
This function masks a Raster* object using JRC's fuelmodel.
}
\details{
In absence of vegetation the risk of ignition reduces considerably,
regardless of the state of the soil. These areas are mapped in the JRC's
fuelmodel (in use in 2018). This package contains a cached version of this
map (matching crs and resolution of ERA-Interim),
stored in the 'inst/extdata' folder of the package. For evaluating fire
danger we use this map to mask deserts, glaciers, urban areas, etc. These
areas have codes > 20. The optional argument `fuelmap` allows to input a
custom fuelmap and costum `codes` to use for masking.
Please note that
}
\examples{
\dontrun{
  xmasked <- mask_with_fuelmodel(x)
}

}
