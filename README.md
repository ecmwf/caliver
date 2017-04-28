# caliver R package

> CALIbration and VERification of gridded (fire risk) models

[![DOI](https://zenodo.org/badge/73203648.svg)](https://zenodo.org/badge/latestdoi/73203648)
[![Travis-CI Build Status](https://travis-ci.org/ecmwf/caliver.svg?branch=master)](https://travis-ci.org/ecmwf/caliver)
[![Coverage Status](https://img.shields.io/codecov/c/github/ecmwf/caliver/master.svg)](https://codecov.io/github/ecmwf/caliver?branch=master)

The name '[caliver](https://github.com/ecmwf/caliver)' stands for CALIbration and VERification of gridded model outputs. It is an extension package developed for the R programming language and available with a GPL-2 license from a public repository.

As the name suggests, the caliver package contains utility functions for the post-processing, calibration and validation/verification of gridded model outputs. Initial test cases include the outputs of the following forest fire models: GEFF (add ref.) and RISICO (add ref.). However, the algorithms can be applied to any gridded model output.

Complete documentation, including a vignette, is available within the package. Contributions are welcome!

Dependencies and Installation
-----------------------------

The installation of the caliver package depends on the Climate Data Operators ([cdo](https://code.zmaw.de/projects/cdo/wiki)), a large tool set for working on climate and NWP model data), the Geospatial Data Abstraction Library ([GDAL](http://www.gdal.org/), a translator library for raster and vector geospatial data formats), the NetCDF4 library ([netcdf4](http://www.unidata.ucar.edu/software/netcdf/)) and the following R packages:

``` r
packs <- c("rgdal", "ncdf4", "ggplot2", "raster", "sp", "grDevices", "RCurl",
           "rworldmap", "graphics", "httr", "stringr", "lubridate", "rhdf5")
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Install Bioconductor packages
devtools::install_github("Bioconductor-mirror/zlibbioc")
devtools::install_github("Bioconductor-mirror/rhdf5")
```

Get the development version from github using [devtools](https://github.com/hadley/devtools):

``` r
devtools::install_github("ecmwf/caliver")
```

Load the caliver package:

``` r
library("caliver")
```

Meta
----

-   This package and functions herein are part of an experimental open-source project. They are provided as is, without any guarantee.
-   Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
-   Please [report any issues or bugs](https://github.com/ecmwf/caliver/issues).
-   License: [GPL-2](https://opensource.org/licenses/GPL-2.0)
-   Get citation information for `caliver` in R doing `citation(package = "caliver")`
