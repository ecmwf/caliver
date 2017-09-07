# caliver R package

> CALIbration and VERification of gridded (fire risk) models

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.376613.svg)](https://doi.org/10.5281/zenodo.376613)
[![Travis-CI Build Status](https://travis-ci.org/ecmwf/caliver.svg?branch=master)](https://travis-ci.org/ecmwf/caliver)
[![Coverage Status](https://codecov.io/gh/ecmwf/caliver/master.svg)](https://codecov.io/github/ecmwf/caliver?branch=master)

The name '[caliver](https://github.com/ecmwf/caliver)' stands for CALIbration and VERification of gridded model outputs. It is an extension package developed for the R programming language and available with a APACHE-2 license from a public repository.

As the name suggests, the caliver package contains utility functions for the post-processing, calibration and validation/verification of gridded model outputs. Initial test cases include the outputs of the following forest fire models: GEFF and RISICO. However, the algorithms can be applied to any gridded model output.

Complete documentation, including a vignette, is available within the package. Contributions are welcome!

Dependencies and Installation
-----------------------------

The installation of the caliver package depends on the Climate Data Operators ([cdo](https://code.zmaw.de/projects/cdo/wiki)), a large tool set for working on climate and NWP model data); the NCAR Command Language ([ncl](https://www.ncl.ucar.edu/)), an interpred language with built-in functionalities to convert hdf5 files to netcdf format; the Geospatial Data Abstraction Library ([GDAL](http://www.gdal.org/), a translator library for raster and vector geospatial data formats) and the NetCDF4 library ([netcdf4](http://www.unidata.ucar.edu/software/netcdf/)).

To install these dependencies:

* On Windows, install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
* On Mac, install [Xcode](https://developer.apple.com/xcode/) from the app store.
* On Ubuntu-Linux, `sudo apt-get install libssl-dev libproj-dev libgdal-dev libhdf5-dev libnetcdf-dev netcdf-bin ncl-ncarg cdo`.

Caliver also depends on additional R packages from both CRAN and Bioconductor. Here is how to install all the necessary packages:

``` r
# Install packages from CRAN
packs <- c("rgdal", "ncdf4", "ggplot2", "raster", "sp", "rworldmap", "httr", "stringr", "lubridate", "RCurl", "plotrix", "reshape2", "R.utils", "devtools", "proto", "roxygen2", "png", "rmarkdown", "pkgdown")
install.packages(packs)

# Install package from BIOCONDUCTOR 
source("https://bioconductor.org/biocLite.R")
biocLite("rhdf5")

# Install packages from GitHub
devtools::install_github("jimhester/covr")
devtools::install_github("jimhester/lintr")
devtools::install_github("MangoTheCat/visualTest")
```

Get the development version from github using [devtools](https://github.com/hadley/devtools):

``` r
install.packages("devtools")
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
-   License: [APACHE-2](LICENSE)
-   Get citation information for `caliver` in R doing `citation(package = "caliver")`
