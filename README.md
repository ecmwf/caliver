
<!-- Edit the README.Rmd only!!! The README.md is generated automatically from README.Rmd. -->
caliver: CALIbration and VERification of gridded model outputs
==============================================================

<!--

[![Travis-CI Build Status](https://travis-ci.org/anywhereProject/caliver.svg?branch=master)](https://travis-ci.org/anywhereProject/caliver)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/anywhereProject/caliver?branch=master&svg=true)](https://ci.appveyor.com/project/anywhereProject/caliver)
[![Coverage Status](https://img.shields.io/codecov/c/github/anywhereProject/caliver/master.svg)](https://codecov.io/github/anywhereProject/caliver?branch=master)

-->
The name '[caliver](https://github.com/anywhereProject/caliver)' stands for CALIbration and VERification of gridded model outputs. It is an extension package developed for the R programming language and available with an APACHE license from a public repository (<https://github.com/anywhereProject/caliver>).

As the name suggests, the caliver package contains utility functions for the post-processing, calibration and validation/verification of gridded model outputs. Initial test cases include the outputs of the following forest fire models: GEFF (add ref.) and RISICO (add ref.). However, the algorithms can be applied to any gridded model output.

Complete documentation, including a vignette, is available within the package.

Dependencies and Installation
-----------------------------

The installation of the caliver package depends on the Climate Data Operators ([cdo](https://code.zmaw.de/projects/cdo/wiki)), a large tool set for working on climate and NWP model data), the Geospatial Data Abstraction Library ([GDAL](http://www.gdal.org/), a translator library for raster and vector geospatial data formats), the NetCDF4 library ([netcdf4](http://www.unidata.ucar.edu/software/netcdf/)) and the following R packages:

``` r
packs <- c("devtools", "graphics", "grDevices", "knitr", "latticeExtra", 
           "raster", "rasterVis", "rgdal", "sp", 
           "stats", "testthat", "rmarkdown", "rworldmap")
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
```

Get the development version from github using [devtools](https://github.com/hadley/devtools):

``` r
devtools::install_github("anywhereProject/caliver")
```

Load the caliver package:

``` r
library("caliver")
```

Meta
----

-   This package and functions herein are part of an experimental open-source project. They are provided as is, without any guarantee.
-   Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
-   Please [report any issues or bugs](https://github.com/anywhereProject/caliver/issues).
-   License: [GPL-3](https://opensource.org/licenses/GPL-3.0)
-   Get citation information for `caliver` in R doing `citation(package = 'caliver')`
