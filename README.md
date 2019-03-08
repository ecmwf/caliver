# caliver R package

> CALIbration and VERification of gridded (wildfire danger) models

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.376613.svg)](https://doi.org/10.5281/zenodo.376613)
[![Travis-CI Build Status](https://travis-ci.org/ecmwf/caliver.svg?branch=master)](https://travis-ci.org/ecmwf/caliver)
[![Coverage Status](https://codecov.io/gh/ecmwf/caliver/master.svg)](https://codecov.io/github/ecmwf/caliver?branch=master)

**caliver** is a package developed for the R programming language. The name stands for **cal**Ibration and **ver**ification of gridded models. Although caliver was initially designed for wildfire danger models such as GEFF (developed by ECMWF) and RISICO (developed by CIMA Research Foundation), the algorithms can be applied to any gridded model output. Caliver is available with an APACHE-2 license.

For more details, please see the following paper:

*Vitolo C., Di Giuseppe F. and D’Andrea M. (2018) **“Caliver: An R Package for Calibration and Verification of Forest Fire Gridded Model Outputs”**. PLOS ONE 13 (1). Public Library of Science: 1–18. [doi:10.1371/journal.pone.0189419](doi:10.1371/journal.pone.0189419)*

## External dependencies
The installation of the caliver package depends on the Geospatial Data Abstraction Library ([GDAL](http://www.gdal.org/), a translator library for raster and vector geospatial data formats), the NetCDF4 library ([netcdf4](http://www.unidata.ucar.edu/software/netcdf/)) and the HDF5 library([hdf5](https://www.hdfgroup.org/solutions/hdf5/)).

Assuming the user has already installed R, below are the instructions to install caliver's external dependencies on various operating systems.

On **Ubuntu-Linux**, run the following in a terminal:

* `sudo apt-get install libssl-dev`
* `sudo apt-get install libproj-dev`
* `sudo apt-get install libgdal-dev`
* `sudo apt-get install gdal-bin`
* `sudo apt-get install libhdf5-dev`
* `sudo apt-get install libnetcdf-dev`
* `sudo apt-get install netcdf-bin`

On **MAC**, use homebrew and run the following in a terminal:

* `brew install netcdf`
* `brew install hdf5`
* `brew install proj`
* `brew install gdal`

On **Windows**, follow the steps below:

  * install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
  * launch an R console and run the following commands:
    - ToDo

## Install and load caliver

Get caliver's development version from github using [devtools](https://github.com/hadley/devtools):

``` r
install.packages("devtools")
devtools::install_github("ecmwf/caliver")
```

Load the caliver package:

``` r
library("caliver")
```

## Docker
If you use docker, in this repository you find a Dockerfile that contains all the necessary dependencies and the caliver package already installed.

```
docker build -t ecmwf/caliver:latest -f Dockerfile .
docker run -it --rm ecmwf/caliver:latest bash
```

Meta
----

- This package and functions herein are part of an experimental open-source project. They are provided as is, without any guarantee.
- Contributions are welcome! Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
- Please [report any issues or bugs](https://github.com/ecmwf/caliver/issues).
- License: Apache License 2.0
- Get citation information for `caliver` in R doing `citation(package = "caliver")`
