# caliver R package

> CALIbration and VERification of gridded (wildfire danger) models

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.376613.svg)](https://doi.org/10.5281/zenodo.376613)
[![Travis-CI Build Status](https://travis-ci.org/ecmwf/caliver.svg?branch=master)](https://travis-ci.org/ecmwf/caliver)
[![Coverage Status](https://codecov.io/gh/ecmwf/caliver/master.svg)](https://codecov.io/github/ecmwf/caliver?branch=master)

**caliver** is a package developed for the R programming language. The name stands for **cal**Ibration and **ver**ification of gridded models. Although caliver was initially designed for wildfire danger models such as GEFF (developed by ECMWF) and RISICO (developed by CIMA Research Foundation), the algorithms can be applied to any gridded model output. Caliver is available with an APACHE-2 license.

For more details, please see the following papers:

*Vitolo C., Di Giuseppe F. and D’Andrea M. (2018) **“Caliver: An R Package for Calibration and Verification of Forest Fire Gridded Model Outputs”**. PLOS ONE 13 (1). Public Library of Science: 1–18. [doi:10.1371/journal.pone.0189419](doi:10.1371/journal.pone.0189419)*

*Vitolo C., Di Giuseppe F., Krzeminski B., San-Miguel-Ayanz J., 2019. A 1980–2018 global fire danger re-analysis dataset for the Canadian Fire Weather Indices, Scientific Data. https://doi.org/10.1038/sdata.2019.32*

## Installation
The installation of the caliver package depends on the following libraries:

* [HDF4](https://support.hdfgroup.org/ftp/HDF/releases/HDF4.2.14/src/hdf-4.2.14.tar.gz) and [HDF5](https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-1.10.5/src/hdf5-1.10.5.tar.gz)
* Geospatial Data Abstraction Library ([GDAL](http://www.gdal.org/))
* NetCDF4 ([netcdf4](http://www.unidata.ucar.edu/software/netcdf/))

Make sure you have the above libraries installed before attempting to install caliver.
Once all the dependencies are installed, get caliver's development version from github using [devtools](https://github.com/hadley/devtools):

``` r
install.packages("remotes")
remotes::install_github("ecmwf/caliver")
```

Load the caliver package:

``` r
library("caliver")
```

## Docker
In this repository you find a Dockerfile that contains all the necessary dependencies and the caliver package already installed.

```
docker build -t ecmwf/caliver:latest -f Dockerfile .
```

Alternatively, you can use the image we host on docker hub:
```
docker run -it --rm ecmwf/caliver:latest bash
```

Meta
----

- This package and functions herein are part of an experimental open-source project. They are provided as is, without any guarantee.
- Contributions are welcome! Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
- Please [report any issues or bugs](https://github.com/ecmwf/caliver/issues).
- License: Apache License 2.0
- Get citation information for `caliver` in R doing `citation(package = "caliver")`
