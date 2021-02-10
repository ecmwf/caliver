# caliver

> An R package for the **cali**bration and **ver**ification of gridded models

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.596343.svg)](https://doi.org/10.5281/zenodo.596343)
[![R-CMD-check](https://github.com/ecmwf/caliver/workflows/R-CMD-check/badge.svg)](https://github.com/ecmwf/caliver/actions)
[![Codecov test coverage](https://codecov.io/gh/ecmwf/caliver/branch/master/graph/badge.svg)](https://codecov.io/gh/ecmwf/caliver?branch=master)
<!-- badges: end -->

**caliver** is a package developed for the R programming language. The name stands for **cal**Ibration and **ver**ification of gridded models. Although caliver was initially designed for wildfire danger models such as GEFF (developed by ECMWF) and RISICO (developed by CIMA Research Foundation), the algorithms can be applied to any gridded model output. Caliver is available with an APACHE-2 license.

For more details, please see the following papers:

- Vitolo C, Di Giuseppe F, D’Andrea M (2018) **Caliver: An R package for CALIbration and VERification of forest fire gridded model outputs**. PLOS ONE 13(1): e0189419. https://doi.org/10.1371/journal.pone.0189419
*Please note: in the latest version of the caliver package many functionalities described in this paper have become obsolete and deprecated, please refer to the vignette "An introduction to the caliver package" for more details.*

- Vitolo C., Di Giuseppe F., Barnard C., Coughlan R., Krzeminski B., San-Miguel-Ayanz J. **ERA5-based global meteorological wildfire danger maps**. Sci Data 7, 216 (2020). https://doi.org/10.1038/s41597-020-0554-z

- Vitolo C., Di Giuseppe F., Krzeminski B., San-Miguel-Ayanz J. **A 1980–2018 global fire danger re-analysis dataset for the Canadian Fire Weather Indices**, Sci Data 6, 190032 (2019). https://doi.org/10.1038/sdata.2019.32

## Installation
The installation of the caliver package depends on the following libraries:

* Geospatial Data Abstraction Library ([GDAL](https://gdal.org/))
* NetCDF4 ([netcdf4](https://www.unidata.ucar.edu/software/netcdf/))

Make sure you have the above libraries installed before attempting to install caliver.
Once all the dependencies are installed, get caliver's development version from github using [devtools](https://github.com/r-lib/devtools):

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
- [Contributions are welcome](https://github.com/ecmwf/caliver/blob/master/CONTRIBUTING.md)! Please note that this project is released with a [Contributor Code of Conduct](https://github.com/ecmwf/caliver/blob/master/CONDUCT.md). By participating in this project you agree to abide by its terms.
- Please [report any issues or bugs](https://github.com/ecmwf/caliver/issues).
- License: Apache License 2.0
- Get citation information for `caliver` in R doing `citation(package = "caliver")`
