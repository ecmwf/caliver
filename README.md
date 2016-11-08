---
output: 
  github_document: 
    toc: TRUE
---

<!-- Edit the README.Rmd only!!! The README.md is generated automatically from README.Rmd. -->

rdefra: Interact with the UK AIR Pollution Database from DEFRA
==============================================================

```{r echo=FALSE}
knitr::opts_chunk$set(
  comment = '#>',
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  eval = FALSE,
  cache = TRUE
)
```

[![DOI](https://zenodo.org/badge/9118/kehraProject/rdefra.svg)](https://zenodo.org/badge/latestdoi/9118/kehraProject/rdefra)
[![status](http://joss.theoj.org/papers/57058f6e8a511f3bb0667ef7687cc87d/status.svg)](http://joss.theoj.org/papers/57058f6e8a511f3bb0667ef7687cc87d)

[![Build Status](https://travis-ci.org/ropenscilabs/rdefra.svg)](https://travis-ci.org/ropenscilabs/rdefra.svg?branch=master)
[![codecov.io](https://codecov.io/gh/ropenscilabs/rdefra/coverage.svg?branch=master)](https://codecov.io/gh/ropenscilabs/rdefra?branch=master)

[![CRAN Status Badge](http://www.r-pkg.org/badges/version/rdefra)](https://cran.r-project.org/package=rdefra)
[![CRAN Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/rdefra)](https://cran.r-project.org/package=rdefra)
[![CRAN Monthly Downloads](http://cranlogs.r-pkg.org/badges/rdefra)](https://cran.r-project.org/package=rdefra)

The package [rdefra](https://cran.r-project.org/package=rdefra) allows to retrieve air pollution data from the Air Information Resource [UK-AIR](https://uk-air.defra.gov.uk/) of the Department for Environment, Food and Rural Affairs in the United Kingdom. UK-AIR does not provide a public API for programmatic access to data, therefore this package scrapes the HTML pages to get relevant information.

This package follows a logic similar to other packages such as [waterData](https://cran.r-project.org/package=waterdata) and [rnrfa](https://cran.r-project.org/package=rnrfa): sites are first identified through a catalogue, data are imported via the station identification number, then data are visualised and/or used in analyses. The metadata related to the monitoring stations are accessible through the function `ukair_catalogue()`, missing stations' coordinates can be obtained using the function `ukair_get_coordinates()`, and time series data related to different pollutants can be obtained using the function `ukair_get_hourly_data()`.

DEFRA's servers can handle multiple data requests, therefore concurrent calls can be sent simultaneously using the [parallel](https://www.R-project.org/) package. Although the limit rate depends on the maximum number of concurrent calls, traffic and available infrustracture, data retrieval is very efficient. Multiple years of data for hundreds of sites can be downloaded in only few minutes.

For similar functionalities see also the [openair](https://cran.r-project.org/package=openair) package, which relies on a local copy of the data on servers at King's College (UK), and the [ropenaq](https://CRAN.R-project.org/package=ropenaq) which provides UK-AIR latest measured levels (see https://uk-air.defra.gov.uk/latest/currentlevels) as well as data from other countries.

## Dependencies & Installation

### Dependencies

The rdefra package depends on two things: 

* The Geospatial Data Abstraction Library (gdal). If you use linux/ubuntu, this can be installed with the following command: `sudo apt-get install -y r-cran-rgdal`. 

* Some additional CRAN packages. Check for missing dependencies and install them using the commands below:

```{r}
packs <- c('httr', 'xml2', 'lubridate', 'tibble', 'dplyr', 'sp', 'devtools',
           'leaflet', 'zoo', 'testthat', 'knitr', 'Rmarkdown')
new.packages <- packs[!(packs %in% installed.packages()[,'Package'])]
if(length(new.packages)) install.packages(new.packages)
```

### Installation

Get the released version from CRAN:

```{r}
install.packages('rdefra')
```

Or the development version from github using [devtools](https://github.com/hadley/devtools):

```{r}
devtools::install_github('ropenscilabs/rdefra')
```

Load the rdefra package:

```{r, eval = TRUE}
library('rdefra')
```

## Meta

* This package and functions herein are part of an experimental open-source project. They are provided as is, without any guarantee.
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
* Please [report any issues or bugs](https://github.com/anywhereProject/caliver/issues).
* License: [GPL-3](https://opensource.org/licenses/GPL-3.0)
* Get citation information for `rdefra` in R doing `citation(package = 'rdefra')`
