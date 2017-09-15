## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      eval = FALSE)

## ---- echo = FALSE, eval = FALSE-----------------------------------------
#  # Is CDO installed?
#  Sys.which("cdo")[[1]]
#  
#  # Is GDAL installed?
#  Sys.which("gdal")[[1]]
#  
#  # Is NetCDF installed?
#  Sys.which("netcdf4")[[1]]
#  
#  # Is NCL installed?
#  Sys.which("ncl")[[1]]
#  
#  setwd('/scratch/mo/moc0/fire/')

## ---- eval=FALSE---------------------------------------------------------
#  packs <- c("rgdal", "ncdf4", "ggplot2", "raster", "sp", "grDevices",
#             "RCurl", "rworldmap", "graphics", "httr", "stringr",
#             "lubridate", "rhdf5", "RColorBrewer", "dplyr", "ggmap",
#             "purrr", "viridis")
#  new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
#  if(length(new.packages)) install.packages(new.packages)
#  
#  # Install Bioconductor packages
#  devtools::install_github("Bioconductor-mirror/rhdf5")

