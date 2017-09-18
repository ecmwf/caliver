.onAttach <- function(...) {

  packageStartupMessage("
+------------------------------------------------------------------------------+
| Copyright 2016 European Centre for Medium-Range Weather Forecasts (ECMWF).   |
| This software is licensed under the terms of the Apache Licence Version 2.0  |
| which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.         |
| In applying this licence, ECMWF does not waive the privileges and immunities |
| granted to it by virtue of its status as an intergovernmental organisation   |
| nor does it submit to any jurisdiction.                                      |
+------------------------------------------------------------------------------+
")

}

.onLoad <- function(libname, pkgname){

  # Check whether CDO is installed
  if (Sys.which("cdo")[[1]] == "") {
    stop("cdo executable not found. Check PATH or install cdo.")
  }

  # Check whether NCL is installed
  if (Sys.which("ncl")[[1]] == "") {
    stop("ncl executable not found. Check PATH or install ncl.")
  }

}
