# caliver 1.5

* In the new GEFF release, the archival system and naming convention have changed. As a result the following functions have been deprecated:

  - import_geff_data_from_tar()
  - decompress_gz()

* The function plot_obs_vs_forecast() is renamed to forecast_summary()
* Argument rotate_map is deprecated as the function automatically detect extent
* Removed dependency from visualTest as it is not in mainstream repositories
* Removed LICENSE file, linking directly to Apache License 2.0 in DESCRIPTION
* Removed dependency from bioconductor package rhdf5, now using hdf5r from cran
* Removed external dependency from ncl
* Removed obsolete/deprecated dependencies

# caliver 1.4

* New addition: anomaly and ranking functions.

# caliver 1.2

* Changes due to review on plosone.

# caliver 1.0

* This is the first stable release.

# caliver 0.1

* This is the first release of the caliver R package and contains function for the retrieval, analysis and visualisation of GEFF and RISICO forest fire model outputs.
