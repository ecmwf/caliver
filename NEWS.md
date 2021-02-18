# caliver 2.0

* Version submitted to CRAN.
* Expanded and improved documentation
* Corrected typos
* Changes in function names:
  - plot_fire_pdf becomes plot_pdf
  - plot_percentile_raster becomes plot_percentiles
  - get_percentile_raster becomes get_percentile_maps and incorporates two approaches: percentile of full record and percentile over daily climatology
* Deprecated function:
  - stack_with_rat

# caliver 1.7

* Removed references and dependencies from defunct functions.
* Removed unused test datasets.
* The following functions have changed:
  * mask_crop_subset, mask and crop input argument have been removed as deemed superfluous. Also the option `accurate` has been removed.
* The following functions are now defunct:
  * decompress_gz() - Decompress all .gz files in a given folder
  * import_geff_data_from_tar() - Import GEFF data from a tar file into a raster stack
  * mean_percs() - Calculate average percentile
  * stack_netcdf_files() - Merges all the netcdf files in a given directory over the time dimension
  * get_gfed4 - Get data from the fourth-generation Global Fire Emissions (GFED4 ftp service is no longer available)
  * mask_with_fuelmodel - there is now an example in the vignette that explains how to do this more generally


# caliver 1.6

* Added non-exported experimental functions
* Deprecated function, then moved to defunct

# caliver 1.5

* In the new GEFF release, the archival system and naming convention have changed. As a result the following functions have been deprecated:

  - import_geff_data_from_tar()
  - decompress_gz()

* The function plot_obs_vs_forecast() is renamed to forecast_summary()
* Argument rotate_map is deprecated as the function automatically detect extent
* Removed dependency from visualTest as it is not in mainstream repositories
* Removed LICENSE file, linking directly to Apache License 2.0 in DESCRIPTION
* Removed dependency from bioconductor package rhdf5, now using hdf5r from cran
* Removed external dependency from ncl and cdo
* Removed obsolete/deprecated dependencies

# caliver 1.4

* New addition: anomaly and ranking functions.

# caliver 1.2

* Changes due to review on plosone.

# caliver 1.0

* This is the first stable release.

# caliver 0.1

* This is the first release of the caliver R package and contains function for the retrieval, analysis and visualisation of GEFF and RISICO forest fire model outputs.
