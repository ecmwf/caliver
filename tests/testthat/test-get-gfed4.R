context("get_gfed4")

test_that("Check get_gfed4 behaves as expected", {

  # Basis regions
  BasisRegions <- get_gfed4(varname = "BasisRegions")
  #'
  #'   # Only BONA region
  #'   BONA <- get_gfed4(varname = "BasisRegions", region = "BONA")
  #'
  #'   # Monthly burned areas
  #'   BurnedAreas <- get_gfed4(start_date = "2003-01-01",
  #'                            end_date = "2003-01-31",
  #'                            temporal_resolution = "monthly",
  #'                            varname = "BurnedArea")
  #'
  #'   # Daily burned areas
  #'   DailyBurnedAreas <- get_gfed4(start_date = "2003-01-01",
  #'                                 end_date = "2003-01-02",
  #'                                 temporal_resolution = "daily",
  #'                                 varname = "BurnedArea")
})
