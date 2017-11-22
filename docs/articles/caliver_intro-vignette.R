## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)

## ------------------------------------------------------------------------
#  # Install caliver from GitHub via devtools
#  install.packages("devtools")
#  devtools::install_github("ecmwf/caliver")

## ---- eval = TRUE--------------------------------------------------------
library("caliver")

## ------------------------------------------------------------------------
#  # Get daily burned area maps from 2003 to 2015 (to be run in the console!)
#  BurnedAreas <- get_gfed4(start_date = "2003-01-01",
#                           end_date = "2015-12-31",
#                           temporal_resolution = "daily",
#                           varname = "BurnedArea",
#                           region = "GLOB")
#  
#  # The above can be saved as follows:
#  raster::writeRaster(BurnedAreas,
#                      filename="BurnedArea.grd",
#                      bandorder='BIL', overwrite=TRUE, progress = 'text')

## ---- echo = FALSE-------------------------------------------------------
#  # The above can be re-loaded as follows:
#  BurnedAreas <- raster::brick("BurnedArea.grd")

## ------------------------------------------------------------------------
#  # Get all the BasisRegions
#  BasisRegions <- get_gfed4(varname = "BasisRegions")

## ---- eval = TRUE--------------------------------------------------------
# Europe
Europe <- get_gfed4(varname = "BasisRegions", region = "EURO")

## ------------------------------------------------------------------------
#  # United Kingdom
#  UnitedK <- raster::getData(name = "GADM", country = "United Kingdom",
#                             level = 0)
#  
#  # Spain
#  Spain <- raster::getData(name = "GADM", country = "Spain", level = 0)

## ---- eval = TRUE--------------------------------------------------------
# Italy
Italy <- raster::getData(name = "GADM", country = "Italy", level = 0)

## ------------------------------------------------------------------------
#  # Italian regions
#  Italy1 <- raster::getData(name = "GADM", country = "Italy", level = 1)
#  
#  # Get polygons for Liguria, Calabria and Sicily
#  Liguria <- Italy1[9,]
#  Calabria <- Italy1[4,]
#  Sicily <- Italy1[15,]
#  
#  # Get polygon for the Province of Genoa
#  Italy2 <- raster::getData(name = "GADM", country = "Italy", level = 2)
#  Genoa <- Italy2[42,]

## ---- eval = TRUE--------------------------------------------------------
geff5tar <- system.file(file.path("testdata", "geff5.tar"), 
                        package = "caliver")
b <- import_geff_data_from_tar(archive = geff5tar)

## ------------------------------------------------------------------------
#  decompress_gz(input_dir = "./tmp")

## ------------------------------------------------------------------------
#  processingTime <- system.time({
#    stack_netcdf_files(input_dir = "./tmp", output_file = "FWI.nc")
#  })

## ------------------------------------------------------------------------
#  map <- get_percentile_raster(input_file = "FWI.nc", probs = 50)

## ---- eval = TRUE--------------------------------------------------------
maps <- get_percentile_raster(r = b, probs = c(50, 75, 90))

## ---- eval = TRUE--------------------------------------------------------
mapItaly <- mask_crop_subset(r = maps, p = Italy, idx = c(1, 3))

## ---- eval = TRUE, fig.width = 7-----------------------------------------
# Use the raster plot method
raster::plot(mapItaly, main = c("FWI 50th perc.", "FWI 90th perc."))

# Use the caliver plotPercentiles function
plot_percentile_raster(maps = mapItaly, main = c("FWI 50th perc.", "FWI 90th perc."))

## ---- eval = TRUE--------------------------------------------------------
dataDates <- seq.Date(from = as.Date("1980-01-01"),
                      to = as.Date("2016-12-31"),
                      by = "day")
                      
# Define a function to extract fire seasons in Europe
seasons <- get_fire_season(dates = dataDates, zone = "north")

# Create an index of fire season dates
fireSeasonIndex <- which(seasons == TRUE)

## ------------------------------------------------------------------------
#  # Load FWI dataset obtained previously
#  FWI <- raster::brick("FWI.nc")
#  
#  # Mask/Crop/Subset FWI over Europe
#  FWIEURO <- mask_crop_subset(r = FWI, p = Europe, idx = fireSeasonIndex)
#  
#  # Calculate levels
#  EuropeThr <- get_fire_danger_levels(fire_index = FWIEURO)
#  
#  # Country level: use a loop to calculate levels for all the countries in the EU:
#  # This does not take into account Greenland, Cyprus, Andorra and Luxembrourg.
#  EUcountries <- c("Austria", "Belgium", "Bulgaria", "Croatia",
#                   "Czech Republic", "Denmark", "Estonia", "Finland", "France",
#                   "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia",
#                   "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
#                   "Portugal", "Romania", "Slovakia", "Slovenia", "Spain",
#                   "Sweden", "United Kingdom")
#  
#  for (singleCountry in EUcountries){
#  
#    print(singleCountry)
#  
#    # Mask/Crop/Subset FWI and generate thresholds for singleCountry
#    singleCountryFWI <- mask_crop_subset(r = FWI,
#                                         p = raster::getData(name = "GADM",
#                                                             country = singleCountry,
#                                                             level = 0),
#                                         idx = fireSeasonIndex)
#    singleCountryThr <- get_fire_danger_levels(fire_index = singleCountryFWI)
#  
#    # Append values to data.frame
#    if (singleCountry == "Austria") {
#      df <- data.frame(matrix(singleCountryThr, nrow = 1))
#    }else{
#      df <- rbind(df, singleCountryThr)
#    }
#  
#    print(df)
#  
#  }
#  
#  EuroThr <- data.frame(cbind(EUcountries, df, stringsAsFactors=FALSE))
#  names(EuroThr) <- c("Country", "Low", "Moderate", "High", "VeryHigh", "Extreme")
#  
#  # Regional level
#  # Mask/Crop/Subset FWI and generate thresholds for Liguria
#  LIG <- mask_crop_subset(r = FWI, p = Liguria, idx = fireSeasonIndex)
#  EuroThr <- rbind(EuroThr, c("Liguria", get_fire_danger_levels(fire_index = LIG)))
#  
#  # Mask/Crop/Subset FWI and generate thresholds for Calabria
#  CAL <- mask_crop_subset(r = FWI, p = Calabria, idx = fireSeasonIndex)
#  EuroThr <- rbind(EuroThr, c("Calabria", get_fire_danger_levels(fire_index = CAL)))
#  
#  # Mask/Crop/Subset FWI and generate thresholds for Sicily
#  SIC <- mask_crop_subset(r = FWI, p = Sicily, idx = fireSeasonIndex)
#  EuroThr <- rbind(EuroThr, c("Sicily", get_fire_danger_levels(fire_index = SIC)))
#  
#  # Province level
#  # Mask/Crop/Subset FWI and generate thresholds for Genoa
#  GEN <- mask_crop_subset(r = FWI, p = Genoa, idx = fireSeasonIndex)
#  EuroThr <- rbind(EuroThr, c("Genoa", get_fire_danger_levels(fire_index = GEN)))
#  
#  # Remove NAs, e.g. Luxembourg and Malta are too small compared to the ratser resolution
#  EuroThr <- EuroThr[complete.cases(EuroThr),]
#  
#  EuroThr <- rbind(EuroThr, c("Europe", EuropeThr))
#  
#  # Save table with thresholds for future use
#  saveRDS(EuroThr, "EuroThr.rds")

## ------------------------------------------------------------------------
#  countryPDF <- plot_fire_pdf(fire_index = IT,
#                              thresholds = EuroThr["Italy", ],
#                              upper_limit = 75,
#                              v_lines = c(0.50, 0.75, 0.90))

## ------------------------------------------------------------------------
#  library("pROC")
#  
#  BurnedAreas <- raster::brick("GFED4_BurnedAreas/BurnedArea.grd")
#  
#  # Mask and crop burned areas over Europe
#  BA <- mask_crop_subset(r = BurnedAreas, p = Europe)
#  
#  # If observations layers have no date, assign it!
#  dataDates <- seq.Date(from = as.Date("2003-01-01"),
#                        to = as.Date("2015-12-31"), by = "day")
#  names(BA) <- dataDates
#  
#  EuroThrHigh <- as.numeric(EuroThr[EuroThr$Country == "Europe", 4])
#  
#  # The above can be saved and re-loaded as follows:
#  raster::writeRaster(BA, filename="BurnedAreaEurope.grd",
#                      bandorder='BIL', overwrite=TRUE, progress = 'text')
#  BurnedAreaEurope <- raster::brick("BurnedAreaEurope.grd")
#  
#  # For the validation we do not want to subset over the fire season, subset to match days in BurnedAreaEurope
#  FWIEURO <- mask_crop_subset(r = FWI, p = Europe, idx = which(names(FWI) %in% names(BurnedAreaEurope)))
#  # The above can be saved and re-loaded as follows:
#  raster::writeRaster(FWIEURO, filename="FWIEURO.grd", bandorder='BIL', overwrite=TRUE, progress = 'text')
#  FWIEURO <- raster::brick("FWIEURO.grd")
#  
#  # Contingency table for JRC - Europe as a whole
#  x1 <- validate_fire_danger_levels(fire_index = FWIEURO, observation = BurnedAreaEurope,
#                                   fire_threshold = 21.3, obs_threshold = 50)
#  tab_x <- table(pred = x1$pred, obs = x1$obs)
#  hits <- tab_x[2,2]
#  misses <- tab_x[1,2]
#  correct_negatives <- tab_x[1,1]
#  false_alarms <- tab_x[2,1]
#  # POD 47%
#  round(hits/(hits+misses),2)*100
#  roc1 <- pROC::roc(response = x1$obs, predictor = x1$pred)
#  pROC::plot.roc(roc1, print.auc = pROC::auc(roc1), print.auc.x = 0, print.auc.y = 0.9)
#  
#  # Contingency table for caliver - Europe as a whole
#  x2 <- validate_fire_danger_levels(fire_index = FWIEURO, observation = BurnedAreaEurope,
#                                   fire_threshold = EuroThrHigh, obs_threshold = 50)
#  tab_x <- table(pred = x2$pred, obs = x2$obs)
#  hits <- tab_x[2,2]
#  misses <- tab_x[1,2]
#  # POD 65%
#  round(hits/(hits+misses),2)*100
#  roc2 <- pROC::roc(response = x2$obs, predictor = x2$pred)
#  pROC::plot.roc(roc2, col = "red", add = TRUE,
#                 print.auc = pROC::auc(roc2), print.auc.x = 0, print.auc.y = 0.95,
#                 print.auc.col = "red")
#  
#  
#  # Loop throught the countries
#  for (singleCountry in EuroThr[1:26,"Country"]){
#  
#    print(singleCountry)
#  
#    if (!(singleCountry %in% c("Cyprus"))){
#  
#      countryPoly <- raster::getData(name = "GADM", country = singleCountry, level = 0)
#      countryThr <- as.numeric(EuroThr[EuroThr$Country == singleCountry, 4])
#  
#      # Crop RasterBricks over country of interest
#      BA_country <- mask_crop_subset(r = BurnedAreaEurope, p = countryPoly)
#      FWI_country <- mask_crop_subset(r = FWIEURO, p = countryPoly)
#  
#      JRC <- validate_fire_danger_levels(fire_index = FWI_country,
#                                         observation = BA_country,
#                                         fire_threshold = 21.3,
#                                         obs_threshold = 50)
#      tab_JRC <- data.frame(table(JRC$pred, JRC$obs))
#      caliver1 <- validate_fire_danger_levels(fire_index = FWI_country,
#                                              observation = BA_country,
#                                              fire_threshold = EuroThrHigh,
#                                              obs_threshold = 50)
#      tab_caliver1 <- data.frame(table(caliver1$pred, caliver1$obs))
#      caliver2 <- validate_fire_danger_levels(fire_index = FWI_country,
#                                              observation = BA_country,
#                                              fire_threshold = countryThr,
#                                              obs_threshold = 50)
#      tab_caliver2 <- data.frame(table(caliver2$pred, caliver2$obs))
#  
#      if (singleCountry == "Austria") {
#        df_caliver1 <- df_caliver2 <- df_effis <- data.frame("pred" = tab_caliver1$pred, "obs" = tab_caliver1$obs)
#        i <- 3
#      }
#  
#      df_caliver1 <- cbind(df_caliver1, tab_caliver1$Freq)
#      names(df_caliver1)[i] <- singleCountry
#      df_caliver2 <- cbind(df_caliver2, tab_caliver2$Freq)
#      names(df_caliver2)[i] <- singleCountry
#      df_effis <- cbind(df_effis, tab_JRC$Freq)
#      names(df_effis)[i] <- singleCountry
#      i <- i + 1
#  
#      rm(countryPoly, countryThr, BA_country, FWI_country)
#  
#    }
#  
#  }
#  
#  # Save contingency tables
#  saveRDS(df_caliver1, "df_caliver1.rds")
#  saveRDS(df_caliver2, "df_caliver2.rds")
#  saveRDS(df_effis, "df_effis.rds")
#  
#  # Europe (EFFIS danger levels)
#  sum(df_effis[4,3:27]) # hits
#  sum(df_effis[3,3:27]) # misses
#  # Europe (averaged danger levels)
#  sum(df_caliver1[4,3:27]) # hits
#  sum(df_caliver1[3,3:27]) # misses
#  # Europe (country-specific danger levels)
#  sum(df_caliver2[4,3:27]) # hits
#  sum(df_caliver2[3,3:27]) # misses
#  
#  # UK (EFFIS danger levels)
#  df_effis[4, which(names(df_caliver2) == "United Kingdom")] # hits
#  df_effis[3, which(names(df_caliver2) == "United Kingdom")] # misses
#  # UK (EU averaged danger levels)
#  df_caliver1[4, which(names(df_caliver2) == "United Kingdom")] # hits
#  df_caliver1[3, which(names(df_caliver2) == "United Kingdom")] # misses
#  # UK (country-specific danger levels)
#  df_caliver2[4, which(names(df_caliver2) == "United Kingdom")] # hits
#  df_caliver2[3, which(names(df_caliver2) == "United Kingdom")] # misses
#  
#  # Spain (EFFIS danger levels)
#  df_effis[4, which(names(df_caliver2) == "Spain")] # hits
#  df_effis[3, which(names(df_caliver2) == "Spain")] # misses
#  # Spain (EU averaged danger levels)
#  df_caliver1[4, which(names(df_caliver2) == "Spain")] # hits
#  df_caliver1[3, which(names(df_caliver2) == "Spain")] # misses
#  # Spain (country-specific danger levels)
#  df_caliver2[4, which(names(df_caliver2) == "Spain")] # hits
#  df_caliver2[3, which(names(df_caliver2) == "Spain")] # misses
#  
#  # Italy (EFFIS danger levels)
#  df_effis[4, which(names(df_caliver2) == "Italy")] # hits
#  df_effis[3, which(names(df_caliver2) == "Italy")] # misses
#  # Italy (EU averaged danger levels)
#  df_caliver1[4, which(names(df_caliver2) == "Italy")] # hits
#  df_caliver1[3, which(names(df_caliver2) == "Italy")] # misses
#  # Italy (country-specific danger levels)
#  df_caliver2[4, which(names(df_caliver2) == "Italy")] # hits
#  df_caliver2[3, which(names(df_caliver2) == "Italy")] # misses

