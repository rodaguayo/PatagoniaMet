# Code for precipitation bias correction ---------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("stats")
library("terra")
library("hydroGOF")
library("randomForest")
library("exactextractr")

period <- seq(as.Date("1980-01-01"), as.Date("2020-12-31"), by = "day")
setwd("/home/rooda/Dropbox/Patagonia/")
terraOptions(memfrac=0.90)
set.seed(123)

# 3. Precipitation factors ------------------------------------------------------------------------
pp_stack_hr <- rast("Data/Precipitation/PP_PMET_1980_2020d_npc.nc") # Gridded data from step 1
pp_stack_hr_m <- tapp(pp_stack_hr, strftime(time(pp_stack_hr), format="%Y"),  fun = sum, na.rm = TRUE)
pp_stack_hr_m <- mean(pp_stack_hr_m)

## 3.1 w factor for Budyko framework --------------------------------------------------------------
basin_data        <- read.csv("Data/Streamflow/Q_PMETobs_v10_metadata.csv")
basin_data$PET_PP <- round(basin_data$PET_BH/basin_data$PP_BH, 2)
basin_data$ET_PP  <- round(basin_data$ET_BH/basin_data$PP_BH,  2)
basin_data$w      <- rep(NA, nrow(basin_data))

w_function <- function(PET_PP, ET_PP, w) 1 + PET_PP - ET_PP - (1+((PET_PP)^w))^(1/w)

for (basin in 1:nrow(basin_data)) { 
  if (!is.na(basin_data$PET_PP[basin])) { # Only basins with data in Chile (79 of 83)
    basin_data$w[basin] <- uniroot(w_function, interval=c(1, 5), tol = 0.001, 
                                   PET_PP=basin_data$PET_PP[basin], 
                                   ET_PP=basin_data$ET_PP[basin])$root
    basin_data$w[basin] <- round(basin_data$w[basin], 2)
  }
}
basin_data$PET_PP <- NULL
basin_data$ET_PP <- NULL
print ("3.1 w factors: Ok")

## 3.2 long-term precipitation  -------------------------------------------------------------------
pp_function <- function(PP, Q, PET, dW, w) {
  1 + (PET/(PP-dW)) - ((PP-Q-dW)/(PP-dW)) - (1+(PET/(PP-dW))^w)^(1/w)
}
basin_data$PP_TRUE <- rep(NA, nrow(basin_data))

for (basin in 1:nrow(basin_data)) {
  if (!is.na(basin_data$Qint_mm_y[basin])){ 
    basin_data$PP_TRUE[basin] <- uniroot(pp_function, interval=c(50, 10000), tol = 0.000001, 
                                         Q   = basin_data$Qint_mm_y[basin], 
                                         PET = basin_data$PET_PMET[basin], 
                                         dW  = basin_data$glacier_dhdt[basin],
                                         w   = median(basin_data$w, na.rm = T))$root
    basin_data$PP_TRUE[basin] <- round(basin_data$PP_TRUE[basin], 0)
  }
}

print ("3.2 long-term precipitation: Ok")

## 3.3 PP factor for each basin centroid ----------------------------------------------------------
basins_shp_int             <- sf::st_read("GIS South/Basins_Patagonia83_int.shp")
basin_data$PP_PMET_int     <- exact_extract(pp_stack_hr_m, basins_shp_int, "mean")
basin_data$BF_PMET         <- round(basin_data$PP_TRUE/basin_data$PP_PMET_int, 3)
basin_data$BF_PMET[basin_data$BF_PMET > 2] <- NA # !!!
basins_shp_int$BF_PMET     <- basin_data$BF_PMET
basins_shp_int             <- subset(basins_shp_int, !is.na(basin_data$BF_PMET))
basins_shp_int             <- centroids(vect(basins_shp_int))

basin_data$PP_PMET_int <- NULL
write.csv(basin_data, "Data/Streamflow/Q_PMETobs_v10_metadata.csv", row.names = FALSE)
print ("3.3 PP factors: Ok")

## 3.4 random forest for PP factors ---------------------------------------------------------------
covariates <- c(rast("GIS South/climate_class005.tif"),              # Climate class (discrete)
                rast("GIS South/dist_coast005l.tif"),                # Distance to coast (log m)
                rast("GIS South/dem_patagonia005.tif"),              # Altitude (masl)
                rast("Data/Temperature/Tavg_ERA5_hr_1980_2020.tif"), # Annual temperature (degC)
                rast("Data/Precipitation/PP_ERA5_hr_1980_2020.tif"), # Annual precipitation (mm)
                rast("GIS South/clouds_005.tif"))                 
names(covariates) <- c("climate_class", "distance_coast", "elevation", "t2m", "pp", "cloud_cover")

rf_importance <- data.frame(matrix(ncol = 3 + nlyr(covariates), nrow = 0))

model_values  <- data.frame(extract(covariates, basins_shp_int), bias_factor = basins_shp_int$BF_PMET)[,-1]
model_values  <- model_values[-nrow(model_values),]
rf_model      <- randomForest(bias_factor ~., data = model_values, ntree = 2000, importance = TRUE)
rf_importance <- rbind(rf_importance, c("pp_factor", "pp", "00", importance(rf_model)[,1]))
bias_factor   <- predict(covariates, rf_model, type='response')
bias_factor   <- focal(bias_factor, w = focalMat(pp_stack_hr_m, 0.04, "Gauss"), pad = TRUE)
bias_factor[bias_factor <= 1] <- 1 # (?)

colnames(rf_importance)  <- c("parameter", "var", "month", names(covariates))
write.csv(rf_importance, "MS1 Results/RF_PP_factor_importance.csv", row.names = FALSE)
writeRaster(bias_factor, "MS1 Results/Bias_Factor_PP.tif")

pp_pmet   <- pp_stack_hr * bias_factor
pp_pmet   <- round(pp_pmet, 0)
print ("3.4 RG regression: Ok")

# 4. Saving and resampling files ------------------------------------------------------------------
pp_pmet_m <- tapp(pp_pmet,   strftime(time(pp_pmet), format="%Y-%m"), fun = sum, na.rm = TRUE)
terra::time(pp_pmet_m) <- seq(from = min(period), to = max(period), by = "month")
pp_pmet_y <- tapp(pp_pmet_m, strftime(time(pp_pmet_m), format="%Y"),  fun = sum, na.rm = TRUE)
pp_pmet_y <- mean(pp_pmet_y)

writeCDF(pp_pmet, "Data/Precipitation/PP_PMET_1980_2020d.nc",  overwrite=TRUE, 
         varname="pp", unit="mm", zname="time", prec = "float",  shuffle = T)

writeCDF(pp_pmet_m, "Data/Precipitation/PP_PMET_1980_2020m.nc",  overwrite=TRUE, 
         varname="pp", unit="mm", zname="time", compression = 9, prec = "float")

writeRaster(pp_pmet_y, "Data/Precipitation/PP_PMET_1980_2020.tif", overwrite=TRUE)
print (paste0("4. pp files: Ok"))