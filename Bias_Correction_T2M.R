# Code for temperature bias correction: Variance and mean scaling -----------------------------------
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("randomForest")
library("hydroGOF")
library("terra")

setwd("/home/rooda/Dropbox/Patagonia/")
terraOptions(memfrac=0.9)
terraOptions(verbose=T)
set.seed(123)

# Variables to analyse
t2m_variables <- c("Tmax", "Tmin")

# list of covariables used in RF regressions
covariates <- c(rast("GIS South/aspect_dem005.tif"),                 # Aspect (deg)
                rast("GIS South/climate_class005.tif"),              # Climate class (discrete)
                rast("GIS South/dist_coast005l.tif"),                # Distance to coast (log m)
                rast("GIS South/dem_patagonia005.tif"),              # Altitude (masl)
                rast("Data/Temperature/T2M_ERA5_hr_1980_2020.tif"),  # Annual temperature (degC)
                rast("Data/Precipitation/PP_ERA5_hr_1980_2020.tif")) # Annual precipitation (mm)
names(covariates) <- c("aspect", "climate_class", "distance_coast", "elevation", "t2m", "pp")

for (t2m in t2m_variables) {

  # 1. Data: observations and gridded product -----------------------------------------------------
  t2m_stack_hr <- rast(paste0("Data/Temperature/", t2m, "_ERA5_hr_1980_2020d.nc"))
  period <- c(min(time(t2m_stack_hr)), max(time(t2m_stack_hr)))
  names(t2m_stack_hr)<-time(t2m_stack_hr)

  t2m_shape <- read.csv("Data/Temperature/Metadata_Temperature_v10.csv")
  t2m_shape <- vect(t2m_shape, geom=c("Longitude", "Latitude"), crs="epsg:4326")
  t2m_shape <- crop(t2m_shape,t2m_stack_hr)
  t2m_obs   <- read.csv(paste0("Data/Temperature/Data_Temperature_", t2m, "_v10_daily.csv"))
  t2m_obs$Date <- as.Date(t2m_obs$Date) #The date is the first column
  t2m_obs   <- subset(t2m_obs, Date >= period[1] & Date <= period[2])
  t2m_obs   <- t2m_obs[t2m_shape$ID]
  print (paste0("1. ", t2m, " data: Ok"))
  
  # 2. Mean and variance scaling ------------------------------------------------------------------
  t2m_sim        <- as.data.frame(t(extract(t2m_stack_hr, t2m_shape, method='simple')))[-1,]
  mva_parameters     <- data.frame(matrix(0,length(t2m_shape),0), row.names = names(t2m_shape$ID))
  mva_parameters$me  <- me(sim=t2m_sim,   obs=t2m_obs, na.rm=TRUE)
  mva_parameters$rSD <- rSD(sim=t2m_sim,  obs=t2m_obs, na.rm=TRUE)
  print (paste0("2. ", t2m, " parameters: Ok"))
  
  ## 2.1 me parameter ------------------------------------------------------------------------------
  model_values <- data.frame(extract(covariates, t2m_shape), me_param = mva_parameters$me)[,-1]
  model_values$climate_class <- as.factor(model_values$climate_class)
  rf_model     <- randomForest(me_param ~., data = model_values, ntree = 2000)
  me_param     <- predict(covariates, rf_model, type='response')
  me_param     <- focal(me_param, w = focalMat(me_param, 0.04, "Gauss"), pad = TRUE)
  print (paste0("2.1 ", t2m, " RF me: Ok"))

  ## 2.2 rSD parameter -----------------------------------------------------------------------------
  model_values <- data.frame(extract(covariates, t2m_shape), rSD_param = mva_parameters$rSD)[,-1]
  model_values$climate_class <- as.factor(model_values$climate_class)
  rf_model     <- randomForest(rSD_param ~., data = model_values, ntree = 2000)
  rSD_param    <- predict(covariates, rf_model, type='response')
  rSD_param    <- focal(rSD_param, w = focalMat(rSD_param, 0.04, "Gauss"), pad = TRUE)
  print (paste0("2.2 ", t2m, " RF rSD: Ok"))

  # 3. Bias correction application ----------------------------------------------------------------
  
  me_param[covariates$elevation <= 1] <- NA
  rSD_param[covariates$elevation <= 1] <- NA

  t2m_stack_hrc  <- t2m_stack_hr  - me_param
  t2m_pmet       <- t2m_stack_hrc - mean(t2m_stack_hrc)
  t2m_pmet       <- t2m_pmet      * rSD_param
  t2m_pmet       <- t2m_pmet      + mean(t2m_stack_hrc)

  print (paste0("3 ", t2m, " Bias correction: Ok"))
  
  # 4. Saving and resampling files ----------------------------------------------------------------
  t2m_pmet_m <- tapp(t2m_pmet,  strftime(time(t2m_pmet),format="%Y-%m"), fun = mean, na.rm = TRUE)
  terra::time(t2m_pmet_m) <- seq(from = period[1], to = period[2], by = "month")
  t2m_pmet_y <- tapp(t2m_pmet_m, strftime(time(t2m_pmet_m),format="%Y"), fun = mean, na.rm = TRUE)
  t2m_pmet_y <- mean(t2m_pmet_y)
  print (paste0("3. ", t2m, " writing files: in progress"))
  
  writeCDF(t2m_pmet, paste0("Data/Temperature/", t2m, "_PMET_1980_2020d.nc"),  overwrite=TRUE, 
           varname=t2m, unit="degC", zname="time", prec = "float", shuffle = T)
  
  writeCDF(t2m_pmet_m, paste0("Data/Temperature/", t2m, "_PMET_1980_2020m.nc"),  overwrite=TRUE, 
           varname=t2m,  compression = 9, unit="degC", zname="time", prec = "float")
  
  writeRaster(t2m_pmet_y, paste0("Data/Temperature/", t2m, "_PMET_1980_2020.tif"), overwrite=TRUE)
  
  print (paste0("4. ", t2m, " files: Ok"))
  
  }
