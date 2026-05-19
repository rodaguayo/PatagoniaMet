# Code for temperature bias correction: Variance and mean scaling -----------------------------------
# Developed by Rodrigo Aguayo (2020-2023)

rm(list=ls())
cat("\014")  

library("randomForest")
library("hydroGOF")
library("caret")
library("terra")

setwd("/home/rooda/Dropbox/Patagonia/")
terraOptions(memfrac=0.9)

# Variables to analyse
t2m_variables <- c("Tmax", "Tmin")
period <- seq(as.Date("1980-01-01"), as.Date("2020-12-31"), by = "day")

# list of covariables used in RF regressions
covariates <- c(rast("GIS South/dist_coast005l.tif"),                # Distance to coast (log m)
                rast("GIS South/dem_patagonia005.tif"),              # Altitude (masl)
                rast("Data/Temperature/Tavg_ERA5_hr_1980_2020.tif"), # Annual temperature (degC)
                rast("Data/Precipitation/PP_ERA5_hr_1980_2020.tif"), # Annual precipitation (mm)
                rast("GIS South/clouds_005.tif"),                    # Cloud cover (%)
                rast("GIS South/west_gradient005.tif"),              # West gradient
                rast("GIS South/aspect_dem005.tif"),                 # Aspect
                rast("GIS South/aridity_index_ERA5_hr_005.tif"))     # Aridity index
names(covariates) <- c("distance_coast", "elevation", "t2m", "pp", "cloud_cover", 
                       "west_gradient", "aspect", "aridity_index")
covariates[is.infinite(covariates)] <- NA # bug in aridity_index

rf_importance  <- data.frame(matrix(ncol = nlyr(covariates)+3, nrow = 0))
colnames(rf_importance)  <- c("parameter", "var", "month", names(covariates))
rf_importance[] <- lapply(rf_importance, as.character)

rf_performance <- data.frame(matrix(ncol = 7, nrow = 0))
mva_parameters <- data.frame(matrix(ncol = 4, nrow = 0))

# validation set for spatial regression
train_control <- rfeControl(functions = rfFuncs, # random forest
                            method = "LGOCV",    # leave-group-out cross validation
                            p = 0.90,            # the training percentage
                            number = 100)        # number of repetitions 

for (t2m in t2m_variables) {

  # 1. Data: observations and gridded product -----------------------------------------------------
  t2m_stack_hr <- rast(paste0("Data/Temperature/", t2m, "_ERA5_hr_1980_2020d.nc"))
  names(t2m_stack_hr)<-time(t2m_stack_hr)

  t2m_shape <- read.csv("Data/Temperature/Tavg_PMETobs_v10_metadata.csv")[-1,] # bad fix
  t2m_shape <- vect(t2m_shape, geom=c("gauge_lon", "gauge_lat"), crs="epsg:4326")
  t2m_shape <- crop(t2m_shape, t2m_stack_hr)
  t2m_shape$Altitude_lr <- extract(covariates$elevation, t2m_shape, method='simple')[,-1]
  
  t2m_obs   <- read.csv(paste0("Data/Temperature/",t2m, "_PMETobs_v10d.csv"))
  t2m_obs$Date <- as.Date(t2m_obs$Date) #The date is the first column
  t2m_obs   <- subset(t2m_obs, Date >= min(period) & Date <= max(period))
  t2m_obs   <- t2m_obs[t2m_shape$gauge_id]
  
  print (paste0("1. ", t2m, " data: Ok"))
  
  # 2. Mean and variance scaling ------------------------------------------------------------------
  t2m_sim   <- as.data.frame(t(extract(t2m_stack_hr, t2m_shape, method='simple')))[-1,]
  t2m_sim   <- sweep(t2m_sim, 2, (t2m_shape$Altitude_lr - t2m_shape$gauge_alt)*-0.0065)
  
  for (month in sprintf("%02d", 1:12)) {
    t2m_obs_m <- subset(t2m_obs, format(period,"%m") == month)
    t2m_sim_m <- subset(t2m_sim, format(period,"%m") == month)
    t2m_stack_hr_m <- t2m_stack_hr[[format(time(t2m_stack_hr),"%m")  == month]]
    
    mva_parameters_m        <- data.frame(matrix(0,ncol(t2m_obs),0))
    mva_parameters_m$var    <- t2m
    mva_parameters_m$month  <- month
    mva_parameters_m$me_obs  <- me(sim=t2m_sim_m,   obs=t2m_obs_m, na.rm=TRUE)
    mva_parameters_m$rSD_obs <- rSD(sim=t2m_sim_m,  obs=t2m_obs_m, na.rm=TRUE)
    print (paste0("2. ", t2m, " parameters: Ok"))
  
    ## 2.1 me parameter ------------------------------------------------------------------------------
    model_values  <- data.frame(extract(covariates, t2m_shape), me_param = mva_parameters_m$me)[,-1]
    
    set.seed(123)
    rf_model <- rfe(x = model_values[names(covariates)], y = model_values$me_param, 
                    sizes = c(1:nlyr(covariates)), metric = "RMSE", 
                    rfeControl = train_control, ntree = 500)
    
    rf_importante_i <- c(parameter = "me", var = t2m, month = month, importance(rf_model$fit)[,1])
    rf_importance  <- dplyr::bind_rows(rf_importance, rf_importante_i)
    performance_i  <- c(mean(rf_model$resample$RMSE), sd(rf_model$resample$RMSE),
                        mean(rf_model$resample$Rsquared**0.5), sd(rf_model$resample$Rsquared**0.5))
    rf_performance <- rbind(rf_performance, c("me", t2m, month, performance_i))
    me_param      <- terra::predict(covariates[[predictors(rf_model)]], rf_model$fit, type='response', na.rm = T)
    #me_param      <- focal(me_param, w = focalMat(me_param, 0.03, "Gauss"), expand = T)
    print (paste0("2.1 ", t2m, " RF me: Ok"))
    
    ## 2.2 rSD parameter -----------------------------------------------------------------------------
    model_values  <- data.frame(extract(covariates, t2m_shape), rSD_param = mva_parameters_m$rSD)[,-1]
    
    set.seed(123)
    rf_model <- rfe(x = model_values[names(covariates)], y = model_values$rSD, 
                    sizes = c(1:nlyr(covariates)), metric = "RMSE", 
                    rfeControl = train_control, ntree = 500)
    
    rf_importante_i <-  c(parameter = "rSD", var = t2m, month = month, importance(rf_model$fit)[,1])
    rf_importance  <- dplyr::bind_rows(rf_importance, rf_importante_i)
    performance_i  <- c(mean(rf_model$resample$RMSE), sd(rf_model$resample$RMSE),
                        mean(rf_model$resample$Rsquared**0.5), sd(rf_model$resample$Rsquared**0.5))
    rf_performance <- rbind(rf_performance, c("rSD", t2m, month, performance_i))
    rSD_param      <- predict(covariates[[predictors(rf_model)]], rf_model$fit, type='response', na.rm = TRUE)
    #rSD_param     <- focal(rSD_param, w = focalMat(rSD_param, 0.03, "Gauss"), expand = T)
    mva_parameters <- rbind(mva_parameters, mva_parameters_m)
    print (paste0("2.2 ", t2m, " RF rSD: Ok"))
  
   # 3. Bias correction application ----------------------------------------------------------------
    
    me_param[covariates$elevation <= 1] <- NA
    rSD_param[covariates$elevation <= 1] <- NA
  
    t2m_stack_hrc  <- t2m_stack_hr_m  - me_param
    t2m_pmet       <- t2m_stack_hrc - mean(t2m_stack_hrc)
    t2m_pmet       <- t2m_pmet      * rSD_param
    t2m_pmet       <- t2m_pmet      + mean(t2m_stack_hrc)
  
    print (paste0("3 ", t2m, " Bias correction: Ok"))
    
    # 4. Saving and resampling files ----------------------------------------------------------------
    print (paste0("3. ", t2m, " writing files: in progress"))
    
    # problem with extent, fix is coming to remove -nans
    writeCDF(t2m_pmet, paste0("Data/Temperature/", t2m, "_PMET_1980_2020d_", month, ".nc"),  
             overwrite=TRUE, varname=t2m, unit="degC", zname="time", prec = "float", shuffle = T)
    
    print (paste0("4. ", month, t2m, " files: Ok"))
    }
  }

colnames(rf_importance)  <- c("parameter", "var", "month", names(covariates))
write.csv(rf_importance, "MS1 Results/RF_T2M_importance.csv", row.names = FALSE)

colnames(rf_performance)  <- c("parameter", "var", "month", "RMSE_mean", "RMSE_sd", "rPearson_mean", "rPearson_sd")
write.csv(rf_performance, "MS1 Results/RF_T2M_performance.csv", row.names = FALSE)

write.csv(mva_parameters, "MS1 Results/T2M_parameters.csv", row.names = FALSE)