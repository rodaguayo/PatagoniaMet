# Code for precipitation bias correction: Part Nº1 ------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2023)

rm(list=ls())
cat("\014")  

library("stats")
library("qmap")
library("terra")
library("caret")
library("hydroGOF")
library("doMC")

setwd("/home/rooda/Dropbox/Patagonia/")
terraOptions(memfrac=0.90)

# use cross-validation (10-fold cv)? 
CV = TRUE

# 1. Data: observations and gridded product -------------------------------------------------------
pp_stack_hr <- rast("Data/Precipitation/PP_ERA5_hr_1980_2020d.nc") # Gridded data
period <- seq(as.Date("1980-01-01"), as.Date("2020-12-31"), by = "day")
names(pp_stack_hr) <- time(pp_stack_hr)

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


for (cv in 1:10) {

  pp_shape <- read.csv("Data/Precipitation/PP_PMETobs_v10_metadata.csv") # Ground-base data
  pp_shape <- pp_shape[!pp_shape$gauge_id %in% c("X00000010", "X10360002", "X00085930"),] # bad fix
  pp_shape <- vect(pp_shape, geom=c("gauge_lon", "gauge_lat"), crs="epsg:4326", keepgeom=TRUE)
  pp_shape <- crop(pp_shape, pp_stack_hr)
  
  # Cross-validation
  set.seed(123)
  pp_shape$fold <- createFolds(pp_shape$gauge_id, k = 10, list = F, returnTrain = F)
  if (cv == 1) {write.csv(as.data.frame(pp_shape), "MS1 Results/CV_PP.csv", row.names = FALSE)}
  if (CV) { pp_shape <- pp_shape[pp_shape$fold != cv,] } 
  
  pp_obs   <- read.csv("Data/Precipitation/PP_PMETobs_1950_2020_v10d.csv")
  pp_obs$Date <- as.Date(pp_obs$Date) # the date is the first column
  pp_obs   <- subset(pp_obs, Date >= min(period) & Date <= max(period))
  pp_obs   <- pp_obs[pp_shape$gauge_id]
  
  pp_sim   <- as.data.frame(t(terra::extract(pp_stack_hr, pp_shape, method='simple')))[-1,]
  
  rf_importance <- data.frame(matrix(ncol = 3 + nlyr(covariates), nrow = 0))
  colnames(rf_importance)  <- c("parameter", "var", "month", names(covariates))
  rf_importance[] <- lapply(rf_importance, as.character)
  
  rf_performance <- data.frame(matrix(ncol = 4, nrow = 0))
  QM_parameters  <- data.frame(matrix(ncol = 4, nrow = 0))
  
  # validation set for spatial regression
  train_control <- rfeControl(functions = rfFuncs, # random forest
                              method = "LGOCV",    # leave-group-out cross validation
                              p = 0.90,            # the training percentage
                              number = 100,        # number of repetitions
                              allowParallel = TRUE)
  
  for (month in sprintf("%02d", 1:12)) {
    
    print(paste0("Month ", month))
    pp_obs_m <- subset(pp_obs, format(period,"%m") == month)
    pp_sim_m <- subset(pp_sim, format(period,"%m") == month)
    pp_stack_hr_m <- pp_stack_hr[[format(time(pp_stack_hr),"%m")  == month]]
    
    # 2. Parametric Quantile mapping ----------------------------------------------------------------
    QM_parameters_m <- data.frame(matrix(0,ncol(pp_obs),6))
    colnames(QM_parameters_m)<-c("var","month","a_linear","b_linear", "KGE_initial", "KGE_linear") 
    
    for (i in 1:length(pp_shape)) {
      fit_ptf_linear  <- fitQmapPTF(pp_obs_m[,i], pp_sim_m[,i], transfun="linear",  wet.day=FALSE, cost="RSS")
      pp_ptf_linear   <- doQmapPTF(pp_sim_m[,i], fit_ptf_linear)
      
      QM_parameters_m$var    <- "PP"
      QM_parameters_m$month  <- month
      QM_parameters_m[i,3:4] <- fit_ptf_linear$par
      QM_parameters_m[i,5:6] <- c(KGE(sim = pp_sim_m[,i],  obs=pp_obs_m[,i], method="2012", na.rm=TRUE), 
                                  KGE(sim = pp_ptf_linear, obs=pp_obs_m[,i], method="2012", na.rm=TRUE))
    }
    
    QM_parameters <- rbind(QM_parameters, QM_parameters_m[,1:4])
    
    ## 2.2. b linear parameter (a + pp *b) ----------------------------------------------------------
    model_values <- data.frame(extract(covariates, pp_shape), b_linear = QM_parameters_m$b_linear)[,-1]
  
    registerDoMC(cores = 14)
    set.seed(123)
    rf_model <- rfe(x = model_values[names(covariates)], y = model_values$b_linear, 
                    sizes = c(2:nlyr(covariates)), metric = "RMSE", 
                    rfeControl = train_control, ntree = 500)
    
    rf_importante_i <- c(parameter = "b_linear", var = "PP", month = month, randomForest::importance(rf_model$fit)[,1])
    rf_importance   <- dplyr::bind_rows(rf_importance, rf_importante_i)
    performance_i   <- c(mean(rf_model$resample$RMSE), sd(rf_model$resample$RMSE),
                        mean(rf_model$resample$Rsquared**0.5), sd(rf_model$resample$Rsquared**0.5))
    rf_performance  <- rbind(rf_performance, c("pp_b", "pp", month, performance_i))
    b_linear        <- predict(covariates[[predictors(rf_model)]], rf_model$fit, type='response', na.rm = T)
    b_linear        <- focal(b_linear, w = focalMat(b_linear, 0.04, "Gauss"), pad = TRUE, na.rm=T)
  
    ## 2.1. a linear parameter (a + pp * b) ---------------------------------------------------------
    model_values <- data.frame(extract(covariates, pp_shape), a_linear = QM_parameters_m$a_linear)[,-1]
  
    registerDoMC(cores = 14)
    set.seed(123)
    rf_model <- rfe(x = model_values[names(covariates)], y = model_values$a_linear, 
                    sizes = c(1:nlyr(covariates)), metric = "RMSE",
                    rfeControl = train_control, ntree = 500)
    
    rf_importante_i <- c(parameter = "a_linear", var = "PP", month = month, randomForest::importance(rf_model$fit)[,1])
    rf_importance   <- dplyr::bind_rows(rf_importance, rf_importante_i)
    performance_i   <- c(mean(rf_model$resample$RMSE), sd(rf_model$resample$RMSE),
                         mean(rf_model$resample$Rsquared**0.5), sd(rf_model$resample$Rsquared**0.5))
    rf_performance  <- rbind(rf_performance, c("pp_a", "pp", month, performance_i))
    a_linear        <- predict(covariates[[predictors(rf_model)]], rf_model$fit, type='response', na.rm = T)
    a_linear        <- focal(a_linear, w = focalMat(a_linear, 0.04, "Gauss"), pad = TRUE, na.rm=T)
    
    
    ## 2.3. PQM correction for PMET -----------------------------------------------------------------
    a_linear[covariates$elevation <= 1] <- NA # check
    b_linear[covariates$elevation <= 1] <- NA
    
    pp_stack_hr_m <- a_linear + pp_stack_hr_m * b_linear # this is too slow and needs a lot of RAM
    pp_stack_hr_m <- classify(pp_stack_hr_m, cbind(-Inf, 0, 0), right=FALSE)
    
    terra::time(pp_stack_hr_m) <- time(pp_stack_hr[[format(time(pp_stack_hr),"%m")  == month]])
    
    if (!CV) { path <- paste0("Data/Precipitation/PP_PMET_1980_2020d_", month, "_npc.nc")
    } else {   path <-  paste0("/home/rooda/PMET_results/Precipitation/raw/PP_PMET_1980_2020d_", month, "_", cv, "_npc.nc")}

    writeCDF(pp_stack_hr_m, path, overwrite=TRUE, varname="pp", unit="mm", zname="time", prec = "float",  shuffle = T)
    
  }
  if (!CV) {break} 
  print(paste0(cv,"-fold ok!"))
}

# save RF results: importance, performance and parameters
if (!CV) {
  
  colnames(rf_importance)  <- c("parameter", "var", "month", names(covariates))
  write.csv(rf_importance, "MS1 Results/RF_PP_importance.csv", row.names = FALSE)
  
  colnames(rf_performance) <- c("parameter", "var", "month", "RMSE_mean", "RMSE_sd", "rPearson_mean", "rPearson_sd")
  write.csv(rf_performance, "MS1 Results/RF_PP_performance.csv", row.names = FALSE)
  
  write.csv(QM_parameters, "MS1 Results/PP_parameters.csv", row.names = FALSE)
} 
