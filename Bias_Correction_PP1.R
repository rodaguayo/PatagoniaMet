# Code for precipitation bias correction ---------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("stats")
library("qmap")
library("terra")
library("hydroGOF")
library("randomForest")

setwd("/home/rooda/Dropbox/Patagonia/")
terraOptions(memfrac=0.90)
set.seed(123)

# 1. Data: observations and gridded product -------------------------------------------------------
pp_stack_hr <- rast("Data/Precipitation/PP_ERA5_hr_1980_2020d.nc") # Gridded data
period <- seq(as.Date("1980-01-01"), as.Date("2020-12-31"), by = "day")
names(pp_stack_hr) <- time(pp_stack_hr)

pp_shape <- read.csv("Data/Precipitation/PP_PMETobs_v10_metadata.csv") # Ground-base data
pp_shape <- vect(pp_shape, geom=c("Longitude", "Latitude"), crs="epsg:4326")
pp_shape <- crop(pp_shape, pp_stack_hr)
pp_obs   <- read.csv("Data/Precipitation/PP_PMETobs_v10d.csv")
pp_obs$Date <- as.Date(pp_obs$Date) #The date is the first column
pp_obs   <- subset(pp_obs, Date >= min(period) & Date <= max(period))
pp_obs   <- pp_obs[pp_shape$ID]
pp_sim   <- as.data.frame(t(extract(pp_stack_hr, pp_shape, method='simple')))[-1,]
print ("1. Data: Ok")

# list of covariables used in RF regressions
covariates <- c(rast("GIS South/climate_class005.tif"),              # Climate class (discrete)
                rast("GIS South/dist_coast005l.tif"),                # Distance to coast (log m)
                rast("GIS South/dem_patagonia005.tif"),              # Altitude (masl)
                rast("Data/Temperature/Tavg_ERA5_hr_1980_2020.tif"), # Annual temperature (degC)
                rast("Data/Precipitation/PP_ERA5_hr_1980_2020.tif"), # Annual precipitation (mm)
                rast("GIS South/clouds_005.tif"),                    # Cloud cover (5)
                rast("GIS South/west_gradient005.tif"),              # West grandient (m/m)
                rast("GIS South/aspect_dem005.tif"))                 # Aspect (deg)
names(covariates) <- c("climate_class", "distance_coast", "elevation", "t2m", "pp", "cloud_cover", "west_gradient", "aspect")
covariates <- list(covariates, covariates[[-8]]) # Remove Aspect

rf_performance<- data.frame(matrix(ncol = 4, nrow = 0))
rf_importance <- data.frame(matrix(ncol = 3 + nlyr(covariates[[1]]), nrow = 0))
QM_parameters <- data.frame(matrix(ncol = 4, nrow = 0))

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
    QM_parameters_m[i,5:6] <- c(KGE(sim = pp_sim_m[,i],  obs=pp_obs_m[,i], method="2012" ,na.rm=TRUE), 
                              KGE(sim = pp_ptf_linear, obs=pp_obs_m[,i], method="2012", na.rm=TRUE))
  }
  
  QM_parameters <- rbind(QM_parameters, QM_parameters_m[,1:4])
  print ("2. PQM parameters: Ok")
  
  ## 2.2. b linear parameter (a + pp *b) ----------------------------------------------------------
  model_values <- data.frame(extract(covariates[[1]], pp_shape), b_linear = QM_parameters$b_linear)[,-1]
  model_values$climate_class <- as.factor(model_values$climate_class)
  rf_model     <- randomForest(b_linear ~., data = model_values, ntree = 2000, importance = TRUE)
  rf_performance <- rbind(rf_performance, c("pp_b", "pp", month, mae(rf_model$predicted, rf_model$y)))
  rf_importance <- rbind(rf_importance, c("a linear", "PP", month, importance(rf_model)[,1]))
  b_linear     <- predict(covariates[[1]], rf_model, type='response')
  b_linear     <- focal(b_linear, w = focalMat(b_linear, 0.04, "Gauss"), pad = TRUE)
  print ("2.2. a and b linear parameters: Ok")
  
  ## 2.1. a linear parameter (a + pp * b) ---------------------------------------------------------
  model_values <- data.frame(extract(covariates[[2]], pp_shape), a_linear = QM_parameters$a_linear)[,-1]
  model_values$climate_class <- as.factor(model_values$climate_class)
  rf_model     <- randomForest(a_linear ~., data = model_values, ntree = 2000, importance = TRUE)
  rf_performance <- rbind(rf_performance, c("pp_a", "pp", month, mae(rf_model$predicted, rf_model$y)))
  rf_importance <- rbind(rf_importance, c("b linear", "PP", month, importance(rf_model)[,1], NA))
  a_linear     <- predict(covariates[[2]], rf_model, type='response')
  a_linear     <- focal(a_linear, w = focalMat(a_linear, 0.04, "Gauss"), pad = TRUE)
  
  ## 2.3. PQM correction for PMET -----------------------------------------------------------------
  a_linear[covariates$elevation <= 1] <- NA # check
  b_linear[covariates$elevation <= 1] <- NA
  
  pp_stack_hr_m <- a_linear + pp_stack_hr_m * b_linear # this is too slow and needs a lot of RAM
  pp_stack_hr_m <- classify(pp_stack_hr_m, cbind(-Inf, 0, 0), right=FALSE)
  
  print ("2.3. PQM applied to original netcdf: Ok")
  
  writeCDF(pp_stack_hr_m, paste0("Data/Precipitation/PP_PMET_1980_2020d_", month, "_npc.nc"),  
           overwrite=TRUE, varname="pp", unit="mm", zname="time", prec = "float",  shuffle = T)

}

colnames(rf_importance)  <- c("parameter", "var", "month", names(covariates[[1]]))
write.csv(rf_importance, "MS1 Results/RF_PP_importance.csv", row.names = FALSE)

colnames(rf_performance)  <- c("parameter", "var", "month", "mae")
write.csv(rf_performance, "MS1 Results/RF_PP_performance.csv", row.names = FALSE)

write.csv(QM_parameters, "MS1 Results/PP_parameters.csv", row.names = FALSE)
