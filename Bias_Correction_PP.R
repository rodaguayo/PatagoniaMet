# Code for precipitation bias correction ---------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("stats")
library("qmap")
library("terra")
library("hydroGOF")
library("exactextractr")
library("randomForest")

setwd("/home/rooda/Dropbox/Patagonia/")
terraOptions(memfrac=0.90)
terraOptions(verbose=T)
set.seed(123)

# 1. Data: observations and gridded product -------------------------------------------------------
pp_stack_hr <- rast("Data/Precipitation/PP_ERA5_hr_1980_2020d.nc") # Gridded data
period <- c(time(pp_stack_hr)[1], max(time(pp_stack_hr)))
names(pp_stack_hr)<-time(pp_stack_hr)

pp_shape <- read.csv("Data/Precipitation/Metadata_Precipitation_v10.csv") # Ground-base data
pp_shape <- vect(pp_shape, geom=c("Longitude", "Latitude"), crs="epsg:4326")
pp_shape <- crop(pp_shape, pp_stack_hr)
pp_obs   <- read.csv("Data/Precipitation/Data_Precipitation_v10_daily.csv")
pp_obs$Date <- as.Date(pp_obs$Date) #The date is the first column
pp_obs   <- subset(pp_obs, Date >= period[1] & Date <= period[2])
pp_obs   <- pp_obs[pp_shape$ID]
pp_sim   <- as.data.frame(t(extract(pp_stack_hr, pp_shape, method='simple')))[-1,]
print ("1. Data: Ok")

# 2. Parametric Quantile mapping ------------------------------------------------------------------
QM_parameters <- data.frame(matrix(0,length(pp_shape),4), row.names = names(pp_obs))
colnames(QM_parameters)<-c("a_linear","b_linear", "KGE_initial", "KGE_linear") 

for (i in 1:length(pp_shape)) {
  fit_ptf_linear  <- fitQmapPTF(pp_obs[,i], pp_sim[,i], transfun="linear",  wet.day=0, cost="RSS")
  pp_ptf_linear  <- doQmapPTF(pp_sim[,i], fit_ptf_linear)
  
  QM_parameters[i,1:2] <- fit_ptf_linear$par
  QM_parameters[i,3:4] <- c(KGE(sim = pp_sim[,i],    obs=pp_obs[,i], method="2012" ,na.rm=TRUE), 
                            KGE(sim = pp_ptf_linear, obs=pp_obs[,i], method="2012", na.rm=TRUE))
  }
print ("2. PQM parameters: Ok")

dem <- rast("GIS South/dem_patagonia005.tif")
covariates <- c(rast("GIS South/clouds_005.tif"),                     # Cloud cover (%)
                rast("GIS South/climate_class005.tif"),               # Climate class (discrete)
                rast("GIS South/dist_coast005l.tif"),                 # Distance to coast (log m)
                rast("GIS South/dem_patagonia005.tif"),               # Altitude (masl)
                rast("Data/Temperature/T2M_ERA5_hr_1980_2020.tif"),   # Annual T2M (degC)
                rast("Data/Precipitation/PP_ERA5_hr_1980_2020.tif"))  # Annual PP (mm)
names(covariates) <- c("cloud_cover", "climate_class", "distance_coast", "elevation", "t2m", "pp")

## 2.1. a linear parameter (a + pp * b) -----------------------------------------------------------
model_values <- data.frame(extract(covariates, pp_shape), a_linear = QM_parameters$a_linear)[,-1]
model_values$climate_class <- as.factor(model_values$climate_class)
rf_model     <- randomForest(a_linear ~., data = model_values, ntree = 2000)
a_linear     <- predict(covariates, rf_model, type='response')
a_linear     <- focal(a_linear, w = focalMat(a_linear, 0.04, "Gauss"), pad = TRUE)

## 2.2. b linear parameter (a + pp *b) ------------------------------------------------------------
model_values <- data.frame(extract(covariates, pp_shape), b_linear = QM_parameters$b_linear)[,-1]
model_values$climate_class <- as.factor(model_values$climate_class)
rf_model     <- randomForest(b_linear ~., data = model_values, ntree = 2000)
b_linear     <- predict(covariates, rf_model, type='response')
b_linear     <- focal(b_linear, w = focalMat(b_linear, 0.04, "Gauss"), pad = TRUE)
print ("2.2. a and b linear parameters: Ok")

## 2.3. PQM correction for PMET -------------------------------------------------------------------
a_linear[covariates$elevation <= 1] <- NA
b_linear[covariates$elevation <= 1] <- NA

pp_stack_hr <- a_linear + pp_stack_hr * b_linear
pp_stack_hr <- classify(pp_stack_hr, cbind(-Inf, 0, 0), right=FALSE)

terra::time(pp_stack_hr) <- seq(from = period[1], to= period[2], by = "day")
pp_stack_hr_m <- tapp(pp_stack_hr, strftime(time(pp_stack_hr),format="%Y"), fun = sum, na.rm = TRUE)
pp_stack_hr_m <- mean(pp_stack_hr_m)
print ("2.3. PQM applied to original netcdf: Ok")

# 3. Precipitation factors ------------------------------------------------------------------------

## 3.1 w factor for Budyko framework --------------------------------------------------------------
basin_data        <- read.csv("Data/Streamflow/Metadata_Streamflow_v10.csv")
basin_data$PET_PP <- basin_data$PET_BH/basin_data$PP_BH
basin_data$ET_PP  <- basin_data$ET_BH/basin_data$PP_BH
basin_data$w      <- rep(NA, nrow(basin_data))

w_function <- function(PET_PP, ET_PP, w) 1 + PET_PP - ET_PP - (1+((PET_PP)^w))^(1/w)

for (basin in 1:nrow(basin_data)) { 
  if (!is.na(basin_data$PET_PP[basin])) { # Only basins with data in Chile (79 of 83)
    basin_data$w[basin] <- uniroot(w_function, interval=c(1, 5), tol = 0.001, 
                                   PET_PP=basin_data$PET_PP[basin], 
                                   ET_PP=basin_data$ET_PP[basin])$root
    }
  }
print ("3.1 w factors: Ok")

## 3.2 long-term precipitation  -------------------------------------------------------------------
pp_function <- function(PP, R, PET, w) 1 + PET/PP - ((PP-R)/PP) - (1+((PET/PP)^w))^(1/w)
basin_data$PP_TRUE <- rep(NA, nrow(basin_data))

for (basin in 1:nrow(basin_data)) {
  if (!is.na(basin_data$R_int_mm_year[basin])){ 
    basin_data$PP_TRUE[basin] <- uniroot(pp_function, interval=c(10, 15000), tol = 0.000001, 
                                         R   = basin_data$Qint_mm_y[basin], 
                                         PET = basin_data$PET_PMET[basin], 
                                         w   = median(basin_data$w, na.rm = T))$root 
  }
}
print ("3.2 long-term precipitation: Ok")

## 3.3 PP factor for each basin centroid ----------------------------------------------------------
basins_shp     <- vect("GIS South/Basins_Patagonia83.shp")
basins_shp     <- basins_shp[order(basins_shp$gridcode), ]
basin_data$pp_pmet_int <- extract(pp_stack_hr_m, basins_shp, fun = "mean", weights=TRUE, na.rm = T)[,2]
basin_data$BF_PMET <- round(basin_data$true_pp/basin_data$pp_pmet_int,3)
basin_data$BF_PMET[basin_data$Use == 0] <- NA
basins_shp     <- subset(basins_shp, basin_data$Use == 1)
basins_shp     <- centroids(basins_shp)
print ("3.3 PP factors: Ok")

## 3.4 random forest for PP factors ---------------------------------------------------------------
covariates <- c(rast("GIS South/west_gradient005.tif"),               # West gradient (m/m)
                rast("GIS South/aspect_dem005.tif"),                  # Aspect (deg)
                rast("GIS South/dist_coast005l.tif"),                 # Distance to coast (log m)
                rast("GIS South/dem_patagonia005.tif"),               # Altitude (masl)
                rast("Data/Temperature/T2M_ERA5_hr_1980_2020.tif"),   # Annual T2M (degC)
                rast("Data/Precipitation/PP_ERA5_hr_1980_2020.tif"))  # Annual PP (mm)
names(covariates) <- c("west_gradient", "aspect", "distance_coast", "elevation", "t2m", "pp")

model_values <- data.frame(extract(covariates, basins_shp), bias_factor = basins_shp$BF)[,-1]
model_values <- model_values[-nrow(model_values),]
rf_model     <- randomForest(bias_factor ~., data = model_values, ntree = 2000)
bias_factor     <- predict(covariates, rf_model, type='response')
bias_factor     <- focal(bias_factor, w = focalMat(a_linear, 0.04, "Gauss"), pad = TRUE)
bias_factor[bias_factor <= 1] <- 1

pp_pmet   <- pp_stack_hr*bias_factor
pp_pmet   <- round(pp_pmet, 0)
pp_pmet   <- pp_pmet*1 #trick to load into disk 
print ("3.4 RG regression: Ok")

# 4. Saving and resampling files ------------------------------------------------------------------
pp_pmet_m <- tapp(pp_pmet,   strftime(time(pp_pmet), format="%Y-%m"), fun = sum, na.rm = TRUE)
terra::time(pp_pmet_m) <- seq(from = period[1], to = period[2], by = "month")
pp_pmet_y <- tapp(pp_pmet_m, strftime(time(pp_pmet_m), format="%Y"),  fun = sum, na.rm = TRUE)
pp_pmet_y <- mean(pp_pmet_y)

writeCDF(pp_pmet, "Data/Precipitation/PP_PMET_1980_2020d.nc",  overwrite=TRUE, 
         varname="pp", unit="mm", zname="time", prec = "float",  shuffle = T)

writeCDF(pp_pmet_m, "Data/Precipitation/PP_PMET_1980_2020m.nc",  overwrite=TRUE, 
         varname="pp", unit="mm", zname="time", compression = 9, prec = "float")

writeRaster(pp_pmet_y, "Data/Precipitation/PP_PMET_1980_2020.tif", overwrite=TRUE)
print (paste0("4. pp files: Ok"))
