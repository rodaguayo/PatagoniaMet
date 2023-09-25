# Code to replicate temperature validation -------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2023)

rm(list=ls())
cat("\014")  

library("hydroGOF")
library("caret")
library("terra")

setwd("/home/rooda/Dropbox/Patagonia/Data/Temperature/")
period     <- c(as.POSIXct("1980-01-01"), as.POSIXct("2020-12-31"))
attributes <- c("gauge_id", "gauge_name","institution", "gauge_lat", "gauge_lon", "gauge_alt")

# 1. Reference ------------------------------------------------------------------------------------

# Simulations (1980-2020 subset)
t2m_stacks <- list(ERA5  = rast("Tavg_ERA5_1959_2021m.nc"),     # ERA5 
                   ERA5d  = rast("Tavg_ERA5_hr_1980_2020m.nc"),  # ERA5d 
                   ERA5L  = rast("Tavg_ERA5L_1950_2021m.nc"),    # ERA5L
                   MERRA2 = rast("Tavg_MERRA2_1980_2021m.nc"),   # MERRA2 
                   CSFR   = rast("Tavg_CSFR_1979_2019m.nc"),     # CSFR
                   CR2REG = rast("Tavg_REGCR2_1980_2015m.nc"),   # CR2REG 
                   CR2MET = rast("Tavg_CR2MET_1960_2021m.nc"),   # CR2MET v2.5
                   MSWX   = rast("Tavg_MSWX_1979_2021m.nc"),     # MSWX 
                   W5E5   = rast("Tavg_W5E5_1979_2019m.nc"),     # W5D5 v2.0
                   PMET   = rast("Tavg_PMET_1980_2020m.nc"))     # PMET v1.0 

# dems for temp correction
dem_hr    <- rast("/home/rooda/Dropbox/Patagonia/GIS South/dem_patagonia3f.tif")
dem_hr[is.na(dem_hr)] <- 0

dem_stacks <- list(ERA5   = resample(dem_hr, t2m_stacks$ERA5,   threads = T),   # ERA5 
                   ERA5d  = resample(dem_hr, t2m_stacks$ERA5d,  threads = T),   # ERA5d 
                   ERA5L  = resample(dem_hr, t2m_stacks$ERA5L,  threads = T),   # ERA5L
                   MERRA2 = resample(dem_hr, t2m_stacks$MERRA2, threads = T),   # MERRA2 
                   CSFR   = resample(dem_hr, t2m_stacks$CSFR,   threads = T),   # CSFR
                   CR2REG = resample(dem_hr, t2m_stacks$CR2REG, threads = T),   # CR2REG 
                   CR2MET = resample(dem_hr, t2m_stacks$CR2MET, threads = T),   # CR2MET v2.5
                   MSWX   = resample(dem_hr, t2m_stacks$MSWX,   threads = T),   # MSWX 
                   W5E5   = resample(dem_hr, t2m_stacks$W5E5,   threads = T),   # W5D5 v2.0
                   PMET   = resample(dem_hr, t2m_stacks$PMET,   threads = T))   # PMET v1.0 

# Observations (location and data)
t2m_validation  <- read.csv("Tavg_PMETobs_v10_metadata.csv")
t2m_shape <- vect(t2m_validation, geom=c("gauge_lon", "gauge_lat"), crs="epsg:4326")

t2m_obs      <- read.csv("Tavg_PMETobs_v10m.csv")
t2m_obs$Date <- as.POSIXct(t2m_obs$Date, tz= "UTC") # The date is the first column
t2m_validation <- subset(t2m_validation, select = attributes)

# reference
for (i in 1:length(t2m_stacks)) {
  
  t2m_stack <- t2m_stacks[[i]]
  terra::time(t2m_stack) <- as.POSIXct(time(t2m_stack), tz= "UTC") 
  t2m_stack <- subset(t2m_stack,  which(time(t2m_stack)  >= period[1] & time(t2m_stack)   <= period[2]))
  
  # time subset and correct obs temperature
  t2m_obs_s <- subset(t2m_obs, Date >= min(time(t2m_stack)) &  Date <= max(time(t2m_stack)))
  t2m_shape$Altitude_lr <- extract(dem_stacks[[i]], t2m_shape, method='simple')[,-1]
  t2m_obs_s      <- sweep(t2m_obs_s[,-1], 2, (t2m_shape$Altitude_lr - t2m_shape$gauge_alt)*0.0065)
  t2m_sim   <- as.data.frame(t(extract(t2m_stack, t2m_shape, method='simple'))[-1,])
  index     <- KGE(sim=t2m_sim, obs=t2m_obs_s, method="2009", out.type="full",na.rm=TRUE)
  index     <- index$KGE.elements[3,] # just use rSD
  index     <- data.frame(ME = me(sim=t2m_sim, obs=t2m_obs_s, na.rm=TRUE), rSD = index)
  colnames(index) <- paste0(names(t2m_stacks)[[i]], "_", colnames(index))
  t2m_validation  <- cbind(t2m_validation, index)
  print(names(t2m_stacks)[[i]])
}

write.csv(t2m_validation, "Tavg_Validation.csv",  row.names = FALSE)


# 2. Cross validation -----------------------------------------------------------------------------

t2m_stacks_cv <- list(PMET_CV1  = rast("CV/Tavg_PMETsim_1980_2020m_1.nc"),   
                      PMET_CV2  = rast("CV/Tavg_PMETsim_1980_2020m_2.nc"), 
                      PMET_CV3  = rast("CV/Tavg_PMETsim_1980_2020m_3.nc"), 
                      PMET_CV4  = rast("CV/Tavg_PMETsim_1980_2020m_4.nc"),
                      PMET_CV5  = rast("CV/Tavg_PMETsim_1980_2020m_5.nc"),
                      PMET_CV6  = rast("CV/Tavg_PMETsim_1980_2020m_6.nc"),
                      PMET_CV7  = rast("CV/Tavg_PMETsim_1980_2020m_7.nc"),
                      PMET_CV8  = rast("CV/Tavg_PMETsim_1980_2020m_8.nc"),
                      PMET_CV9  = rast("CV/Tavg_PMETsim_1980_2020m_9.nc"), 
                      PMET_CV10 = rast("CV/Tavg_PMETsim_1980_2020m_10.nc")) 

# dems for temp correction
dem_hr    <- rast("/home/rooda/Dropbox/Patagonia/GIS South/dem_patagonia3f.tif")
dem_hr[is.na(dem_hr)] <- 0

dem_stacks <- list(PMET_CV1  = resample(dem_hr, t2m_stacks_cv$PMET_CV1, threads = T),  
                   PMET_CV2  = resample(dem_hr, t2m_stacks_cv$PMET_CV1, threads = T), 
                   PMET_CV3  = resample(dem_hr, t2m_stacks_cv$PMET_CV1, threads = T),
                   PMET_CV4  = resample(dem_hr, t2m_stacks_cv$PMET_CV1, threads = T), 
                   PMET_CV5  = resample(dem_hr, t2m_stacks_cv$PMET_CV1, threads = T), 
                   PMET_CV6  = resample(dem_hr, t2m_stacks_cv$PMET_CV1, threads = T), 
                   PMET_CV7  = resample(dem_hr, t2m_stacks_cv$PMET_CV1, threads = T),
                   PMET_CV8  = resample(dem_hr, t2m_stacks_cv$PMET_CV1, threads = T), 
                   PMET_CV9  = resample(dem_hr, t2m_stacks_cv$PMET_CV1, threads = T),
                   PMET_CV10 = resample(dem_hr, t2m_stacks_cv$PMET_CV1, threads = T))

# Observations (location and data)
t2m_validation  <- read.csv("Tavg_PMETobs_v10_metadata.csv")
t2m_shape <- vect(t2m_validation, geom=c("gauge_lon", "gauge_lat"), crs="epsg:4326")

t2m_obs      <- read.csv("Tavg_PMETobs_v10m.csv")
t2m_obs$Date <- as.POSIXct(t2m_obs$Date, tz= "UTC") # The date is the first column
t2m_validation <- subset(t2m_validation, select = attributes)

# reference
for (i in 1:length(t2m_stacks_cv)) {
  
  t2m_stack <- t2m_stacks_cv[[i]]
  terra::time(t2m_stack) <- as.POSIXct(time(t2m_stack), tz= "UTC") 
  t2m_stack <- subset(t2m_stack,  which(time(t2m_stack)  >= period[1] & time(t2m_stack)   <= period[2]))
  
  # time subset and correct obs temperature
  t2m_obs_s <- subset(t2m_obs, Date >= min(time(t2m_stack)) &  Date <= max(time(t2m_stack)))
  t2m_shape$Altitude_lr <- extract(dem_stacks[[i]], t2m_shape, method='simple')[,-1]
  t2m_obs_s      <- sweep(t2m_obs_s[,-1], 2, (t2m_shape$Altitude_lr - t2m_shape$gauge_alt)*0.0065)
  t2m_sim   <- as.data.frame(t(extract(t2m_stack, t2m_shape, method='simple'))[-1,])
  index     <- KGE(sim=t2m_sim, obs=t2m_obs_s, method="2009", out.type="full",na.rm=TRUE)
  index     <- index$KGE.elements[3,] # just use rSD
  index     <- data.frame(ME = me(sim=t2m_sim, obs=t2m_obs_s, na.rm=TRUE), rSD = index)
  colnames(index) <- paste0(names(t2m_stacks_cv)[[i]], "_", colnames(index))
  t2m_validation  <- cbind(t2m_validation, index)
  print(names(t2m_stacks_cv)[[i]])
}


set.seed(123)
t2m_shape <- read.csv("Tavg_PMETobs_v10_metadata.csv")[-1,] # bad fix
cv_folds <- createFolds(t2m_shape$gauge_id, k = 10, list = T, returnTrain = T)

for (i in 1:length(t2m_stacks_cv)) {
  cv_i <- paste0("CV", i, "_")
  t2m_validation[cv_folds[[i]], grepl(cv_i, colnames(t2m_validation))] <- NA
}

me_cv <-  as.numeric(rowSums(t2m_validation[, grepl("_ME",  colnames(t2m_validation))], na.rm = T))
me_rSD <- as.numeric(rowSums(t2m_validation[, grepl("_rSD", colnames(t2m_validation))], na.rm = T))
me_cv[me_cv == 0] <- NA
me_rSD[me_rSD == 0] <- NA

t2m_validation <- subset(t2m_validation, select = attributes)
t2m_validation$me_cv  <- me_cv
t2m_validation$me_rSD <- me_rSD
write.csv(t2m_validation, "Tavg_Validation_CV.csv",  row.names = FALSE)


