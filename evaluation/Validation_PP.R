# Code to replicate precipitation validation ------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2023)

rm(list=ls())
cat("\014")  

library("hydroGOF")
library("terra")
source("TimeResample.R")

setwd("/home/rooda/Dropbox/Patagonia/Data/Precipitation/")
period     <- c(as.POSIXct("1980-01-01"), as.POSIXct("2020-12-31"))
attributes <- c("gauge_id", "gauge_name","institution", "gauge_lat", "gauge_lon", "gauge_alt")

# 1. Reference ------------------------------------------------------------------------------------

# Observations (location and data)
pp_validation <- read.csv("PP_PMETobs_v10_metadata.csv")
pp_shape      <- vect(pp_validation, geom=c("gauge_lon", "gauge_lat"), crs="epsg:4326")
pp_obs        <- read.csv("PP_PMETobs_1950_2020_v10d.csv")
pp_obs        <- zoo(pp_obs[,-1], order.by = as.Date(pp_obs$Date))
pp_obs        <- as.data.frame(MonthlyResample(pp_obs,  20, FUN = sum))
pp_obs        <- cbind(Date = rownames(pp_obs), pp_obs)
pp_obs$Date   <- as.POSIXct(pp_obs$Date, tz= "UTC") # The date is the first column
pp_obs        <- subset(pp_obs, Date >= period[1] &  Date <= period[2])
pp_validation <- subset(pp_validation, select = attributes)

# Simulations (1980-2020 subset)
pp_stacks <- list(ERA5   = rast("PP_ERA5_1959_2021m.nc"),     # ERA5 
                  ERA5d  = rast("PP_ERA5_hr_1980_2020m.nc"),  # ERA5d 
                  ERA5L  = rast("PP_ERA5L_1950_2021m.nc"),    # ERA5L
                  MERRA2 = rast("PP_MERRA2_1980_2021m.nc"),   # MERRA2 
                  CSFR   = rast("PP_CSFR_1979_2019m.nc"),     # CSFR
                  CR2REG = rast("PP_REGCR2_1980_2015m.nc"),   # CR2REG 
                  CR2MET = rast("PP_CR2MET_1960_2021m.nc"),   # CR2MET v2.5
                  MSWEP  = rast("PP_MSWEPv28_1979_2020m.nc"), # MSWEP v2.8 
                  W5E5   = rast("PP_W5E5_1979_2019m.nc"),     # W5D5 v2.0
                  PMET   = rast("PP_PMETsim_1980_2020_v10m.nc"))     # PMET v1.0 

for (i in 1:length(pp_stacks)) {
  pp_stack <- pp_stacks[[i]]
  terra::time(pp_stack) <- as.POSIXct(time(pp_stack), tz= "UTC") 
  pp_stack <- subset(pp_stack,  which(time(pp_stack)  >= period[1] & time(pp_stack)   <= period[2]))
  pp_obs_s <- subset(pp_obs, Date >= min(time(pp_stack)) &  Date <= max(time(pp_stack)))
  pp_sim   <- as.data.frame(t(terra::extract(pp_stack, pp_shape, method='simple'))[-1,])
  index    <- KGE(sim=pp_sim, obs=pp_obs_s[,-1], method="2012", out.type="full",na.rm=TRUE)
  index    <- data.frame(t(index$KGE.elements), KGE = index$KGE.value)
  colnames(index) <- paste0(names(pp_stacks)[[i]], "_", colnames(index))
  pp_validation   <- cbind(pp_validation, index)
  print(names(pp_stacks)[[i]])
}

write.csv(pp_validation, "PP_Validation.csv", row.names = FALSE)

# 2. Cross validation -----------------------------------------------------------------------------

# Observations (location and data)
pp_validation <- read.csv("/home/rooda/Dropbox/Patagonia/MS1 Results/CV_PP.csv")
pp_shape      <- vect(pp_validation, geom=c("gauge_lon", "gauge_lat"), crs="epsg:4326")
pp_obs        <- read.csv("PP_PMETobs_1950_2020_v10d.csv")
pp_obs        <- zoo(pp_obs[,-1], order.by = as.Date(pp_obs$Date))
pp_obs        <- as.data.frame(MonthlyResample(pp_obs,  20, FUN = sum))
pp_obs        <- cbind(Date = rownames(pp_obs), pp_obs)
pp_obs$Date   <- as.POSIXct(pp_obs$Date, tz= "UTC") # The date is the first column
pp_obs        <- subset(pp_obs, Date >= period[1] &  Date <= period[2])
pp_obs        <- pp_obs[c("Date", pp_shape$gauge_id)]
pp_validation <- subset(pp_validation, select = c(attributes, "fold"))

pp_stacks_cv <- list(PMET_CV1  = rast("/home/rooda/PMET_results/Precipitation/PP_PMETsim_1980_2020m_1.nc"),   
                      PMET_CV2  = rast("/home/rooda/PMET_results/Precipitation/PP_PMETsim_1980_2020m_2.nc"), 
                      PMET_CV3  = rast("/home/rooda/PMET_results/Precipitation/PP_PMETsim_1980_2020m_3.nc"), 
                      PMET_CV4  = rast("/home/rooda/PMET_results/Precipitation/PP_PMETsim_1980_2020m_4.nc"),
                      PMET_CV5  = rast("/home/rooda/PMET_results/Precipitation/PP_PMETsim_1980_2020m_5.nc"),
                      PMET_CV6  = rast("/home/rooda/PMET_results/Precipitation/PP_PMETsim_1980_2020m_6.nc"),
                      PMET_CV7  = rast("/home/rooda/PMET_results/Precipitation/PP_PMETsim_1980_2020m_7.nc"),
                      PMET_CV8  = rast("/home/rooda/PMET_results/Precipitation/PP_PMETsim_1980_2020m_8.nc"),
                      PMET_CV9  = rast("/home/rooda/PMET_results/Precipitation/PP_PMETsim_1980_2020m_9.nc"), 
                      PMET_CV10 = rast("/home/rooda/PMET_results/Precipitation/PP_PMETsim_1980_2020m_10.nc")) 

for (i in 1:length(pp_stacks_cv)) {
  pp_stack <- pp_stacks_cv[[i]]
  terra::time(pp_stack) <- as.POSIXct(time(pp_stack), tz= "UTC") 
  pp_stack <- subset(pp_stack,  which(time(pp_stack)  >= period[1] & time(pp_stack)   <= period[2]))
  pp_obs_s <- subset(pp_obs, Date >= min(time(pp_stack)) &  Date <= max(time(pp_stack)))
  pp_sim   <- as.data.frame(t(terra::extract(pp_stack, pp_shape, method='simple'))[-1,])
  index    <- KGE(sim=pp_sim, obs=pp_obs_s[,-1], method="2012", out.type="full",na.rm=TRUE)
  index    <- data.frame(t(index$KGE.elements), KGE = index$KGE.value)
  colnames(index) <- paste0(names(pp_stacks_cv)[[i]], "_", colnames(index))
  pp_validation   <- cbind(pp_validation, index)
  print(names(pp_stacks_cv)[[i]])
}

for (i in 1:length(pp_stacks_cv)) {
  cv_i <- paste0("CV", i, "_")
  pp_validation[pp_validation$fold != i, grepl(cv_i, colnames(pp_validation))] <- NA
}

pmet_rP <-  as.numeric(rowSums(pp_validation[, grepl("_r",  colnames(pp_validation))], na.rm = T))
pmet_beta <- as.numeric(rowSums(pp_validation[, grepl("_Beta", colnames(pp_validation))], na.rm = T))
pmet_gamma <- as.numeric(rowSums(pp_validation[, grepl("_Gamma", colnames(pp_validation))], na.rm = T))
pmet_kge <- as.numeric(rowSums(pp_validation[, grepl("_KGE", colnames(pp_validation))], na.rm = T))
pmet_rP[pmet_rP == 0] <- NA
pmet_beta[pmet_beta == 0] <- NA
pmet_gamma[pmet_gamma == 0] <- NA
pmet_kge[pmet_kge == 0] <- NA

pp_validation <- subset(pp_validation, select = attributes)
pp_validation$r <- pmet_rP
pp_validation$Beta <- pmet_beta
pp_validation$Gamma <- pmet_gamma
pp_validation$KGE <- pmet_kge
write.csv(pp_validation, "PP_Validation_CV.csv",  row.names = FALSE)

