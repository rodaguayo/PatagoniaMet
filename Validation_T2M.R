# Code to replicate temperature validation 
# Developed by Rodrigo Aguayo (2020-2023)

rm(list=ls())
cat("\014")  

library("hydroGOF")
library("terra")

setwd("/home/rooda/Dropbox/Patagonia/Data/Temperature/")
period     <- c(as.POSIXct("1980-01-01"), as.POSIXct("2020-12-31"))
attributes <- c("gauge_id", "gauge_name","institution", "gauge_lat", "gauge_lon", "gauge_alt")
dem_lr     <- rast("/home/rooda/Dropbox/Patagonia/GIS South/dem_patagonia005.tif")

# Observations (location and data)
t2m_validation  <- read.csv("Tavg_PMETobs_v10_metadata.csv")
t2m_shape <- vect(t2m_validation, geom=c("gauge_lon", "gauge_lat"), crs="epsg:4326")
t2m_shape$Altitude_lr <- extract(dem_lr, t2m_shape, method='simple')[,-1]

t2m_obs      <- read.csv("Tavg_PMETobs_v10m.csv")
Date <- as.POSIXct(t2m_obs$Date, tz= "UTC") #The date is the first column
t2m_obs      <- sweep(t2m_obs[,-1], 2, (t2m_shape$Altitude_lr - t2m_shape$gauge_alt)*0.0065)
t2m_obs      <- cbind(Date, t2m_obs)
t2m_validation <- subset(t2m_validation, select = attributes)

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

for (i in 1:length(t2m_stacks)) {
  
  t2m_stack <- t2m_stacks[[i]]
  time(t2m_stack) <- as.POSIXct(time(t2m_stack), tz= "UTC") 
  t2m_stack <- subset(t2m_stack,  which(time(t2m_stack)  >= period[1] & time(t2m_stack)   <= period[2]))
  t2m_obs_s <- subset(t2m_obs, Date >= min(time(t2m_stack)) &  Date <= max(time(t2m_stack)))
  t2m_sim   <- as.data.frame(t(extract(t2m_stack, t2m_shape, method='simple'))[-1,])
  index     <- KGE(sim=t2m_sim, obs=t2m_obs_s[,-1], method="2009", out.type="full",na.rm=TRUE)
  index     <- index$KGE.elements[3,] # just use rSD
  index     <- data.frame(ME = me(sim=t2m_sim, obs=t2m_obs_s[,-1], na.rm=TRUE), rSD = index)
  colnames(index) <- paste0(names(t2m_stacks)[[i]], "_", colnames(index))
  t2m_validation  <- cbind(t2m_validation, index)
  print(names(t2m_stacks)[[i]])
}

write.csv(t2m_validation, "Tavg_Validation.csv",  row.names = FALSE)
