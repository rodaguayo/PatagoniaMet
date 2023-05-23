# Code to replicate precipitation validation 
# Developed by Rodrigo Aguayo (2020-2023)

rm(list=ls())
cat("\014")  

library("hydroGOF")
library("terra")

setwd("/home/rooda/Dropbox/Patagonia/Data/Precipitation/")
period     <- c(as.POSIXct("1980-01-01"), as.POSIXct("2020-12-31"))
attributes <- c("gauge_id", "gauge_name","institution", "gauge_lat", "gauge_lon", "gauge_alt")

# Observations (location and data)
pp_validation <- read.csv("PP_PMETobs_v10_metadata.csv")
pp_shape      <- vect(pp_validation, geom=c("gauge_lon", "gauge_lat"), crs="epsg:4326")
pp_obs        <- read.csv("PP_PMETobs_v10m.csv")
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
                  PMET   = rast("PP_PMET_1980_2020m.nc"))     # PMET v1.0 

for (i in 1:length(pp_stacks)) {
  pp_stack <- pp_stacks[[i]]
  time(pp_stack) <- as.POSIXct(time(pp_stack), tz= "UTC") 
  pp_stack <- subset(pp_stack,  which(time(pp_stack)  >= period[1] & time(pp_stack)   <= period[2]))
  pp_obs_s <- subset(pp_obs, Date >= min(time(pp_stack)) &  Date <= max(time(pp_stack)))
  pp_sim   <- as.data.frame(t(extract(pp_stack, pp_shape, method='simple'))[-1,])
  index    <- KGE(sim=pp_sim, obs=pp_obs_s[,-1], method="2012", out.type="full",na.rm=TRUE)
  index    <- data.frame(t(index$KGE.elements), KGE = index$KGE.value)
  colnames(index) <- paste0(names(pp_stacks)[[i]], "_", colnames(index))
  pp_validation   <- cbind(pp_validation, index)
  print(names(pp_stacks)[[i]])
}

write.csv(pp_validation, "PP_Validation.csv", row.names = FALSE)
