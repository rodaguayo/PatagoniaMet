# Code to downscale reanalysis data
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("terra")

#Sample dataset: extent, crs and res
sample <- rast(extent = ext(c(-76,-65,-57,-40)), crs = "+init=epsg:4326", resolution = 0.05)

#Gridded dataset to downscale
setwd("/home/rooda/Dropbox/Patagonia/")

pp_stack  <- rast("Data/Precipitation/PP_ERA5_1950_2021m.nc")
t2m_stack <- rast("Data/Temperature/T2M_ERA5_1950_2021m.nc")
pp_stack  <- subset(pp_stack, which(time(pp_stack) >= as.POSIXct("1989-12-31")    &   time(pp_stack)  <= as.POSIXct("2019-12-31")))
t2m_stack <- subset(t2m_stack,which(time(t2m_stack) >= as.POSIXct("1989-12-31")   &   time(t2m_stack) <= as.POSIXct("2019-12-31")))

#Precipitation downscaling: bilinear resampling
pp_stack_hr   <- resample(pp_stack, sample, method = "bilinear") # should be something better 
pp_stack_hr_m <- mean(tapp(pp_stack_hr, strftime(time(pp_stack_hr),format="%Y"), fun = sum, na.rm = TRUE)) # mean annual value

writeCDF(pp_stack_hr, "Data/Precipitation/PP_ERA5_hr_1990_2019m.nc",  overwrite=TRUE, varname="pp", unit="mm", longname="Precipitation", zname="time", compression = 9)
writeRaster(pp_stack_hr_m, "Data/Precipitation/PP_ERA5_hr_1990_2019m.tif", overwrite=TRUE)

#Temperature downscaling: lapse rate = 6.5 degC km-1
dem_hr       <- rast("GIS South/dem_patagonia3f.tif")
dem_hr <- resample(dem_hr,    sample,    method="bilinear")
dem_lr <- resample(dem_hr,    t2m_stack, method="bilinear")
dem_lr <- resample(dem_lr,    dem_hr,    method="bilinear")
factor <- (dem_lr-dem_hr)*0.0065
factor[is.na(factor)] <- 0

t2m_stack_hr <-resample(t2m_stack, dem_hr, method="near")
t2m_stack_hr <-t2m_stack_hr+factor
t2m_stack_hr <-round(t2m_stack_hr, 2)

t2m_stack_hr_m <- mean(tapp(t2m_stack_hr, strftime(time(t2m_stack_hr),format="%Y"), fun = mean, na.rm = TRUE)) # mean annual value

writeCDF(t2m_stack_hr, "Data/Temperature/T2M_ERA5_hr_1990_2019m.nc",  overwrite=TRUE, varname="t2m", unit="degC", longname="Temperature", zname="time", compression = 9)
writeRaster(t2m_stack_hr_m, "Data/Temperature/T2M_ERA5_hr_1990_2019m.tif", overwrite=TRUE)

#Alternative for temperature: best lapse rate according to observations
t2m_shape <- read.csv("Data/Temperature/Metadata_Temperature_v10.csv")
t2m_shape <- vect(t2m_shape, geom=c("Longitude", "Latitude"), crs="epsg:4326")
t2m_obs   <- read.csv("Data/Temperature/Data_Temperature_mean_v10_monthly.csv")
t2m_obs$Date<-as.Date(t2m_obs$Date) #The date is the first column

best_lapse_rate <- function(lr) {
  
  t2m_stack_hr<-resample(t2m_stack, dem_hr, method="near")
  factor      <-(dem_lr-dem_hr)*lr
  t2m_stack_hr<-t2m_stack_hr+factor
  
  t2m_sim_hr<-as.data.frame(t(extract(t2m_stack_hr,t2m_shape, method='simple')))
  best_lapse_rate<-sum((hydroGOF::me(sim=t2m_sim_hr, obs=t2m_obs, na.rm=TRUE))^2)
  
}

best_lr<-optim(0.0065, best_lapse_rate)

