# Code to downscale ERA5 data ---------------------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2023)

rm(list=ls())
cat("\014")  

library("terra")
terraOptions(memfrac=0.90)
terraOptions(verbose=T)

#Sample dataset: extent, crs and res
sample <- rast(extent = ext(c(-75.75, -68.25, -55.75, -40.5)), crs = "+init=epsg:4326", resolution = 0.05)
setwd("/home/rooda/Dropbox/Patagonia/")
dem    <- rast("GIS South/dem_patagonia005.tif")
period <- c(as.Date("1980-01-01"), as.Date("2020-12-31"))

#Precipitation downscaling: bilinear resampling
pp_stack <- rast("Data/Precipitation/PP_ERA5_1959_2021d.nc")
pp_stack <- subset(pp_stack,  which(time(pp_stack)  >= period[1] & time(pp_stack)   <= period[2]))
pp_stack <- resample(pp_stack, sample, method = "bilinear") # should be something better
pp_stack <- pp_stack * is.finite(dem)
pp_stack <- round(pp_stack, 0)

pp_stack_m <- tapp(pp_stack,   strftime(time(pp_stack), format="%Y-%m"), fun = sum, na.rm = TRUE)
terra::time(pp_stack_m) <- seq(from = period[1], to = period[2], by = "month")
pp_stack_y <- tapp(pp_stack_m, strftime(time(pp_stack_m), format="%Y"),  fun = sum, na.rm = TRUE)
pp_stack_y <- mean(pp_stack_y)

writeCDF(pp_stack, "Data/Precipitation/PP_ERA5_hr_1980_2020d.nc",  overwrite=TRUE, 
         varname="pp", unit="mm", zname="time", prec = "float",  shuffle = T)

writeCDF(pp_stack_m, "Data/Precipitation/PP_ERA5_hr_1980_2020m.nc",  overwrite=TRUE, 
         varname="pp", unit="mm", zname="time", compression = 9, prec = "float")

writeRaster(pp_stack_y, "Data/Precipitation/PP_ERA5_hr_1980_2020.tif", overwrite=TRUE)

# Maximum temperature downscaling: lapse rate = 6.5 degC km-1
tmax_stack <- rast("Data/Temperature/Tmax_ERA5_1959_2021d.nc")
tmax_stack <- subset(tmax_stack, which(time(tmax_stack) >= period[1] & time(tmax_stack)  <= period[2]))

dem_hr <- rast("GIS South/dem_patagonia3f.tif")
dem_hr <- resample(dem_hr,    sample,    method="bilinear")
dem_lr <- resample(dem_hr,    tmax_stack, method="bilinear")
dem_lr <- resample(dem_lr,    dem_hr,    method="near")
factor <- round((dem_lr-dem_hr)*0.0065,2)
factor[is.na(factor)] <- 0

tmax_stack <- resample(tmax_stack, dem_hr, method="near")
tmax_stack <- tmax_stack * is.finite(dem)
tmax_stack <- tmax_stack + factor

tmax_stack_m <- tapp(tmax_stack, strftime(time(tmax_stack),format="%Y"), fun = mean, na.rm = TRUE)
tmax_stack_m <- mean(tmax_stack_m) # mean annual value
writeRaster(tmax_stack_m, "Data/Temperature/Tmax_ERA5_hr_1980_2020.tif", overwrite=TRUE)

writeCDF(tmax_stack, "Data/Temperature/Tmax_ERA5_hr_1980_2020d.nc",  overwrite=TRUE, 
         varname="tmax", unit="degC", longname="Temperature", zname="time", prec = "float", shuffle = T)

# Minimum temperature downscaling: lapse rate = 6.5 degC km-1
tmin_stack <- rast("Data/Temperature/Tmin_ERA5_1959_2021d.nc")
tmin_stack <- subset(tmin_stack, which(time(tmin_stack) >= period[1] & time(tmin_stack)  <= period[2]))

dem_hr <- rast("GIS South/dem_patagonia3f.tif")
dem_hr <- resample(dem_hr,    sample,    method="bilinear")
dem_lr <- resample(dem_hr,    tmin_stack, method="bilinear")
dem_lr <- resample(dem_lr,    dem_hr,    method="near")
factor <- round((dem_lr-dem_hr)*0.0065,2)
factor[is.na(factor)] <- 0

tmin_stack <- resample(tmin_stack, dem_hr, method="near")
tmin_stack <- tmin_stack * is.finite(dem)
tmin_stack <- tmin_stack + factor

tmin_stack_m <- tapp(tmin_stack, strftime(time(tmin_stack),format="%Y"), fun = mean, na.rm = TRUE)
tmin_stack_m <- mean(tmin_stack_m) # mean annual value
writeRaster(tmin_stack_m, "Data/Temperature/Tmin_ERA5_hr_1980_2020.tif", overwrite=TRUE)

writeCDF(tmin_stack, "Data/Temperature/Tmin_ERA5_hr_1980_2020d.nc",  overwrite=TRUE, 
         varname="tmin", unit="degC", longname="Temperature", zname="time", prec = "float", shuffle = T)
