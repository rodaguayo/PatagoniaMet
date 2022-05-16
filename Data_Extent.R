# Code to reprocessing reanalysis datasets used in Aguayo et al. (in review)
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("terra")

# Initialization of area and folder
setwd("/home/rooda/Dropbox/Patagonia/Data/")
cut<-ext(-79,-64,-57,-40)

# ERA5: Precipitation and air temperature (daily and monthly)
stack_pp  <- rast(c("/home/rooda/Documents/PP_ERA5_1950_1978m.nc","/home/rooda/Documents/PP_ERA5_1979_2021m.nc"))
stack_t2m <- rast(c("/home/rooda/Documents/T2M_ERA5_1950_1978m.nc","/home/rooda/Documents/T2M_ERA5_1979_2021m.nc"))
stack_pp  <- round(crop(stack_pp, cut)*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)*1000,0)
stack_t2m <- round(crop(stack_t2m, cut)-273.15, 2)
writeCDF(stack_pp, "Precipitation/PP_ERA5_1950_2021m.nc", overwrite=TRUE, varname="pp", unit="mm", longname="Precipitation", zname="time", compression = 9)
writeCDF(stack_t2m, "Temperature/T2M_ERA5_1950_2021m.nc", overwrite=TRUE, varname="t2m", unit="degC", longname="Temperature", zname="time", compression = 9)

stack_pp      <- rast(c("/home/rooda/Documents/PP_ERA5_1950_1978d.nc","/home/rooda/Documents/PP_ERA5_1979_2021d.nc"))*1000
stack_t2m_max <- rast(c("/home/rooda/Documents/T2M_ERA5_1950_1978d.nc","/home/rooda/Documents/T2M_ERA5_1979_2021d.nc"))-273.15
stack_t2m_min <- rast(c("/home/rooda/Documents/T2M_ERA5_1950_1978d.nc","/home/rooda/Documents/T2M_ERA5_1979_2021d.nc"))-273.15
stack_t2m_max <- tapp(stack_t2m_max, strftime(time(stack_t2m_max),format="%Y-%m-%d"), fun = max)
stack_t2m_min <- tapp(stack_t2m_min, strftime(time(stack_t2m_min),format="%Y-%m-%d"), fun = min)
stack_pp      <- tapp(stack_pp, strftime(time(stack_pp),format="%Y-%m-%d"), fun = mean)
stack_pp      <- round(crop(stack_pp, cut) ,0)
stack_t2m_max <- round(crop(stack_t2m, cut),2)
stack_t2m_min <- round(crop(stack_t2m, cut),2)
writeCDF(stack_pp,      "Precipitation/PP_ERA5_1950_2021d.nc", overwrite=TRUE, varname="pp", unit="mm", longname="Precipitation", zname="time", compression = 9)
writeCDF(stack_t2m_max, "Temperature/T2M_max_ERA5_1950_2021d.nc", overwrite=TRUE, varname="t2m_max", unit="degC", longname="Maximum temperature", zname="time", compression = 9)
writeCDF(stack_t2m_min, "Temperature/T2M_min_ERA5_1950_2021d.nc", overwrite=TRUE, varname="t2m_min", unit="degC", longname="Minimum temperature", zname="time", compression = 9)

# ERA5-land (ERA5L): Precipitation and air temperature (daily and monthly)
stack_pp  <- rast("/home/rooda/Documents/PP_ERA5L_1950_2021m.nc")
stack_t2m <- rast("/home/rooda/Documents/T2M_ERA5L_1950_2021m.nc")
stack_pp  <- round(crop(stack_pp, cut)*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)*1000,0)
stack_t2m <- round(crop(stack_t2m, cut)-273.15,2)
writeCDF(stack_pp, "Precipitation/PP_ERA5L_1950_2021m.nc", prec='integer', overwrite=TRUE, varname="pp", unit="mm", longname="Precipitation", zname="time", compression = 9)
writeCDF(stack_t2m, "Temperature/T2M_ERA5L_1950_2021m.nc", overwrite=TRUE, varname="t2m", unit="degC", longname="Temperature", zname="time", compression = 9)

stack_pp      <- rast(c("/home/rooda/Documents/PP_ERA5l_1950_1978d.nc","/home/rooda/Documents/PP_ERA5l_1979_2021d.nc"))*1000
stack_t2m_max <- rast(c("/home/rooda/Documents/T2M_ERA5l_1950_1978d.nc","/home/rooda/Documents/T2M_ERA5l_1979_2021d.nc"))-273.15
stack_t2m_min <- rast(c("/home/rooda/Documents/T2M_ERA5l_1950_1978d.nc","/home/rooda/Documents/T2M_ERA5l_1979_2021d.nc"))-273.15
stack_t2m_max <- tapp(stack_t2m_max, strftime(time(stack_t2m_max),format="%Y-%m-%d"), fun = max)
stack_t2m_min <- tapp(stack_t2m_min, strftime(time(stack_t2m_min),format="%Y-%m-%d"), fun = min)
stack_pp      <- tapp(stack_pp, strftime(time(stack_pp),format="%Y-%m-%d"), fun = mean)
stack_pp      <- round(crop(stack_pp, cut) ,0)
stack_t2m_max <- round(crop(stack_t2m, cut),2)
stack_t2m_min <- round(crop(stack_t2m, cut),2)
writeCDF(stack_pp,      "Precipitation/PP_ERA5l_1950_2021d.nc", overwrite=TRUE, varname="pp", unit="mm", longname="Precipitation", zname="time", compression = 9)
writeCDF(stack_t2m_max, "Temperature/T2M_max_ERA5l_1950_2021d.nc", overwrite=TRUE, varname="t2m_max", unit="degC", longname="Maximum temperature", zname="time", compression = 9)
writeCDF(stack_t2m_min, "Temperature/T2M_min_ERA5l_1950_2021d.nc", overwrite=TRUE, varname="t2m_min", unit="degC", longname="Minimum temperature", zname="time", compression = 9)

# MERRA2: Precipitation and air temperature (monthly)
stack_pp  <- rast(list.files("C:/Users/rooda/Downloads/MERRA2/PP/", full.names = TRUE))
stack_t2m <- rast(list.files("C:/Users/rooda/Downloads/MERRA2/T2M/", full.names = TRUE))
stack_pp  <- round(crop(stack_pp, cut)*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)*86400, 0)
stack_t2m <- round(crop(stack_t2m, cut)-273.15, 2)
stack_pp  <- setZ(stack_pp,  seq(as.POSIXct('1980-01-01'), as.POSIXct('2019-12-31'), by = 'month'))
stack_t2m <- setZ(stack_t2m, seq(as.POSIXct('1980-01-01'), as.POSIXct('2019-12-31'), by = 'month'))
writeCDF(stack_pp,  "Precipitation/PP_MERRA2_1980_2019m.nc", overwrite=TRUE, varname="pp", unit="mm", longname="Precipitation", zname="time", compression = 9)
writeCDF(stack_t2m, "Temperature/T2M_MERRA2_1980_2019m.nc", overwrite=TRUE, varname="t2m", unit="degC", longname="Temperature", zname="time", compression = 9)

# CSFR: Precipitation and air temperature (monthly)
stack_pp  <- rast(c(list.files("C:/Users/rooda/Downloads/CSFR/pp/1979-2010/", full.names = TRUE)), list.files("C:/Users/rooda/Downloads/CSFR/pp/2011-2019/", full.names = TRUE))
stack_t2m <- rast(c(list.files("C:/Users/rooda/Downloads/CSFR/t2m/1979-2010/", full.names = TRUE)), list.files("C:/Users/rooda/Downloads/CSFR/pp/2011-2019/", full.names = TRUE))
stack_pp  <- rotate(stack_pp)
stack_t2m <- rotate(stack_t2m)
stack_pp  <- round(crop(stack_pp, cut)*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)*4, 2)
stack_t2m <- round(crop(stack_t2m, cut)-273.15, 2)
stack_pp  <- setZ(stack_pp,  seq(as.POSIXct('1979-01-01'), as.POSIXct('2019-12-31'), by = 'month'))
stack_t2m <- setZ(stack_t2m, seq(as.POSIXct('1979-01-01'), as.POSIXct('2019-12-31'), by = 'month'))
writeCDF(stack_pp,  "Precipitation/PP_MERRA2_1979_2019m.nc", overwrite=TRUE, varname="pp", unit="mm", longname="Precipitation", zname="time", compression = 9)
writeCDF(stack_t2m, "Temperature/T2M_MERRA2_1979_2019m.nc",  overwrite=TRUE, varname="t2m", unit="degC", longname="Temperature", zname="time", compression = 9)

# MSWEP v2.8: (monthly)
stack_pp        <- rast(list.files("C:/Users/rooda/Downloads/MSWEP/", full.names = TRUE))
stack_pp        <- crop(stack_pp, cut)
time(stack_pp)  <- seq(as.POSIXct('1979-02-01'), as.POSIXct('2020-11-01'), by = 'month')
writeCDF(stack_pp, "Precipitation/PP_MSWEPv28_1979_2021m.nc", prec='integer', overwrite=TRUE, varname="pp", unit="mm", longname="Precipitation", zname="time", compression = 9)

#CR2MET v2.0 (daily and monthly)
stack_pp      <- rast("/home/rooda/Documents/PP_CR2METv2_1979_2020d.nc")
stack_t2m_max <- rast("/home/rooda/Documents/T2M_max_CR2METv2_1979_2020d.nc")
stack_t2m_min <- rast("/home/rooda/Documents/T2M_min_CR2METv2_1979_2020d.nc")
stack_pp      <- crop(stack_pp, cut)
stack_t2m_max <- crop(stack_t2m_max, cut)
stack_t2m_min <- crop(stack_t2m_min, cut)
writeCDF(stack_pp,      "Precipitation/PP_CR2MET_1979_2020d.nc",    overwrite=TRUE, varname="pp", unit="mm", longname="Precipitation", zname="time", compression = 9)
writeCDF(stack_t2m_max, "Temperature/T2M_max_CR2MET_1979_2020d.nc", overwrite=TRUE, varname="t2m_max", unit="degC", longname="Maximum temperature", zname="time", compression = 9)
writeCDF(stack_t2m_min, "Temperature/T2M_min_CR2MET_1979_2020d.nc", overwrite=TRUE, varname="t2m_min", unit="degC", longname="Minimum temperature", zname="time", compression = 9)

stack_t2m_max <- tapp(stack_t2m_max, strftime(time(stack_t2m_max),format="%Y-%m"), fun = mean)
stack_t2m_min <- tapp(stack_t2m_min, strftime(time(stack_t2m_min),format="%Y-%m"), fun = mean)
stack_pp      <- tapp(stack_t2m_min, strftime(time(stack_t2m_min),format="%Y-%m"), fun = sum)
time(stack_t2m_max)  <- seq(as.POSIXct('1979-01-01'), as.POSIXct('2020-12-01'), by = 'month')
time(stack_t2m_min)  <- seq(as.POSIXct('1979-01-01'), as.POSIXct('2020-12-01'), by = 'month')
time(stack_pp)       <- seq(as.POSIXct('1979-01-01'), as.POSIXct('2020-12-01'), by = 'month')
writeCDF(stack_pp,      "Precipitation/PP_CR2MET_1979_2020m.nc",    overwrite=TRUE, varname="pp", unit="mm", longname="Precipitation", zname="time", compression = 9)
writeCDF(stack_t2m_max, "Temperature/T2M_max_CR2MET_1979_2020m.nc", overwrite=TRUE, varname="t2m_max", unit="degC", longname="Maximum temperature", zname="time", compression = 9)
writeCDF(stack_t2m_min, "Temperature/T2M_min_CR2MET_1979_2020m.nc", overwrite=TRUE, varname="t2m_min", unit="degC", longname="Minimum temperature", zname="time", compression = 9)

# RegCM4-CR2 (monthly)
stack_pp  <- rast(list.files("C:/Users/rooda/Desktop/pr/", full.names = TRUE))
stack_t2m <- rast(list.files("C:/Users/rooda/Desktop/tas/", full.names = TRUE))
stack_pp  <- crop(stack_pp, cut)*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)*86400
stack_t2m <- crop(stack_t2m, cut)-273.15
stack_pp  <- setZ(stack_pp,  seq(as.POSIXct('1980-01-01'), as.POSIXct('2015-12-31'), by = 'month'))
stack_t2m <- setZ(stack_t2m, seq(as.POSIXct('1980-01-01'), as.POSIXct('2015-12-31'), by = 'month'))
writeCDF(stack_pp,  "Precipitation/PP_MERRA2_1980_2015m.nc", overwrite=TRUE, varname="pp",  unit="mm", longname="Precipitation", zname="time", compression = 9)
writeCDF(stack_t2m, "Temperature/T2M_MERRA2_1980_2015m.nc",  overwrite=TRUE, varname="t2m", unit="degC", longname="Temperature", zname="time", compression = 9)

#Patagonia: PET-ET Gleam
pet_stack<-rast("/home/rooda/Documents/PET_GLEAM35a_1980_2020m.nc")
pet_stack<-t(pet_stack)
ext(pet_stack)<-c(-180, 180, -90, 90)
pet_stack<-flip(pet_stack, direction = "h")
crs(pet_stack)<-"+init=epsg:4326"
pet_stack<-round(crop(pet_stack, cut), 0)

pet_stack<-focal(pet_stack, w=3, fun=mean, NAonly=T, na.rm=T) # Fill NAs
time(pet_stack)<-seq(as.POSIXct("1980-01-01"), as.POSIXct("2020-12-31"), "month")
pet_stack[pet_stack == 0] <- NA

writeCDF(stack_pet, "Evapotranspiration/PET_GLEAM35a_1980_2020m.nc",  overwrite=TRUE, varname="pet", unit="mm", longname="Potenntial evapotranspiration", zname="time", compression = 9)



#BH-DGA Stage III and IV
nc_bh3<-nc_open("E:/Datasets/DGA_BH/BH3/4_Base_de_datos/Archivos_netcdf/1_Historico/regionalizacion_1979_2015.nc")
lon3 <- ncvar_get(nc_bh3,"lon")
lat3 <- ncvar_get(nc_bh3,"lat")

stack_pp3<-stack("E:/Datasets/DGA_BH/BH3/4_Base_de_datos/Archivos_netcdf/1_Historico/regionalizacion_1979_2015.nc", varname = "pr")
stack_pp3<-flip(t(stack_pp3), 'x')
extent(stack_pp3) <- c(min(lon3), max(lon3), min(lat3), max(lat3))
crs(stack_pp3) <- CRS('+init=EPSG:4326')
stack_pp3<-setZ(stack_pp3,seq(as.Date("1979/1/1"), as.Date("2015/12/1"), "month"))
stack_pp3 <- stack_pp3[[which(getZ(stack_pp3) >= as.Date("1984-12-31"))]]
stack_pp3<-mean(stackApply(stack_pp3, indices<-format(stack_pp3@z$time,"%y"), fun=sum))
stack_pp3[stack_pp3 == 0] <- NA

stack_et3<-stack("E:/Datasets/DGA_BH/BH3/4_Base_de_datos/Archivos_netcdf/1_Historico/regionalizacion_1979_2015.nc", varname = "ET")
stack_et3<-flip(t(stack_et3), 'x')
extent(stack_et3) <- c(min(lon3), max(lon3), min(lat3), max(lat3))
crs(stack_et3) <- CRS('+init=EPSG:4326')
stack_et3 <- stack_et3*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
stack_et3<-setZ(stack_et3,seq(as.Date("1979/1/1"), as.Date("2015/12/1"), "month"))
stack_et3 <- stack_et3[[which(getZ(stack_et3) >= as.Date("1984-12-31"))]]
stack_et3<-mean(stackApply(stack_et3, indices<-format(stack_et3@z$time,"%y"), fun=sum))
stack_et3[stack_et3 == 0] <- NA

stack_pet3<-stack("E:/Datasets/DGA_BH/BH3/4_Base_de_datos/Archivos_netcdf/1_Historico/regionalizacion_1979_2015.nc", varname = "PET")
stack_pet3<-flip(t(stack_pet3), 'x')
extent(stack_pet3) <- c(min(lon3), max(lon3), min(lat3), max(lat3))
crs(stack_pet3) <- CRS('+init=EPSG:4326')
stack_pet3 <- stack_pet3*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
stack_pet3<-setZ(stack_pet3,seq(as.Date("1979/1/1"), as.Date("2015/12/1"), "month"))
stack_pet3 <- stack_pet3[[which(getZ(stack_pet3) >= as.Date("1984-12-31"))]]
stack_pet3<-mean(stackApply(stack_pet3, indices<-format(stack_pet3@z$time,"%y"), fun=sum))
stack_pet3[stack_pet3 == 0] <- NA

basins_pp<-extract(stack_pp3,basins,fun=mean,na.rm=TRUE)
basins_pet<-extract(stack_pet3,basins,fun=mean,na.rm=TRUE)
basins_et<-extract(stack_et3,basins,fun=mean,na.rm=TRUE)
rownames(basins_pp)<-basins$gridcode
rownames(basins_pet)<-basins$gridcode
rownames(basins_et)<-basins$gridcode

write.csv(cbind(basins$gridcode,basins_pp, basins_pet, basins_et), "eva_aridity_bh3.csv")

stack_pp4<- raster("E:/Datasets/DGA_BH/BH4/4_Base_de_datos/Archivos_raster/BH_85-15/Forzantes/1_Historico/pr_Anual_LatLon.tif")
stack_pet4<-raster("E:/Datasets/DGA_BH/BH4/4_Base_de_datos/Archivos_raster/BH_85-15/VIC/1_Historico/pet_Anual_LatLon.tif")
stack_et4<-raster("E:/Datasets/DGA_BH/BH4/4_Base_de_datos/Archivos_raster/BH_85-15/VIC/1_Historico/et_Anual_LatLon.tif")

basins_pp<-extract(stack_pp4,basins,fun=mean,na.rm=TRUE)
basins_pet<-extract(stack_pet4,basins,fun=mean,na.rm=TRUE)
basins_et<-extract(stack_et4,basins,fun=mean,na.rm=TRUE)
rownames(basins_pp)<-basins$gridcode
rownames(basins_pet)<-basins$gridcode
rownames(basins_et)<-basins$gridcode

write.csv(cbind(basins$gridcode,basins_pp, basins_pet, basins_et), "eva_aridity_bh4.csv")