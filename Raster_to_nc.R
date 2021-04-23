rm(list=ls())
cat("\014")  

library("raster")
library("ncdf4")


#Precipitation
stack_pp<-stack(list.files("C:/Users/Rodrigo/Dropbox/Puelo/Datos/Precipitacion/Datos Grillados PP/procesados", pattern = "tif$", full.names = TRUE))
stack_pp <- setZ(stack_pp, seq(as.Date('1997-01-01'), as.Date('2017-12-31'), by = 'month'))
writeRaster(stack_pp, "PP_1997_2017", format = "CDF",overwrite=TRUE, varname="Precipitation", varunit="mm", xname="Longitude",   yname="Latitude", zname="Time (Month)")
stack_ppa<-stack(list.files("C:/Users/Rodrigo/Dropbox/Puelo/Datos/Precipitacion/Datos Grillados PP/Anomalias", pattern = "tif$", full.names = TRUE))
stack_ppa <- setZ(stack_ppa, seq(as.Date('1997-01-01'), as.Date('2017-12-31'), by = 'month'))
writeRaster(stack_ppa, "PP_Anom_1997_2017", format = "CDF",overwrite=TRUE, varname="Precipitation Anomaly", varunit="mm", xname="Longitude",   yname="Latitude", zname="Time (Month)")

#Temperature
stack_tx<-stack(list.files("C:/Users/Rodrigo/Dropbox/Puelo/Datos/Temperatura/Datos Grillados TEMP/Procesados", pattern = "tif$", full.names = TRUE))
stack_tx <- setZ(stack_tx, seq(as.Date('2001-01-01'), as.Date('2017-12-31'), by = 'month'))
writeRaster(stack_tx, "Temp_2001_2017", format = "CDF",overwrite=TRUE, varname="Temperature", varunit="degC", xname="Longitude",   yname="Latitude", zname="Time (Month)")
stack_tx<-stack(list.files("C:/Users/Rodrigo/Dropbox/Puelo/Datos/Temperatura/Datos Grillados TEMP/Anomalias_procesados", pattern = "tif$", full.names = TRUE))
stack_tx <- setZ(stack_tx, seq(as.Date('2001-01-01'), as.Date('2017-12-31'), by = 'month'))
writeRaster(stack_tx, "Temp_Anom_2001_2017", format = "CDF",overwrite=TRUE, varname="Temperature", varunit="degC", xname="Longitude",   yname="Latitude", zname="Time (Month)")

#Temperature update 2.0
stack_tx_day<-stack(list.files("C:/Users/Rodrigo/Desktop/lst1/Surf_Temp_Monthly_005dg_v6/LST_Day_CMG", pattern = "tif$", full.names = TRUE))
stack_tx_night<-stack(list.files("C:/Users/Rodrigo/Desktop/lst1/Surf_Temp_Monthly_005dg_v6/LST_Night_CMG", pattern = "tif$", full.names = TRUE))
stack_tx_day <-(stack_tx_day*0.02)-273.15
stack_tx_night <-(stack_tx_night*0.02)-273.15
stack_tx_day <- setZ(stack_tx_day, seq(as.Date('2000-03-01'), as.Date('2019-07-31'), by = 'month'))
stack_tx_night <- setZ(stack_tx_night, seq(as.Date('2000-03-01'), as.Date('2019-07-31'), by = 'month'))
writeRaster(stack_tx_day, "Temp_MOD11C3_day_2000_2019", format = "CDF",overwrite=TRUE, varname="Temperature", varunit="degC", xname="Longitude",   yname="Latitude", zname="Time (Month)")
writeRaster(stack_tx_night, "Temp_MOD11C3_night_2000_2019", format = "CDF",overwrite=TRUE, varname="Temperature", varunit="degC", xname="Longitude",   yname="Latitude", zname="Time (Month)")

#Clouds
stack_tx<-stack(list.files("C:/Users/Rodrigo/Desktop/CAPAS", pattern = "TIFF$", full.names = TRUE))
area<-raster("C:/Users/Rodrigo/Dropbox/Puelo/SIG/Puelo/Dem/dem_005_puelo2.tif")
stack_cut<-stack()

for(i in 1:232) {
  stack_cut<-stack(stack_cut,crop(stack_tx[[i]], area))
  print(i)
 }

stack_cut <- stack_cut/255
stack_cut <- setZ(stack_cut, seq(as.Date('2000-02-01'), as.Date('2019-05-31'), by = 'month'))
writeRaster(stack_cut, "Clouds", format = "CDF",overwrite=TRUE, varname="CLOUD", varunit="cover", xname="Longitude",   yname="Latitude", zname="Time (Month)")

#How to read date from NETcdf
any_stack<-stack("C:/Users/Rodrigo/Documents/R/hola344.nc")
any_stack<-setZ(any_stack, as.Date(as.numeric(substring(names(any_stack), 2)), origin = "1970-01-01"))