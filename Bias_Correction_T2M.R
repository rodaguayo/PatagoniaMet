rm(list=ls())
cat("\014")  

library("raster")
library("zoo")
library("sf")
library("readxl")
library("hydroGOF")
library("SPEI")
library("doParallel")

#T2M Bias correction: Variance and mean scaling: First stage
t2m_era5_v1<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_ERA5_1990_2019_v1.nc", varname = "tas")

delta_value<-raster("C:/Users/rooda/Dropbox/Rstudio/mean_value.tif")
variance_value<-raster("C:/Users/rooda/Dropbox/Rstudio/variance_value.tif")

variance_value[variance_value == 0 ] <- NA
variance_value<-trim(variance_value)
t2m_era5_v1<-resample(t2m_era5_v1, variance_value)
delta_value<-resample(delta_value, variance_value)

t2m_era5_v1.5<-t2m_era5_v1 - delta_value
t2m_era5_v2<-t2m_era5_v1.5-mean(t2m_era5_v1.5)
t2m_era5_v2<-t2m_era5_v2*variance_value
t2m_era5_v2<-t2m_era5_v2 + mean(t2m_era5_v1.5)

t2m_era5_v2<-setZ(t2m_era5_v2,seq(as.Date("1990/1/1"), as.Date("2019/12/1"), "month"))
t2m_era5_v2_mean<-mean(stackApply(t2m_era5_v2, indices<-format(t2m_era5_v2@z$time,"%y"), fun=mean))

writeRaster(t2m_era5_v2, "T2M_ERA5_1990_2019_v2.nc", format = "CDF", overwrite=TRUE, varname="tas", varunit="degC", longname="temperature", xname="X", yname="Y", zname="time", zunit="month")
writeRaster(t2m_era5_v2_mean, "T2M_ERA5_1990_2019_v2_mean.tif", format = "GTiff", overwrite=TRUE)

#Validation: Use v1 or v2
t2m_shape<-shapefile("C:/Users/rooda/Dropbox/Patagonia/GIS South/Temperature_v10.shp")
t2m_obs<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/Data_temperature_v10.xlsx", sheet = "data_monthly", guess_max = 30000)
t2m_obs$Date <- NULL

t2m_sim<-as.data.frame(t(extract(t2m_era5_v2,t2m_shape, method='simple')))
ME_t2m<-me(sim=t2m_sim, obs=t2m_obs[481:840,], na.rm=TRUE)

t2m_sim2<-t2m_sim
for (i in 1:91) {t2m_sim2[,i]<-t2m_sim[,i]-ME_t2m[i]}
for (i in 1:91) {t2m_sim2[,i]<-t2m_sim2[,i]-mean(t2m_sim2[,i])}

rSD_t2m2<-1/rSD(sim=t2m_sim2, obs=t2m_obs[481:840,], na.rm=TRUE)

rSD_t2m<-rSD(sim=t2m_sim, obs=t2m_obs[481:840,], na.rm=TRUE)
write.csv(cbind(ME_t2m, rSD_t2m, rSD_t2m2),"ME_rSD_t2m.csv")

#Evapotranspiration
t2m_era5_v2<-brick("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_ERA5_1990_2019_v2.nc", varname = "tas")
lat <- init(t2m_era5_v2, "y")
n<-nrow(t2m_era5_v2)*ncol(t2m_era5_v2)

cl<-makeCluster(8)
registerDoParallel(cl)

PET_values<-foreach(i = 1:n, .packages = c("raster","SPEI")) %dopar% {
  unlist(thornthwaite(as.vector(t2m_era5_v2[i], "numeric"), lat[i]))
  }

parallel::stopCluster(cl)

PET_era5_v2<-brick(t2m_era5_v2, values= FALSE)
for (i in 1:n) {
  PET_era5_v2[i] <- unlist(PET_values[i])
  print(i)
}

PET_era5_v2<-setZ(PET_era5_v2,seq(as.Date("1990/1/1"), as.Date("2019/12/1"), "month"))
PET_era5_v3<-PET_era5_v2
PET_era5_v3[is.na(PET_era5_v3)] <- 0
PET_era5_v3[!is.finite(PET_era5_v3)] <- 0
PET_era5_v3[PET_era5_v3>200] <- 0

PET_era5_v3_mean<-mean(stackApply(PET_era5_v3, indices<-format(PET_era5_v2@z$time,"%y"), fun=sum))

writeRaster(PET_era5_v3, "PET_ERA5_1990_2019_v2.nc", format = "CDF", overwrite=TRUE, varname="pet", varunit="mm", longname="temperature", xname="X", yname="Y", zname="time", zunit="month")
writeRaster(PET_era5_v3_mean, "PET_ERA5_1990_2019_v2_mean.tif", format = "GTiff", overwrite=TRUE)
