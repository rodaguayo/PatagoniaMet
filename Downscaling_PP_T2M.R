rm(list=ls())
cat("\014")  

library("raster")

#Sample
sample <- matrix(rnorm(220*340),340,220)
sample <- raster(sample)
extent(sample) <- c(-76,-65,-57,-40)
projection(sample) <- CRS("+init=epsg:4326")

#Bilinear resampling for PP
pp_era5<- stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_ERA5_1950_2019.nc", varname = "tp")
pp_era5<- resample(pp_era5, sample, method="bilinear")
pp_era5<-setZ(pp_era5,seq(as.Date("1950/1/1"), as.Date("2019/12/1"), "month"))
pp_era5<- pp_era5[[which(getZ(pp_era5) >= as.Date("1989-12-31"))]]
pp_era5<-setZ(pp_era5,seq(as.Date("1990/1/1"), as.Date("2019/12/1"), "month"))

pp_era5_mean<-mean(stackApply(pp_era5, indices<-format(pp_era5@z$time,"%y"), fun=sum))

writeRaster(pp_era5, "PP_ERA5_1990_2019.nc", format = "CDF", datatype='INT2S', overwrite=TRUE, varname="tp", varunit="mm", 
            longname="precipitation", xname="X", yname="Y", zname="time", zunit="month")
writeRaster(pp_era5_mean, "PP_ERA5_1990-2019.tif", format = "GTiff")


#Downscaling using a lapse rate of 6.5C km-1
sample <- matrix(rnorm(220*340),340,220)
sample <- raster(sample)
extent(sample) <- c(-76,-65,-57,-40)
projection(sample) <- CRS("+init=epsg:4326")

dem<-raster("C:/Users/rooda/Dropbox/Patagonia/GIS South/dem_patagonia3f.tif")
t2m_era5_v0<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_ERA5_1950_2019.nc", varname = "tas")
t2m_era5_v0<-t2m_era5_v0[[which(getZ(t2m_era5_v0) >= as.Date("1989-12-31"))]]

dem[is.na(dem)] <- 0
dem_005<-resample(dem, sample, method="bilinear")
dem_025<-resample(dem, t2m_era5_v0, method="bilinear")
dem_025<-resample(dem_025, dem_005, method="ngb")

t2m_era5_v1<-resample(t2m_era5_v0, dem_005, method="ngb")
t2m_era5_v1<-t2m_era5_v1+(dem_025-dem_005)*0.0065
t2m_era5_v1<-round(t2m_era5_v1, 2)
t2m_era5_v1<-setZ(t2m_era5_v1, getZ(t2m_era5_v0))

t2m_era5_v1_mean<-mean(stackApply(t2m_era5_v1, indices<-format(t2m_era5_v1@z$time,"%y"), fun=mean))

writeRaster(t2m_era5_v1, "T2M_ERA5_1990_2019_v1.nc", format = "CDF", overwrite=TRUE, varname="t2m", varunit="degC", 
            longname="temperature", xname="X", yname="Y", zname="time", zunit="month")
writeRaster(t2m_era5_v1_mean, "T2M_ERA5_1990_2019.tif", format = "GTiff")


#Alternive: best lapse rate according to 

library("hydroGOF")
t2m_shape<-shapefile("C:/Users/rooda/Dropbox/Patagonia/GIS South/Temperature_v10.shp")

ME_t2m_era5_v1 <- function(lr) {
  
  t2m_era5_v1<-resample(t2m_era5_v0, dem_005, method="ngb")
  t2m_era5_v1<-t2m_era5_v1+(dem_025-dem_005)*lr
  
  t2m_sim_era5_v1<-as.data.frame(t(extract(t2m_era5_v1,t2m_shape, method='simple')))
  ME_t2m_era5_v1<-sum((me(sim=t2m_sim_era5_v1, obs=t2m_obs, na.rm=TRUE))^2)
  
}

best_lr<-optim(0.0065, ME_t2m_era5_v1)

