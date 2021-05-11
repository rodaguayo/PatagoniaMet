rm(list=ls())
cat("\014")  

library("zoo")
library("sf")
library("readxl")
library("raster")
library("qmap")
library("hydroGOF")
library("spatialEco")

#Sample
sample <- matrix(rnorm(220*340),340,220)
sample <- raster(sample)
extent(sample) <- c(-76,-65,-57,-40)
projection(sample) <- CRS("+init=epsg:4326")

#Quantile mapping
pp_shape<-shapefile("C:/Users/rooda/Dropbox/Patagonia/GIS South/Precipitation_v10.shp")
pp_obs<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v10.xlsx", sheet = "data_monthly", guess_max = 30000)
pp_obs<-pp_obs[481:840,]
pp_obs$Date <- NULL

pp_era5<- stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_ERA5_1950_2019.nc", varname = "tp")
pp_era5<- resample(pp_era5, sample, method="bilinear")
pp_era5<-setZ(pp_era5,seq(as.Date("1950/1/1"), as.Date("2019/12/1"), "month"))
pp_era5<- pp_era5[[which(getZ(pp_era5) >= as.Date("1989-12-31"))]]
pp_era5<-setZ(pp_era5,seq(as.Date("1990/1/1"), as.Date("2019/12/1"), "month"))

writeRaster(pp_era5, "PP_ERA5_1990_2019.nc", format = "CDF", datatype='INT2S', overwrite=TRUE, varname="tp", varunit="mm", 
            longname="precipitation", xname="X", yname="Y", zname="time", zunit="month")

pp_sim_era5<-as.data.frame(t(extract(pp_era5, pp_shape, method='simple')))

pp_era5_mean<-mean(stackApply(pp_era5, indices<-format(pp_era5@z$time,"%y"), fun=sum))
writeRaster(pp_era5_mean, "PP_ERA5_005_1990-2019.tif", format = "GTiff")

#Validation and parameters
KGE<-matrix(0,147,28)
QM_parameters<-matrix(0,147,4)
rownames(KGE)<-names(pp_obs)
rownames(QM_parameters)<-names(pp_obs)
colnames(QM_parameters)<-c("a_power","b_power","a_linear","b_linear")

for (i in 1:147) {
pp_obs_i<-as.vector(unlist(pp_obs[,i]))
pp_mod_i<-as.vector(unlist(pp_sim_era5[,i]))
pp_mod_i[is.na(pp_obs_i)] <- NA

pp_obs_i <- pp_obs_i[!is.na(pp_obs_i)]
pp_mod_i <- pp_mod_i[!is.na(pp_mod_i)]

fit_ptf_power<-fitQmapPTF(pp_obs_i, pp_mod_i, transfun="power", wet.day=0, cost="RSS")
fit_ptf_linear<-fitQmapPTF(pp_obs_i, pp_mod_i, transfun="linear", wet.day=0, cost="RSS")
fit_ptf_expasympt<-fitQmapPTF(pp_obs_i, pp_mod_i, transfun="expasympt", wet.day=FALSE, cost="RSS")
fit_ptf_scale<-fitQmapPTF(pp_obs_i, pp_mod_i, transfun="scale", wet.day=FALSE, cost="RSS")
fit_ptf_powerx0<-fitQmapPTF(pp_obs_i, pp_mod_i, transfun="power.x0", wet.day=FALSE, cost="RSS")
fit_ptf_expasymptx0<-fitQmapPTF(pp_obs_i, pp_mod_i, transfun="expasympt.x0", wet.day=FALSE, cost="RSS")

QM_parameters[i,1:2]<-fit_ptf_power$par
QM_parameters[i,3:4]<-fit_ptf_linear$par

pp_bc_power <- doQmapPTF(pp_mod_i,fit_ptf_power)
pp_bc_linear <- doQmapPTF(pp_mod_i,fit_ptf_linear)
pp_bc_expasympt <- doQmapPTF(pp_mod_i,fit_ptf_expasympt)
pp_bc_scale <- doQmapPTF(pp_mod_i,fit_ptf_scale)
pp_bc_powerx0 <- doQmapPTF(pp_mod_i,fit_ptf_powerx0)
pp_bc_expasymmptx0 <- doQmapPTF(pp_mod_i,fit_ptf_expasymptx0)

KGE_original<-KGE(sim=pp_mod_i, obs=pp_obs_i, method="2012", out.type="full",na.rm=TRUE)
KGE_power<-KGE(sim=pp_bc_power, obs=pp_obs_i, method="2012", out.type="full",na.rm=TRUE)
KGE_linear<-KGE(sim=pp_bc_linear, obs=pp_obs_i, method="2012", out.type="full",na.rm=TRUE)
KGE_expasympt<-KGE(sim=pp_bc_expasympt, obs=pp_obs_i, method="2012", out.type="full",na.rm=TRUE)
KGE_scale<-KGE(sim=pp_bc_scale, obs=pp_obs_i, method="2012", out.type="full",na.rm=TRUE)
KGE_powerx0<-KGE(sim=pp_bc_powerx0, obs=pp_obs_i, method="2012", out.type="full",na.rm=TRUE)
KGE_expasymmptx0<-KGE(sim=pp_bc_expasymmptx0, obs=pp_obs_i, method="2012", out.type="full",na.rm=TRUE)

KGE[i,1:4]<-cbind(t(KGE_original$KGE.elements),KGE_original$KGE.value)
KGE[i,5:8]<-cbind(t(KGE_power$KGE.elements),KGE_power$KGE.value)
KGE[i,9:12]<-cbind(t(KGE_linear$KGE.elements),KGE_linear$KGE.value)
KGE[i,13:16]<-cbind(t(KGE_expasympt$KGE.elements),KGE_expasympt$KGE.value)
KGE[i,17:20]<-cbind(t(KGE_scale$KGE.elements),KGE_scale$KGE.value)
KGE[i,21:24]<-cbind(t(KGE_powerx0$KGE.elements),KGE_powerx0$KGE.value)
KGE[i,25:28]<-cbind(t(KGE_expasymmptx0$KGE.elements),KGE_expasymmptx0$KGE.value)

#print(i)
}

write.csv(KGE,"KGE_pp.csv")
write.csv(QM_parameters,"QM_parameters.csv")


#Rasterstack correction
sample <- raster(matrix(rnorm(220*340),340,220))
extent(sample) <- c(-76,-65,-57,-40)
projection(sample) <- CRS("+init=epsg:4326")

pp_era5<- stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_ERA5_1950_2019.nc", varname = "tp")
pp_era5<- resample(pp_era5, sample, method="bilinear")
pp_era5<-setZ(pp_era5,seq(as.Date("1950/1/1"), as.Date("2019/12/1"), "month"))
pp_era5<- pp_era5[[which(getZ(pp_era5) >= as.Date("1989-12-31"))]]

dem<- raster("C:/Users/rooda/Dropbox/Patagonia/GIS South/dem_patagonia005.tif")
dem[dem <= 1] <- NA
pp_era5=mask(pp_era5, dem)

a_linear<-resample(raster("C:/Users/rooda/Dropbox/Patagonia/MS1 Results/PP_a_linear.tif"), sample)
b_linear<-resample(raster("C:/Users/rooda/Dropbox/Patagonia/MS1 Results/PP_b_linear.tif"), sample)
b_linear[b_linear == 0] <- NA

gf <- focalWeight(pp_era5, 0.04, "Gauss")
a_linear <- mask(focal(a_linear, w=gf, pad = TRUE), dem)
b_linear <- mask(focal(b_linear, w=gf, pad = TRUE), dem)

pp_era5_v2=a_linear+pp_era5*b_linear
pp_era5_v2[pp_era5_v2 < 0] <- 0
pp_era5_v2<-setZ(pp_era5_v2,seq(as.Date("1990/1/1"), as.Date("2019/12/1"), "month"))
pp_era5_v2_mean<-mean(stackApply(pp_era5_v2, indices<-format(pp_era5_v2@z$time,"%y"), fun=sum))
plot(pp_era5_v2_mean)

writeRaster(pp_era5_v2, "PP_PMET_1990_2019_v1.nc", format = "CDF", overwrite=TRUE, varname="PP", varunit="mm", longname="Precipitation", xname="X", yname="Y", zname="time", zunit="month")
writeRaster(pp_era5_v2_mean, "PP_PMET_1990-2019_v1.tif", format = "GTiff", overwrite = TRUE)

#Rasterstack correction Budyko
pp_patagoniamet<- stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_PMET_1990_2019_v1.nc", varname = "PP")
bias_factor<-resample(raster("C:/Users/rooda/Dropbox/Patagonia/GIS South/bias_factor005.tif"),pp_patagoniamet)
bias_factor[bias_factor == 0] <- NA

bias_factor <- focal(bias_factor, w=focalWeight(bias_factor, 0.04, "Gauss"))
bias_factor[bias_factor <= 1] <- 1
bias_factor<-mask(bias_factor,pp_patagoniamet[[1]])

writeRaster(bias_factor, "bias_factor005_v2.tif", format = "GTiff", overwrite = TRUE)

pp_patagoniamet_v2<-pp_patagoniamet*bias_factor

pp_patagoniamet_v2<-setZ(pp_patagoniamet_v2,seq(as.Date("1990/1/1"), as.Date("2019/12/1"), "month"))
pp_patagoniamet_v2_mean<-mean(stackApply(pp_patagoniamet_v2, indices<-format(pp_patagoniamet_v2@z$time,"%y"), fun=sum))
pp_patagoniamet_v2_mean[pp_patagoniamet_v2_mean == 0] <- NA

writeRaster(pp_patagoniamet_v2, "PP_PMET_1990_2019_v2.nc", format = "CDF", overwrite=TRUE, varname="PP", varunit="mm", longname="precipitation", xname="X", yname="Y", zname="time", zunit="month")
writeRaster(pp_patagoniamet_v2_mean, "PP_PMET_1990-2019_mean_v2.tif", format = "GTiff", overwrite = TRUE)
  
#Validation
pp_obs<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v10.xlsx", sheet = "data_monthly", guess_max = 30000))
pp_shape<-shapefile("C:/Users/rooda/Dropbox/Patagonia/GIS South/Precipitation_v10.shp")
pp_obs$Date <- NULL

pp_sim_era5<-as.data.frame(t(extract(pp_era5_v2,pp_shape, method='simple')))
KGE_pp_era5_v2<-KGE(sim=pp_sim_era5, obs=pp_obs[481:840,], method="2012", out.type="full",na.rm=TRUE)
write.csv(cbind(t(KGE_pp_era5_v2$KGE.elements),KGE_pp_era5_v2$KGE.value),"KGE_PP_ERA5_v2.csv")
