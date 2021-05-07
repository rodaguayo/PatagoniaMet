rm(list=ls())
cat("\014")  

library("hydroGOF")
library("raster")
library("readxl")

#Observations
t2m_shape<-shapefile("C:/Users/rooda/Dropbox/Patagonia/GIS South/Temperature_v10.shp")
t2m_obs<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/Data_temperature_v10.xlsx", sheet = "data_monthly", guess_max = 30000)
t2m_obs<-as.data.frame(t2m_obs)
t2m_obs$Date <- NULL

#Climate models (reanalysis, satellites, etc)
t2m_era5<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_ERA5_1950_2019.nc", varname = "tas")

t2m_merra2<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_MERRA2_1980_2019.nc")
t2m_merra2<-setZ(t2m_merra2,seq(as.Date("1980/1/1"), as.Date("2019/12/1"), "month"))

t2m_csfr<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_CSFR_1979_2019.nc")
t2m_csfr<-setZ(t2m_csfr,seq(as.Date("1979/1/1"), as.Date("2019/12/1"), "month"))

t2m_cr2reg<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_CR2MET_RegCM4__1980_2015.nc")
t2m_cr2reg<-setZ(t2m_cr2met_reg,seq(as.Date("1980/1/1"), as.Date("2015/12/1"), "month"))

t2m_cr2met<-stack("H:/Datasets/CR2MET/T2M_CR2MET_1979_2018.nc")
t2m_cr2met<-setZ(t2m_cr2met,seq(as.Date("1979/1/1"), as.Date("2018/12/1"), "month"))

#Extract
t2m_sim_era5<-as.data.frame(t(extract(t2m_era5,t2m_shape, method='simple')))
t2m_sim_merra2<-as.data.frame(t(extract(t2m_merra2,t2m_shape, method='simple')))
t2m_sim_csfr<-as.data.frame(t(extract(t2m_csfr,t2m_shape, method='simple')))
t2m_sim_cr2met_reg<-as.data.frame(t(extract(t2m_cr2reg,t2m_shape, method='simple')))
t2m_sim_cr2met<-as.data.frame(t(extract(t2m_cr2met,t2m_shape, method='simple')))
colnames(t2m_sim_cr2met) <- t2m_shape$ID
colnames(t2m_sim_era5) <- t2m_shape$ID
colnames(t2m_sim_merra2) <- t2m_shape$ID
colnames(t2m_sim_csfr) <- t2m_shape$ID
colnames(t2m_sim_cr2reg) <- t2m_shape$ID

#Performance
ME_t2m_era5<-me(sim=t2m_sim_era5, obs=t2m_obs, na.rm=TRUE)
ME_t2m_merra2<-me(sim=t2m_sim_merra2, obs=t2m_obs[361:840,], na.rm=TRUE)
ME_t2m_csfr<-me(sim=t2m_sim_csfr, obs=t2m_obs[349:840,], na.rm=TRUE)
ME_t2m_cr2met_reg<-me(sim=t2m_sim_cr2met_reg, obs=t2m_obs[361:792,], na.rm=TRUE)
ME_t2m_cr2met<-me(sim=t2m_sim_cr2met, obs=t2m_obs[349:828,], na.rm=TRUE)

rSD_t2m_era5<-rSD(sim=t2m_sim_era5, obs=t2m_obs, na.rm=TRUE)
rSD_t2m_merra2<-rSD(sim=t2m_sim_merra2, obs=t2m_obs[361:840,], na.rm=TRUE)
rSD_t2m_csfr<-rSD(sim=t2m_sim_csfr, obs=t2m_obs[349:840,], na.rm=TRUE)
rSD_t2m_cr2met_reg<-rSD(sim=t2m_sim_cr2met_reg, obs=t2m_obs[361:792,], na.rm=TRUE)
rSD_t2m_cr2met<-rSD(sim=t2m_sim_cr2met[-c(6,36,55,91)], obs=t2m_obs[-c(6,36,55,91)][349:828,], na.rm=TRUE)
rSD_t2m_cr2met<-append(rSD_t2m_cr2met, NA, after=5)
rSD_t2m_cr2met<-append(rSD_t2m_cr2met, NA, after=35)
rSD_t2m_cr2met<-append(rSD_t2m_cr2met, NA, after=54)
rSD_t2m_cr2met<-append(rSD_t2m_cr2met, NA, after=90)

ME_t2m<-rbind(ME_t2m_era5, ME_t2m_merra2, ME_t2m_csfr, ME_t2m_cr2met_reg, ME_t2m_cr2met)
rSD_t2m<-rbind(rSD_t2m_era5, rSD_t2m_merra2, rSD_t2m_csfr, rSD_t2m_cr2met_reg, rSD_t2m_cr2met)
valitation_t2m<-cbind(ME_t2m, rSD_t2m)

write.csv(valitation_t2m,"Validation_T2M.xlsx")

