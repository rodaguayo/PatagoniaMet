# Code to replicate temperature validation 
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("hydroGOF")
library("terra")

setwd("/home/rooda/Dropbox/Patagonia/Data/Temperature/")

#Observations (location and data)
t2m_shape <- read.csv("Metadata_Temperature/v10.csv")
t2m_shape <- vect(t2m_shape, geom=c("Longitude", "Latitude"), crs="epsg:4326")
t2m_obs   <- read.csv("Data_Temperature_v10_monthly.csv")
t2m_obs$Date<-as.Date(t2m_obs$Date) #The date is the first column

#Climate models (reanalysis, satellites, etc)
t2m_era5<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_ERA5_1950_2019.nc", varname = "tas")
t2m_era5_v2<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_ERA5_1990_2019_v1.nc", varname = "tas")

t2m_merra2<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_MERRA2_1980_2019.nc")
t2m_merra2<-setNames(t2m_merra2,seq(as.Date("1980/1/1"), as.Date("2019/12/1"), "month"))

t2m_csfr<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_CSFR_1979_2019.nc")
t2m_csfr<-setNames(t2m_csfr,seq(as.Date("1979/1/1"), as.Date("2019/12/1"), "month"))

t2m_cr2reg<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_CR2MET_RegCM4__1980_2015.nc")
t2m_cr2reg<-setNames(t2m_cr2reg,seq(as.Date("1980/1/1"), as.Date("2015/12/1"), "month"))

t2m_cr2met<-stack("E:/Datasets/CR2MET/T2M_CR2MET_1979_2019.nc")
t2m_cr2met<-setNames(t2m_cr2met,seq(as.Date("1979/1/1"), as.Date("2019/12/1"), "month"))
t2m_cr2met<-setZ(t2m_cr2met,seq(as.Date("1979/1/1"), as.Date("2019/12/1"), "month"))
t2m_cr2met<-subset(t2m_cr2met, which(getZ(t2m_cr2met) >= '1990-01-01' & (getZ(t2m_cr2met) <= '2019-12-31')))

t2m_pmet<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_PMET_1990_2019.nc", varname = "tas")

#Extract
t2m_era5<-t(extract(t2m_era5,t2m_shape, method='simple'))
t2m_era5_v2<-t(extract(t2m_era5_v2,t2m_shape, method='simple'))
t2m_merra2<-t(extract(t2m_merra2,t2m_shape, method='simple'))
t2m_csfr<-t(extract(t2m_csfr,t2m_shape, method='simple'))
t2m_cr2reg<-t(extract(t2m_cr2reg,t2m_shape, method='simple'))
t2m_cr2met<-t(extract(t2m_cr2met,t2m_shape, method='simple'))
t2m_cr2met[is.na(t2m_cr2met)] <- -9999
t2m_pmet<-t(extract(t2m_pmet,t2m_shape, method='simple'))
colnames(t2m_era5) <- t2m_shape$ID
colnames(t2m_era5_v2) <- t2m_shape$ID
colnames(t2m_merra2) <- t2m_shape$ID
colnames(t2m_csfr) <- t2m_shape$ID
colnames(t2m_cr2reg) <- t2m_shape$ID
colnames(t2m_cr2met) <- t2m_shape$ID
colnames(t2m_pmet) <- t2m_shape$ID

#Subset and performance (CR2MET, ERA5v2 and PMET: 1990-2019)
t2m_obs_subset<-subset(t2m_obs, Date >= min(as.Date(rownames(t2m_era5), format =  "X%Y.%m.%d")))[,-1]
ME_era5<-cbind(me(sim=t2m_era5, obs=t2m_obs_subset, na.rm=TRUE),rSD(sim=t2m_era5, obs=t2m_obs_subset, na.rm=TRUE))

t2m_obs_subset<-subset(t2m_obs, Date >= min(as.Date(rownames(t2m_era5_v2), format =  "X%Y.%m.%d")))[,-1]
ME_era5_v2<-cbind(me(sim=t2m_era5_v2, obs=t2m_obs_subset, na.rm=TRUE),rSD(sim=t2m_era5_v2, obs=t2m_obs_subset, na.rm=TRUE),1/rSD(sim=t2m_era5_v2, obs=t2m_obs_subset, na.rm=TRUE))

t2m_obs_subset<-subset(t2m_obs, Date >= min(as.Date(rownames(t2m_merra2), format =  "X%Y.%m.%d")))[,-1]
ME_merra2<-cbind(me(sim=t2m_merra2, obs=t2m_obs_subset, na.rm=TRUE), rSD(sim=t2m_merra2, obs=t2m_obs_subset, na.rm=TRUE))

t2m_obs_subset<-subset(t2m_obs, Date >= min(as.Date(rownames(t2m_csfr), format =  "X%Y.%m.%d")))[,-1]
ME_csfr<-cbind(me(sim=t2m_csfr, obs=t2m_obs_subset, na.rm=TRUE), rSD(sim=t2m_csfr, obs=t2m_obs_subset, na.rm=TRUE))

t2m_obs_subset<-subset(t2m_obs, Date >= min(as.Date(rownames(t2m_cr2reg), format =  "X%Y.%m.%d")) & Date <= max(as.Date(rownames(t2m_cr2reg), format =  "X%Y.%m.%d")))[,-1]
ME_cr2reg<-cbind(me(sim=t2m_cr2reg, obs=t2m_obs_subset, na.rm=TRUE), rSD(sim=t2m_cr2reg, obs=t2m_obs_subset, na.rm=TRUE))

t2m_obs_subset<-subset(t2m_obs, Date >= min(as.Date(rownames(t2m_cr2met), format =  "X%Y.%m.%d")))[,-1]
ME_cr2met<-cbind(me(sim=t2m_cr2met, obs=t2m_obs_subset, na.rm=TRUE), rSD(sim=t2m_cr2met, obs=t2m_obs_subset, na.rm=TRUE))
ME_cr2met[c(6,36,55,91),]<-NA

t2m_obs_subset<-subset(t2m_obs, Date >= min(as.Date(rownames(t2m_pmet), format =  "X%Y.%m.%d")))[,-1]
ME_pmet<-cbind(me(sim=t2m_pmet, obs=t2m_obs_subset, na.rm=TRUE), rSD(sim=t2m_pmet, obs=t2m_obs_subset, na.rm=TRUE))

#Merge and save
valitation_t2m<-cbind(ME_era5, ME_era5_v2, ME_merra2, ME_csfr, ME_cr2reg, ME_cr2met, ME_pmet)
colnames(valitation_t2m)<-c("ME_ERA5","rSD_ERA5","ME_ERA5_v2","rSD_ERA5_v2", "rSD_ERA5_v3","ME_MERRA2","rSD_MERRA2",
                            "ME_CSFR","rSD_CSFR","ME_CR2REG","rSD_CR2REG","ME_CR2MET","rSD_CR2MET", "ME_PMET", "rSD_PMET")
write.csv(valitation_t2m,"C:/Users/rooda/Dropbox/Rstudio/Validation_T2M.csv")

