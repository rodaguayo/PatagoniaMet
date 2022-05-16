# Code to replicate precipitation validation 
# Developed by Rodrigo Aguayo (2020-2022)


rm(list=ls())
cat("\014")  

library("hydroGOF")
library("terra")

setwd("/home/rooda/Dropbox/Patagonia/Data/Precipitation/")

#Observations (location and data)
pp_shape <- read.csv("Metadata_Precipitation_v10.csv")
pp_shape <- vect(pp_shape, geom=c("Longitude", "Latitude"), crs="epsg:4326")
pp_obs   <- read.csv("Data_Precipitation_v10_monthly.csv")
pp_obs$Date<-as.Date(pp_obs$Date) #The date is the first column

#Climate models (reanalysis, satellites, etc)
pp_era5l_raw<- rast("PP_ERA5l_1950_2021m.nc")
pp_era5_raw <- rast("PP_ERA5_1950_2021m.nc")
pp_era5_ds  <- rast("PP_ERA5_hr_1990_2019.nc")

pp_merra2   <-rast("PP_MERRA2_1980_2019m.nc")
pp_merra2   <-setNames(pp_merra2,seq(as.Date("1980/1/1"), as.Date("2019/12/1"), "month"))

pp_csfr     <-rast("PP_CSFR_1979_2019.nc")
pp_csfr     <-setNames(pp_csfr,seq(as.Date("1979/1/1"), as.Date("2019/12/1"), "month"))

pp_cr2reg   <-rast("PP_CR2MET_RegCM4_1980_2015.nc")
pp_cr2reg   <-setNames(pp_cr2reg,seq(as.Date("1980/1/1"), as.Date("2015/12/1"), "month"))
pp_cr2reg   <-setZ(pp_cr2reg,seq(as.Date("1980/1/1"), as.Date("2015/12/1"), "month"))
pp_cr2reg   <-subset(pp_cr2reg, which(getZ(pp_cr2reg) >= '1990-01-01' & (getZ(pp_cr2reg) <= '2019-12-31')))

pp_cr2met   <-rast("PP_CR2MET_1979_2019.nc")
pp_cr2met   <-setNames(pp_cr2met,seq(as.Date("1979/1/1"), as.Date("2019/12/1"), "month"))
pp_cr2met   <-setZ(pp_cr2met,seq(as.Date("1979/1/1"), as.Date("2019/12/1"), "month"))
pp_cr2met   <-subset(pp_cr2met, which(getZ(pp_cr2met) >= '1990-01-01' & (getZ(pp_cr2met) <= '2019-12-31')))

pp_mswep   <-rast("PP_MSWEP_1979_2019.nc")
pp_mswep   <-setNames(pp_mswep,seq(as.Date("1979/2/1"), as.Date("2019/12/1"), "month"))

pp_pmet_v0 <-rast("PP_PMET_1990_2019_v1.nc")
pp_pmet    <-rast("PP_PMET_1990_2019_v2.nc")

#Extract
pp_era5    <-t(extract(pp_era5,    pp_shape, method='simple'))
pp_era5_v2 <-t(extract(pp_era5_v2, pp_shape, method='simple'))
pp_merra2  <-t(extract(pp_merra2,  pp_shape, method='simple'))
pp_csfr    <-t(extract(pp_csfr,    pp_shape, method='simple'))
pp_cr2reg  <-t(extract(pp_cr2reg,  pp_shape, method='simple'))
pp_cr2met  <-t(extract(pp_cr2met,  pp_shape, method='simple'))
pp_mswep   <-t(extract(pp_mswep,   pp_shape, method='simple'))
pp_pmet    <-t(extract(pp_pmet,    pp_shape, method='simple'))
pp_pmet_v0 <-t(extract(pp_pmet_v0, pp_shape, method='simple'))
colnames(pp_era5)    <- pp_shape$ID
colnames(pp_era5_v2) <- pp_shape$ID
colnames(pp_merra2)  <- pp_shape$ID
colnames(pp_csfr)    <- pp_shape$ID
colnames(pp_cr2reg)  <- pp_shape$ID
colnames(pp_cr2met)  <- pp_shape$ID
colnames(pp_mswep)   <- pp_shape$ID
colnames(pp_pmet)    <- pp_shape$ID
colnames(pp_pmet_v0) <- pp_shape$ID

#Subset and performance (CR2MET, MSWEP and PMET: 1990-2019)
pp_obs_subset<-subset(pp_obs, Date >= min(as.Date(rownames(pp_era5), format =  "X%Y.%m.%d")))[,-1]
KGE_era5<-KGE(sim=pp_era5, obs=pp_obs_subset, method="2012", out.type="full",na.rm=TRUE)

pp_obs_subset<-subset(pp_obs, Date >= min(as.Date(rownames(pp_era5_v2), format =  "X%Y.%m.%d")))[,-1]
KGE_era5_v2<-KGE(sim=pp_era5_v2, obs=pp_obs_subset, method="2012", out.type="full",na.rm=TRUE)

pp_obs_subset<-subset(pp_obs, Date >= min(as.Date(rownames(pp_merra2), format =  "X%Y.%m.%d")))[,-1]
KGE_merra2<-KGE(sim=pp_merra2, obs= pp_obs_subset, method="2012", out.type="full",na.rm=TRUE)

pp_obs_subset<-subset(pp_obs, Date >= min(as.Date(rownames(pp_csfr), format =  "X%Y.%m.%d")))[,-1]
KGE_csfr<-KGE(sim=pp_csfr, obs=pp_obs_subset, method="2012", out.type="full",na.rm=TRUE)

pp_obs_subset<-subset(pp_obs, Date >= min(as.Date(rownames(pp_cr2reg), format =  "X%Y.%m.%d")) & Date <= max(as.Date(rownames(pp_cr2reg), format =  "X%Y.%m.%d")))[,-1]
KGE_cr2reg<-KGE(sim=pp_cr2reg, obs=pp_obs_subset, method="2012", out.type="full",na.rm=TRUE)

pp_obs_subset<-subset(pp_obs, Date >= min(as.Date(rownames(pp_cr2met), format =  "X%Y.%m.%d")))[,-1]
KGE_cr2met<-KGE(sim=pp_cr2met, obs=pp_obs_subset, method="2012", out.type="full",na.rm=TRUE)

pp_obs_subset<-subset(pp_obs, Date >= min(as.Date(rownames(pp_mswep), format =  "X%Y.%m.%d")))[,-1]
KGE_mswep<-KGE(sim=pp_mswep, obs=pp_obs_subset, method="2012", out.type="full",na.rm=TRUE)

pp_obs_subset<-subset(pp_obs, Date >= min(as.Date(rownames(pp_pmet), format =  "X%Y.%m.%d")))[,-1]
KGE_pmet<-KGE(sim=pp_pmet, obs=pp_obs_subset, method="2012", out.type="full",na.rm=TRUE)

pp_obs_subset<-subset(pp_obs, Date >= min(as.Date(rownames(pp_pmet_v0), format =  "X%Y.%m.%d")))[,-1]
KGE_pmet_v0<-KGE(sim=pp_pmet_v0, obs=pp_obs_subset, method="2012", out.type="full",na.rm=TRUE)

#Merge and save
KGE_era5 <- cbind(t(KGE_era5$KGE.elements),KGE_era5$KGE.value)
colnames(KGE_era5)<-paste0(colnames(KGE_era5), "_ERA5")

KGE_era5_v2 <- cbind(t(KGE_era5_v2$KGE.elements),KGE_era5_v2$KGE.value)
colnames(KGE_era5_v2)<-paste0(colnames(KGE_era5_v2), "_ERA5d")

KGE_merra2 <- cbind(t(KGE_merra2$KGE.elements),KGE_merra2$KGE.value)
colnames(KGE_merra2)<-paste0(colnames(KGE_merra2), "_MERRA2")

KGE_csfr <- cbind(t(KGE_csfr$KGE.elements),KGE_csfr$KGE.value)
colnames(KGE_csfr)<-paste0(colnames(KGE_csfr), "_CSFR")

KGE_cr2reg <- cbind(t(KGE_cr2reg$KGE.elements),KGE_cr2reg$KGE.value)
colnames(KGE_cr2reg)<-paste0(colnames(KGE_cr2reg), "_CR2REG")

KGE_cr2met <- cbind(t(KGE_cr2met$KGE.elements),KGE_cr2met$KGE.value)
colnames(KGE_cr2met)<-paste0(colnames(KGE_cr2met), "_CR2MET")

KGE_mswep <- cbind(t(KGE_mswep$KGE.elements),KGE_mswep$KGE.value)
colnames(KGE_mswep)<-paste0(colnames(KGE_mswep), "_MSWEP")

KGE_pmet_v0 <- cbind(t(KGE_pmet_v0$KGE.elements),KGE_pmet_v0$KGE.value)
colnames(KGE_pmet_v0)<-paste0(colnames(KGE_pmet_v0), "_PMET_v0")

KGE_pmet <- cbind(t(KGE_pmet$KGE.elements),KGE_pmet$KGE.value)
colnames(KGE_pmet)<-paste0(colnames(KGE_pmet), "_PMET")

#Merge and save
KGE<-cbind(KGE_era5, KGE_era5_v2, KGE_merra2, KGE_csfr, KGE_cr2reg, KGE_cr2met, KGE_mswep, KGE_pmet_v0, KGE_pmet)
write.csv(KGE,"home/rooda/Dropbox/Rstudio/Validation_PP.csv")
