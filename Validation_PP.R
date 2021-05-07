rm(list=ls())
cat("\014")  

library("hydroGOF")
library("raster")
library("readxl")

#Precipitation validation

#Observations
pp_shape<-shapefile("C:/Users/rooda/Dropbox/Patagonia/GIS South/Precipitation_v10.shp")
pp_obs<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v10.xlsx", sheet = "data_monthly", guess_max = 30000)
pp_obs<-as.data.frame(pp_obs)
pp_obs$Date <- NULL

#Climate models (reanalysis, satellites, etc)
pp_era5<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_ERA5_1950_2019.nc", varname = "tp")

pp_merra2<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_MERRA2_1980_2019.nc")
pp_merra2<-setZ(pp_merra2,seq(as.Date("1980/1/1"), as.Date("2019/12/1"), "month"))

pp_csfr<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_CSFR_1979_2019.nc")
pp_csfr<-setZ(pp_csfr,seq(as.Date("1979/1/1"), as.Date("2019/12/1"), "month"))

pp_cr2met_reg<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_CR2MET_RegCM4_1980_2015.nc")
pp_cr2met_reg<-setZ(pp_cr2met_reg,seq(as.Date("1980/1/1"), as.Date("2015/12/1"), "month"))

pp_cr2met<-stack("E:/Datasets/CR2MET/PP_CR2MET_1979_2018.nc")
pp_cr2met<-setZ(pp_cr2met,seq(as.Date("1979/1/1"), as.Date("2018/12/1"), "month"))

pp_mswep<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_MSWEP_1979_2019.nc")
pp_mswep<-setZ(pp_mswep,seq(as.Date("1979/2/1"), as.Date("2019/12/1"), "month"))

pp_pmet<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_PMET_1990_2019.nc")

#Extract
pp_sim_era5<-as.data.frame(t(extract(pp_era5,pp_shape, method='simple')))
pp_sim_merra2<-as.data.frame(t(extract(pp_merra2,pp_shape, method='simple')))
pp_sim_csfr<-as.data.frame(t(extract(pp_csfr,pp_shape, method='simple')))
pp_sim_cr2met_reg<-as.data.frame(t(extract(pp_cr2met_reg,pp_shape, method='simple')))
pp_sim_cr2met<-as.data.frame(t(extract(pp_cr2met,pp_shape, method='simple')))
pp_sim_mswep<-as.data.frame(t(extract(pp_mswep,pp_shape, method='simple')))
colnames(pp_sim_cr2met) <- pp_shape$ID
colnames(pp_sim_mswep) <- pp_shape$ID
colnames(pp_sim_era5) <- pp_shape$ID
colnames(pp_sim_merra2) <- pp_shape$ID
colnames(pp_sim_csfr) <- pp_shape$ID
colnames(pp_sim_cr2met_reg) <- pp_shape$ID

#Performance
KGE_pp_era5<-KGE(sim=pp_sim_era5, obs=pp_obs, method="2012", out.type="full",na.rm=TRUE)
KGE_pp_merra2<-KGE(sim=pp_sim_merra2, obs=pp_obs[361:840,], method="2012", out.type="full",na.rm=TRUE)
KGE_pp_csfr<-KGE(sim=pp_sim_csfr, obs=pp_obs[349:840,], method="2012", out.type="full",na.rm=TRUE)
KGE_pp_cr2met_reg<-KGE(sim=pp_sim_cr2met_reg, obs=pp_obs[361:792,], method="2012", out.type="full",na.rm=TRUE)
KGE_pp_cr2met<-KGE(sim=pp_sim_cr2met, obs=pp_obs[349:828,], method="2012", out.type="full",na.rm=TRUE)
KGE_pp_mswep<-KGE(sim=pp_sim_mswep, obs=pp_obs[350:840,], method="2012", out.type="full",na.rm=TRUE)

#Merge and save
KGE_pp_era5<-cbind(t(KGE_pp_era5$KGE.elements),KGE_pp_era5$KGE.value)
KGE_pp_merra2<-cbind(t(KGE_pp_merra2$KGE.elements),KGE_pp_merra2$KGE.value)
KGE_pp_csfr<-cbind(t(KGE_pp_csfr$KGE.elements),KGE_pp_csfr$KGE.value)
KGE_pp_cr2met_reg<-cbind(t(KGE_pp_cr2met_reg$KGE.elements),KGE_pp_cr2met_reg$KGE.value)
KGE_pp_cr2met<-cbind(t(KGE_pp_cr2met$KGE.elements),KGE_pp_cr2met$KGE.value)
KGE_pp_mswep<-cbind(t(KGE_pp_mswep$KGE.elements),KGE_pp_mswep$KGE.value)

KGE<-cbind(KGE_pp_era5, KGE_pp_merra2, KGE_pp_csfr, 
           KGE_pp_cr2met_reg, KGE_pp_cr2met, KGE_pp_mswep)

write.csv(KGE,"Validation_PP.csv")
