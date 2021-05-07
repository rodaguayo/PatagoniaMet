rm(list=ls())
cat("\014")  

library("hydroGOF")
library("raster")
library("xlsx")
library("readxl")

#Validation for GLEAM
pet_shape<-shapefile("C:/Users/rooda/Dropbox/Patagonia/GIS South/Evapotranspiration_v10.shp")
pet_obs<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Evapotranspiration/Data_Evapotranspiration_v10.xlsx", sheet = "data_monthly", guess_max = 30000))
pet_obs$Date <- NULL

#Climate models (reanalysis, satellites, etc)
pet_gleam<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Evapotranspiration/PET_GLEAM_1980_2020.nc", varname = "pet")
pet_gleam <- pet_gleam[[which(getZ(pet_gleam) >= as.Date("2009-12-31"))]]
pet_gleam<-setZ(pet_gleam, seq(as.Date("2010/1/1"), as.Date("2020/12/31"), "month"))

#Extract
pet_sim_gleam<-as.data.frame(t(extract(pet_gleam,pet_shape, method='simple')))
colnames(pet_sim_gleam) <- pet_shape$Name

#Performance
KGE_pet_gleam<-KGE(sim=pet_sim_gleam, obs=pet_obs, method="2012", out.type="full",na.rm=TRUE)
ME_pet_gleam<-me(sim=pet_sim_gleam, obs=pet_obs, na.rm=TRUE)
rSD_pet_gleam<-rSD(sim=pet_sim_gleam, obs=pet_obs, na.rm=TRUE)

#Save
write.xlsx(cbind(t(KGE_pet_gleam$KGE.elements),KGE_pet_gleam$KGE.value, ME_pet_gleam, rSD_pet_gleam),"PET_Validation.xlsx")
