rm(list=ls())
cat("\014")  

library("hydroGOF")
library("raster")
library("readxl")

#Observations
pet_shape<-shapefile("C:/Users/rooda/Dropbox/Patagonia/GIS South/Evapotranspiration_v10.shp")
pet_obs<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Evapotranspiration/Data_Evapotranspiration_v10.xlsx", sheet = "data_monthly", guess_max = 30000))
pet_obs$Date<-as.Date(pet_obs$Date)

#Gleam v3.5a
pet_gleam<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Evapotranspiration/PET_GLEAM_1990_2019.nc", varname = "pet")
pet_gleam <- pet_gleam[[which(getZ(pet_gleam) >= as.Date("2009-12-31"))]]
pet_gleam<-setNames(pet_gleam, seq(as.Date("2010/1/1"), as.Date("2019/12/31"), "month"))
pet_gleam<-as.data.frame(t(extract(pet_gleam, pet_shape, method='simple')))
colnames(pet_gleam) <- pet_shape$Name

#Performance
pet_obs_subset<-subset(pet_obs, Date <= max(as.Date(rownames(pet_gleam), format =  "X%Y.%m.%d")))[,-1]

KGE_pet_gleam<-KGE(sim=pet_gleam, obs=pet_obs_subset, method="2012", out.type="full",na.rm=TRUE)
ME_pet_gleam<-me(sim=pet_gleam, obs=pet_obs_subset, na.rm=TRUE)
rSD_pet_gleam<-rSD(sim=pet_gleam, obs=pet_obs_subset, na.rm=TRUE)

#Save
pet_validation<-cbind(t(KGE_pet_gleam$KGE.elements),KGE_pet_gleam$KGE.value, ME_pet_gleam, rSD_pet_gleam)
write.csv(pet_validation,"C:/Users/rooda/Dropbox/Rstudio/PET_Validation.csv")
