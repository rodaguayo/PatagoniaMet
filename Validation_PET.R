# Code to replicate potential evapotranspiration validation
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("hydroGOF")
library("raster")
library("readxl")

setwd("/home/rooda/Dropbox/Patagonia/")

#Observations (location and data)
pet_shape <- read.csv("Data/Evapotranspiration/Metadata_Evapotranspiration_v10.csv")
pet_shape <- vect(pet_shape, geom=c("Longitude", "Latitude"), crs="epsg:4326")
pet_obs   <- read.csv("Data/Evapotranspiration/Data_Evapotranspiration_v10_monthly.csv")
pet_obs$Date<-as.Date(pet_obs$Date) #The date is the first column

# GLEAM v3.5a
pet_gleam <- rast("Data/Evapotranspiration/PET_GLEAM_1990_2019.nc")
pet_gleam <- pet_gleam[[which(time(pet_gleam) >= as.Date("2009-12-31"))]]
pet_gleam <-setNames(pet_gleam, seq(as.Date("2010/1/1"), as.Date("2019/12/31"), "month"))
pet_gleam <-as.data.frame(t(extract(pet_gleam, pet_shape, method='simple')))
colnames(pet_gleam) <- pet_shape$Name

#Performance
pet_obs_subset<-subset(pet_obs, Date <= max(as.Date(rownames(pet_gleam), format =  "X%Y.%m.%d")))[,-1]

KGE_pet_gleam<-KGE(sim=pet_gleam, obs=pet_obs_subset, method="2012", out.type="full",na.rm=TRUE)
ME_pet_gleam<-me(sim=pet_gleam, obs=pet_obs_subset, na.rm=TRUE)
rSD_pet_gleam<-rSD(sim=pet_gleam, obs=pet_obs_subset, na.rm=TRUE)

#Save
pet_validation<-cbind(t(KGE_pet_gleam$KGE.elements),KGE_pet_gleam$KGE.value, ME_pet_gleam, rSD_pet_gleam)
write.csv(pet_validation,"C:/Users/rooda/Dropbox/Rstudio/PET_Validation.csv")
