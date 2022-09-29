# Code to replicate potential evapotranspiration validation
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("hydroGOF")
library("terra")

setwd("/home/rooda/Dropbox/Patagonia/Data/Evapotranspiration/")
period     <- c(as.Date("2010-01-01"), as.Date("2020-12-31"))
attributes <- c("N", "ID", "Institution", "Latitude", "Longitude", "Altitude", "Zone")

#Observations (location and data)
pet_validation  <- read.csv("Metadata_Evapotranspiration_v10.csv")
pet_shape       <- vect(pet_validation, geom=c("Longitude", "Latitude"), crs="epsg:4326")
pet_obs         <- read.csv("Data_Evapotranspiration_v10_monthly.csv")
pet_obs$Date    <-as.POSIXct(pet_obs$Date, tz= "UTC") #The date is the first column
pet_validation  <- subset(pet_validation, select = attributes)

pet_stack <- rast("PET_GLEAM36a_1980_2021m.nc") #GLEAM data for 1990-2019
pet_stack <- subset(pet_stack,  which(time(pet_stack)  >= period[1] & time(pet_stack)   <= period[2]))
pet_sim   <- as.data.frame(t(extract(pet_stack, pet_shape, method='simple'))[-1,])
index     <- KGE(sim=pet_sim, obs=pet_obs[,-1], method="2009", out.type="full",na.rm=TRUE)
index     <- data.frame(t(index$KGE.elements), KGE = index$KGE.value, ME = me(sim=pet_sim, obs=pet_obs[,-1], na.rm=TRUE))
colnames(index) <- paste0("GLEAM_", colnames(index))
pet_validation  <- cbind(pet_validation, index)

write.csv(pet_validation, "PET_Validation.csv", row.names = FALSE)
