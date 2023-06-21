# Code for Ep quality control----------------------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2023)

rm(list=ls())
cat("\014")  

library("zoo")
source("TimeResample.R")

setwd("/home/rooda/Dropbox/Patagonia/")
check = FALSE # do you need to visually check the time series?

ep_meta <- read.csv("Data/Evapotranspiration/ET0_PMETobs_v10_metadata_raw.csv")
ep_data <- read.csv("Data/Evapotranspiration/ETo_PMETobs_v10d_raw.csv")
ep_data <- zoo(ep_data[,-1], order.by = as.Date(ep_data$Date))
ep_raw  <- ep_data # for the final comparison

# requirements 
ep_meta$length <- as.numeric(colSums(!is.na(ep_data))) # save initial lengths
operation      <- as.numeric(colSums(!is.na(ep_data[(index(ep_data) > "2000-01-01")])))

# more than 4 years and continue in operation during 2000-2020
ep_meta   <- ep_meta[(ep_meta$length > 365*3) & (operation > 365*1),] 
ep_data   <- ep_data[, ep_meta$gauge_id]

# resampling
ep_data_m <- MonthlyResample(ep_data,  20, FUN = sum)

# save data (daily and monthly)
ep_data_d <- cbind(Date = index(ep_data),   as.data.frame(ep_data))
ep_data_m <- cbind(Date = index(ep_data_m), as.data.frame(ep_data_m))

write.csv(ep_data_d, "Data/Evapotranspiration/ET0_PMETobs_v10d.csv", row.names = FALSE, na = "")
write.csv(ep_data_m, "Data/Evapotranspiration/ETo_PMETobs_v10m.csv", row.names = FALSE, na = "")
write.csv(ep_meta, "Data/Evapotranspiration/ETo_PMETobs_v10_metadata.csv", row.names = FALSE)