# Code to change the time step of the different data sets
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("hydroTSM")
setwd("/home/rooda/Dropbox/Patagonia/Data/")

## Lake levels data: Daily to monthly time step (> 20 days)
lake_daily   <- read.csv("Lake Levels/Data_Lakes_Levels_v10_daily.csv")
lake_daily_c <- read.csv("Lake Levels/Data_Lakes_Levels_v10_daily.csv")

Date <-seq(from = as.Date(min(lake_daily$Date)), to = as.Date(max(lake_daily$Date)), by = "month")
lake_daily_c[,2:length(lake_daily)][lake_daily_c[,2:length(lake_daily)]>-99 ]<- 1
lake_monthly_c <- data.frame(daily2monthly(lake_daily_c, FUN = sum,  na.rm = TRUE))
lake_monthly   <- data.frame(daily2monthly(lake_daily,   FUN = mean, na.rm = TRUE))
lake_monthly[lake_monthly_c < 20]<-NA
lake_monthly<-round(lake_monthly,2)
write.csv(lake_monthly, "Lake Levels/Data_Lakes_Levels_v10_monthly.csv")

## Streamflow data: Daily to monthly (> 20 days) and annual (> 10 months) time step 
streamflow_daily           <- read.csv("Streamflow/Data_Streamflow_v10_daily.csv")
streamflow_daily_c         <- read.csv("Streamflow/Data_Streamflow_v10_daily.csv")
streamflow_endesa      <- read.csv("Streamflow/Data_Streamflow_Endesa.csv")
streamflow_endesa$Date <- as.Date(streamflow_endesa$Date)

Date <-seq(from = as.Date(min(streamflow_daily$Date)), to = as.Date(max(streamflow_daily$Date)), by = "month")
streamflow_daily_c[,2:length(streamflow_daily)][streamflow_daily_c[,2:length(streamflow_daily)]>-99 ]<- 1
streamflow_monthly_c <- data.frame(daily2monthly(streamflow_daily_c, FUN = sum, na.rm = TRUE))
streamflow_monthly   <- data.frame(daily2monthly(streamflow_daily,   FUN = mean, na.rm = TRUE))
streamflow_monthly   <- round(streamflow_monthly, 2)
streamflow_monthly[streamflow_monthly_c < 20] <- NA
streamflow_monthly   <- cbind(Date, streamflow_monthly)
streamflow_monthly[,59] <- ifelse(is.na(streamflow_monthly[,59]),streamflow_endesa[,2], streamflow_monthly[,59])
streamflow_monthly[,60] <- ifelse(is.na(streamflow_monthly[,60]),streamflow_endesa[,3], streamflow_monthly[,60])
write.csv(streamflow_monthly, "Streamflow/Data_Streamflow_v10_monthly.csv", row.names = F)

streamflow_monthly_c<-streamflow_monthly
Date <-seq(from = as.Date(min(streamflow_monthly$Date)), to = as.Date(max(streamflow_monthly$Date)), by = "year")
streamflow_annual_c  <- data.frame(monthly2annual(streamflow_monthly_c, FUN = sum, na.rm = TRUE))
streamflow_annual    <- data.frame(monthly2annual(streamflow_monthly,   FUN = mean, na.rm = TRUE))
streamflow_annual   <- round(streamflow_annual, 2)
streamflow_annual[streamflow_annual_c < 10] <- NA
streamflow_annual   <- cbind(Date, streamflow_annual)
write.csv(streamflow_annual, "Streamflow/Data_Streamflow_v10_annual.csv", row.names = F)

## Precipitation data: Daily to monthly (> 20 days) and annual (> 10 months) time step 
pp_daily   <-read.csv("Precipitation/Data_precipitation_v10_daily.csv")
pp_daily_c <-read.csv("Precipitation/Data_precipitation_v10_daily.csv")

pp_daily_c[,2:length(pp_daily)][pp_daily_c[,2:length(pp_daily)]>-99 ]<- 1
pp_monthly_c <- data.frame(daily2monthly(pp_daily_c, FUN = sum, na.rm = TRUE))
pp_monthly   <- data.frame(daily2monthly(pp_daily, FUN = sum, na.rm = TRUE))
pp_monthly[pp_monthly_c < 20] <- NA
write.csv(pp_monthly, "Precipitation/Data_precipitation_v10_monthly.csv")

pp_monthly_c<-pp_monthly
Date <-seq(from = as.Date(min(pp_daily$Date)), to = as.Date(max(pp_daily$Date)), by = "month")
pp_monthly_c[,2:length(pp_monthly)][pp_monthly_c[,2:length(pp_monthly)]>-99 ]<- 1
pp_annual_c  <- data.frame(monthly2annual(pp_monthly_c, FUN = sum, na.rm = TRUE))
pp_annual    <- data.frame(monthly2annual(pp_monthly, FUN = sum, na.rm = TRUE))
pp_annual[pp_annual_c < 11] <- NA
write.csv(pp_annual, "Precipitation/Data_precipitation_v10_annual.csv")

## Temperature data: Daily to monthly (> 20 days) and annual (> 10 months) time step
## Tdaily is the average between the maximum and minimum temperature
t2m_daily_max   <- read.csv("Temperature/Data_Temperature_max_v10_daily.csv")
t2m_daily_min   <- read.csv("Temperature/Data_Temperature_min_v10_daily.csv")
t2m_daily <- (t2m_daily_max[,2:length(t2m_daily_max)]+t2m_daily_min[,2:length(t2m_daily_max)])/2
t2m_daily <- cbind(t2m_daily_max$Date, t2m_daily)
write.csv(t2m_daily, "Temperature/Data_Temperature_mean_v10_daily.csv")

t2m_daily_c <-t2m_daily
Date <-seq(from = as.Date(min(t2m_daily$Date)), to = as.Date(max(t2m_daily$Date)), by = "month")
t2m_daily_c[,2:length(t2m_daily)][t2m_daily_c[,2:length(t2m_daily)]>-99 ]<- 1
t2m_monthly_c  <- data.frame(daily2monthly(t2m_daily_c, FUN = sum,  na.rm = TRUE))
t2m_monthly    <- data.frame(daily2monthly(t2m_daily,   FUN = mean, na.rm = TRUE))
t2m_monthly[t2m_monthly_c < 20]<-NA
t2m_monthly<-round(t2m_monthly,2)
write.csv(t2m_monthly, "Temperature/Data_Temperature_mean_v10_monthly.csv")

t2m_monthly_c<-t2m_monthly
t2m_monthly_c[,2:length(t2m_monthly)][t2m_monthly_c[,2:length(t2m_monthly)]>-99 ]<- 1
t2m_annual_c  <- data.frame(monthly2annual(t2m_monthly_c, FUN = sum,  na.rm = TRUE))
t2m_annual    <- data.frame(monthly2annual(t2m_monthly,   FUN = mean, na.rm = TRUE))
t2m_annual[t2m_annual_c < 11] <- NA
t2m_annual<-round(t2m_annual,2)
write.csv(t2m_annual, "Temperature/Data_temperature_mean_v10_annual.csv")


## Potential evapotranspiration data: Daily to monthly (> 20 days) and annual (> 10 months) time step
pet_daily   <-read.csv("Evapotranspiration/Data_Evapotranspiration_v10_daily.csv")
pet_daily_c <-read.csv("Evapotranspiration/Data_Evapotranspiration_v10_daily.csv")

Date <-seq(from = as.Date(min(pet_daily$Date)), to = as.Date(max(pet_daily$Date)), by = "month")
pet_daily_c[,2:length(pet_daily)][pet_daily_c[,2:length(pet_daily)]>-99 ]<- 1
pet_monthly_c <- data.frame(daily2monthly(pet_daily_c, FUN = sum, na.rm = TRUE))
pet_monthly   <- data.frame(daily2monthly(pet_daily,   FUN = sum, na.rm = TRUE))
pet_monthly[pet_monthly_c < 20]<-NA
pet_monthly<-round(pet_monthly,1)
write.csv(pet_monthly, "Evapotranspiration/Data_Evapotranspiration_v10_monthly.csv")

pet_monthly_c<-pet_monthly
pet_monthly_c[,2:length(pet_monthly)][pet_monthly_c[,2:length(pet_monthly)]>-99 ]<- 1
pet_annual_c <- data.frame(monthly2annual(pet_monthly_c, FUN = sum, na.rm = TRUE))
pet_annual   <- data.frame(monthly2annual(pet_monthly, FUN = sum, na.rm = TRUE))
pet_annual[pet_annual_c < 11]<-NA
pet_annual<-round(pet_annual, 1)
write.csv(pet_annual, "Evapotranspiration/Data_Evapotranspiration_v10_annual.csv")
