rm(list=ls())
cat("\014")  

library("hydroTSM")
library("readxl")

#Lake levels data
lake_daily<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Lake Levels/Data_Lakes_Levels_v10.xlsx", sheet = "data_daily", guess_max = 30000))
lake_daily_c<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Lake Levels/Data_Lakes_Levels_v10.xlsx", sheet = "data_daily", guess_max = 30000))

lake_daily_c[,2:19][lake_daily_c[,2:19]>-99 ]<- 1
lake_monthly_c<-daily2monthly(lake_daily_c, FUN = sum, na.rm = TRUE)
lake_monthly<-daily2monthly(lake_daily, FUN = mean, na.rm = TRUE)
lake_monthly[lake_monthly_c < 20]<-NA
lake_monthly<-round(lake_monthly,2)
write.csv(lake_monthly, "Data_Lake_Levels_m.csv")

#Streamflow data 
streamflow_daily<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/streamflow/Data_streamflow_v10.xlsx", sheet = "data_daily", guess_max = 30000))
streamflow_daily_c<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/streamflow/Data_streamflow_v10.xlsx", sheet = "data_daily", guess_max = 30000))

streamflow_daily_c[,2:84][streamflow_daily_c[,2:84]>-99 ]<- 1
streamflow_monthly_c<-daily2monthly(streamflow_daily_c, FUN = sum, na.rm = TRUE)
streamflow_monthly<-daily2monthly(streamflow_daily, FUN = mean, na.rm = TRUE)
streamflow_monthly[streamflow_monthly_c < 20]<-NA
streamflow_monthly<-round(streamflow_monthly,2)
write.csv(streamflow_monthly, "Data_Streamflow_m.csv")

#Precipitation data
precipitation_daily<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v10.xlsx", sheet = "data_daily", guess_max = 30000))
precipitation_daily_c<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v10.xlsx", sheet = "data_daily", guess_max = 30000))

precipitation_daily_c[,2:153][precipitation_daily_c[,2:153]>-99 ]<- 1
precipitation_monthly_c<-daily2monthly(precipitation_daily_c, FUN = sum, na.rm = TRUE)
precipitation_monthly<-daily2monthly(precipitation_daily, FUN = sum, na.rm = TRUE)
precipitation_monthly[precipitation_monthly_c < 20]<-NA
write.csv(precipitation_monthly, "Data_Precipitation_m.csv")

#Temperature data
temperature_daily<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/temperature/Data_temperature_v10.xlsx", sheet = "data_daily", guess_max = 30000))
temperature_daily_c<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/temperature/Data_temperature_v10.xlsx", sheet = "data_daily", guess_max = 30000))

temperature_daily_c[,2:length(temperature_daily)][temperature_daily_c[,2:length(temperature_daily)]>-99 ]<- 1
temperature_monthly_c<-daily2monthly(temperature_daily_c, FUN = sum, na.rm = TRUE)
temperature_monthly<-daily2monthly(temperature_daily, FUN = mean, na.rm = TRUE)
temperature_monthly[temperature_monthly_c < 20]<-NA
temperature_monthly<-round(temperature_monthly,2)
write.csv(temperature_monthly, "Data_Temperature_m.csv")

#Potential evapotranspiration data
pet_daily<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Evapotranspiration/Data_Evapotranspiration_v10.xlsx", sheet = "data_daily", guess_max = 30000))
pet_daily_c<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Evapotranspiration/Data_Evapotranspiration_v10.xlsx", sheet = "data_daily", guess_max = 30000))

pet_daily_c[,2:length(pet_daily)][pet_daily_c[,2:length(pet_daily)]>-99 ]<- 1
pet_monthly_c<-daily2monthly(pet_daily_c, FUN = sum, na.rm = TRUE)
pet_monthly<-daily2monthly(pet_daily, FUN = sum, na.rm = TRUE)
pet_monthly[pet_monthly_c < 25]<-NA
pet_monthly<-round(pet_monthly,1)
write.csv(pet_monthly, "Data_PET_m.csv")
