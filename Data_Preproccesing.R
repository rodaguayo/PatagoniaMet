rm(list=ls())
cat("\014")  

library("hydroTSM")
library("readxl")

## Lake levels data

# Monthly time step (> 20 days)
lake_daily<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Lake Levels/Data_Lakes_Levels_v10.xlsx", sheet = "data_daily", guess_max = 30000))
lake_daily_c<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Lake Levels/Data_Lakes_Levels_v10.xlsx", sheet = "data_daily", guess_max = 30000))

lake_daily_c[,2:length(lake_daily)][lake_daily_c[,2:length(lake_daily)]>-99 ]<- 1
lake_monthly_c<-daily2monthly(lake_daily_c, FUN = sum, na.rm = TRUE)
lake_monthly<-daily2monthly(lake_daily, FUN = mean, na.rm = TRUE)
lake_monthly[lake_monthly_c < 20]<-NA
lake_monthly<-round(lake_monthly,2)
write.csv(lake_monthly, "C:/Users/rooda/Dropbox/Rstudio/Data_Lake_Levels_m.csv")

## Streamflow data 

# Monthly time step (> 20 days)
streamflow_daily<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/streamflow/Data_streamflow_v10.xlsx", sheet = "data_daily", guess_max = 30000))
streamflow_daily_c<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/streamflow/Data_streamflow_v10.xlsx", sheet = "data_daily", guess_max = 30000))

streamflow_daily_c[,2:length(streamflow_daily)][streamflow_daily_c[,2:length(streamflow_daily)]>-99 ]<- 1
streamflow_monthly_c<-daily2monthly(streamflow_daily_c, FUN = sum, na.rm = TRUE)
streamflow_monthly<-daily2monthly(streamflow_daily, FUN = mean, na.rm = TRUE)
streamflow_monthly[streamflow_monthly_c < 20]<-NA
streamflow_monthly<-round(streamflow_monthly,2)
write.csv(streamflow_monthly, "C:/Users/rooda/Dropbox/Rstudio/Data_Streamflow_m.csv")


## Precipitation data

# Monthly time step (> 20 days)
pp_daily   <-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v10.xlsx", sheet = "data_daily", guess_max = 30000))
pp_daily_c <-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v10.xlsx", sheet = "data_daily", guess_max = 30000))

pp_daily_c[,2:length(pp_daily)][pp_daily_c[,2:length(pp_daily)]>-99 ]<- 1
pp_monthly_c <- daily2monthly(pp_daily_c, FUN = sum, na.rm = TRUE)
pp_monthly   <- daily2monthly(pp_daily, FUN = sum, na.rm = TRUE)
pp_monthly[pp_monthly_c < 20] <- NA
write.csv(pp_monthly, "C:/Users/rooda/Dropbox/Rstudio/Data_Precipitation_m.csv")

# Yearly time step (> 10 months)
pp_monthly<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v10.xlsx", sheet = "data_monthly", guess_max = 30000))
pp_monthly_c<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v10.xlsx", sheet = "data_monthly", guess_max = 30000))

pp_monthly_c[,2:length(pp_monthly)][pp_monthly_c[,2:length(pp_monthly)]>-99 ]<- 1
pp_annual_c  <- data.frame(monthly2annual(pp_monthly_c, FUN = sum, na.rm = TRUE))
pp_annual   <- data.frame(monthly2annual(pp_monthly, FUN = sum, na.rm = TRUE))
pp_annual[pp_annual_c < 11] <- NA
write.csv(pp_annual, "C:/Users/rooda/Dropbox/Rstudio/Data_Precipitation_y.csv")


## Temperature data

# Monthly time step (> 20 days)
t2m_daily<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/temperature/Data_temperature_v10.xlsx", sheet = "data_daily", guess_max = 30000))
t2m_daily_c<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/temperature/Data_temperature_v10.xlsx", sheet = "data_daily", guess_max = 30000))

t2m_daily_c[,2:length(t2m_daily)][t2m_daily_c[,2:length(t2m_daily)]>-99 ]<- 1
t2m_monthly_c<-daily2monthly(t2m_daily_c, FUN = sum, na.rm = TRUE)
t2m_monthly<-daily2monthly(t2m_daily, FUN = mean, na.rm = TRUE)
t2m_monthly[t2m_monthly_c < 20]<-NA
t2m_monthly<-round(t2m_monthly,2)
write.csv(t2m_monthly, "C:/Users/rooda/Dropbox/Rstudio/Data_Temperature_m.csv")

# Yearly time step (> 10 months)
t2m_monthly<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/temperature/Data_temperature_v10.xlsx", sheet = "data_monthly", guess_max = 30000))
t2m_monthly_c<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/temperature/Data_temperature_v10.xlsx", sheet = "data_monthly", guess_max = 30000))

t2m_monthly_c[,2:length(t2m_monthly)][t2m_monthly_c[,2:length(t2m_monthly)]>-99 ]<- 1
t2m_annual_c  <- data.frame(monthly2annual(t2m_monthly_c, FUN = sum, na.rm = TRUE))
t2m_annual   <- data.frame(monthly2annual(t2m_monthly, FUN = mean, na.rm = TRUE))
t2m_annual[t2m_annual_c < 11] <- NA
t2m_annual<-round(t2m_annual,2)
write.csv(t2m_annual, "C:/Users/rooda/Dropbox/Rstudio/Data_Temperature_y.csv")


## Potential evapotranspiration data

# Monthly time step (> 20 days)
pet_daily<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Evapotranspiration/Data_Evapotranspiration_v10.xlsx", sheet = "data_daily", guess_max = 30000))
pet_daily_c<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Evapotranspiration/Data_Evapotranspiration_v10.xlsx", sheet = "data_daily", guess_max = 30000))

pet_daily_c[,2:length(pet_daily)][pet_daily_c[,2:length(pet_daily)]>-99 ]<- 1
pet_monthly_c<-daily2monthly(pet_daily_c, FUN = sum, na.rm = TRUE)
pet_monthly<-daily2monthly(pet_daily, FUN = sum, na.rm = TRUE)
pet_monthly[pet_monthly_c < 25]<-NA
pet_monthly<-round(pet_monthly,1)
write.csv(pet_monthly, "C:/Users/rooda/Dropbox/Rstudio/Data_PET_m.csv")

# Yearly time step (> 10 months)
pet_monthly<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Evapotranspiration/Data_Evapotranspiration_v10.xlsx", sheet = "data_monthly", guess_max = 30000))
pet_monthly_c<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Evapotranspiration/Data_Evapotranspiration_v10.xlsx", sheet = "data_monthly", guess_max = 30000))

pet_monthly_c[,2:length(pet_monthly)][pet_monthly_c[,2:length(pet_monthly)]>-99 ]<- 1
pet_annual_c <- data.frame(monthly2annual(pet_monthly_c, FUN = sum, na.rm = TRUE))
pet_annual   <- data.frame(monthly2annual(pet_monthly, FUN = sum, na.rm = TRUE))
pet_annual[pet_annual_c < 11]<-NA
pet_annual<-round(pet_annual, 1)
write.csv(pet_annual, "C:/Users/rooda/Dropbox/Rstudio/Data_PET_y.csv")
