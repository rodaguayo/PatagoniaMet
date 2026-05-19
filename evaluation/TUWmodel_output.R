# TUWmodel outputs ---------------------------------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2023)

rm(list=ls())
cat("\014")  

library("TUWmodel")
library("hydroGOF")
library("hydroTSM")

setwd("/home/rooda/Dropbox/Patagonia/")
pmet_dir <- "MS1 Results/TUWmodel/PMET"

# Warm up, calibration and validation period
cal_period  <- c("1990-01-01", "2004-12-31")
val_period  <- c("2005-01-01", "2019-12-31")
dates       <- seq(as.Date("1987-01-01"), as.Date("2019-12-31"), by ="day")
dates_m     <- seq(as.Date("1987-01-01"), as.Date("2019-12-31"), by ="month")
days        <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# General setting
q_obs <- read.csv("Data/Streamflow/Q_PMETobs_v10m.csv")
q_obs <- subset(q_obs, as.Date(q_obs$Date) >= dates[1] & as.Date(q_obs$Date) <= val_period[2])[,-1]
areas <- read.csv("MS1 Results/TUWmodel/data_area.csv", row.names=1)

# Parameters obtained from the calibration process
KGE_params <- read.csv("MS1 Results/Q_performance.csv")
KGE_params <- subset(KGE_params, KGE_params$Model == "PMET" & KGE_params$Stage == "Calibration")
colnames(KGE_params)[1] <- "gauge_id"

q_water   <- read.csv("Data/Streamflow/Q_PMETobs_v10_metadata.csv")
q_water   <- q_water[,c("gauge_id", "gauge_code", "gauge_name", "gauge_lat", "gauge_lon", "gauge_alt", "institution")]
q_water   <- merge(q_water, KGE_params[,c("gauge_id", "KGE", "r", "Beta", "Gamma")], all = F)

df_output <- data.frame(matrix(nrow = 0, ncol = 0)) 

for (basin in colnames(q_obs)) { # total number of basins (length(q_obs))
  if (areas[basin,]$nbands > 0){ 
    
    q_obs_i <- q_obs[basin][,1]*1000*86400*days/(areas[basin,]$area*10^6)
    pp_i    <- paste0(pmet_dir,"/PP/PP_",   basin, ".csv")
    t2m_i   <- paste0(pmet_dir,"/T2M/T2M_",   basin, ".csv")
    pet_i   <- paste0(pmet_dir,"/PET/PET_",   basin, ".csv")
    pp_i    <- as.matrix(read.csv(pp_i,  sep = ",", row.names=1, header = TRUE))
    t2m_i   <- as.matrix(read.csv(t2m_i, sep = ",", row.names=1, header = TRUE))
    pet_i   <- as.matrix(read.csv(pet_i, sep = ",", row.names=1, header = TRUE))
    pet_i[is.na(pet_i)] <- 0 # less than 10 values (out of 5000)
    area_i  <- rep(1/areas[basin,]$nbands, areas[basin,]$nbands)
    param_i <- as.numeric(KGE_params[KGE_params$gauge_id == basin,][8:22])
    
    tuw_model_i <- TUWmodel(prec = pp_i, airt = t2m_i, ep = pet_i, area = area_i, param=param_i)
    
    if (areas[basin,]$nbands > 1){  
      pp_i <- apply(tuw_model_i$prec, 1, weighted.mean, w=area_i)
      eta_i <- apply(tuw_model_i$eta, 1, weighted.mean, w=area_i)
      } 
      
    pp_i <- zoo(as.numeric(pp_i), dates)
    pp_i <- mean(as.numeric(daily2annual(pp_i, FUN = sum, na.rm = TRUE)))
    
    eta_i <- zoo(as.numeric(eta_i), dates)
    eta_i <- mean(as.numeric(daily2annual(eta_i, FUN = sum, na.rm = TRUE)))
    
    q_i <- zoo(as.numeric(tuw_model_i$q), dates)
    q_i <- mean(as.numeric(daily2annual(q_i, FUN = sum, na.rm = TRUE)))
    
    df_output <- rbind(df_output, c(basin, pp_i, eta_i, q_i))
    
    
    print(basin)
  }
}

colnames(df_output) <- c("gauge_id", "PP", "E", "R")
q_water   <- merge(q_water, df_output, all = T)
q_water[c("PP", "E", "R")][q_water$KGE < 0.5,] <- NA
write.csv(q_water, "Data/Streamflow/Q_PMETobs_v10_water_balance.csv")


