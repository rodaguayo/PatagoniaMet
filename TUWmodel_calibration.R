rm(list=ls())
cat("\014")  

library("TUWmodel")
library("hydroGOF")
library("hydroPSO")
library("hydroTSM")
setwd("/home/rooda/Dropbox/Patagonia/")

# Warm up, calibration and validation period
cal_period  <- c("1990-01-01", "2004-12-31")
val_period  <- c("2005-01-01", "2019-12-31")
dates       <- seq(as.Date("1987-01-01"), as.Date("2019-12-31"),   by ="day")
dates_m     <- seq(as.Date("1987-01-01"), as.Date("2019-12-31"), by ="month")
days        <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# General setting
q_obs <- read.csv("Data/Streamflow/Data_Streamflow_v10_monthly.csv")
q_obs <- subset(q_obs, as.Date(q_obs$Date) >= dates[1] & as.Date(q_obs$Date) <= val_period[2])[,-1]
areas <- read.csv("MS1 Results/TUWmodel/data_area.csv", row.names=1)
dirs  <- list.dirs("MS1 Results/TUWmodel", full.names = TRUE, recursive =FALSE)
dirs  <- c(dirs[2], dirs[3], dirs[1], dirs[4]) # for Figure 9

# Parameters: Lower and upper bound
lower_param        <-  c(1.0,    0.0,   2.0,  -2.0,  -3.0,     0.0,     0,    0.5,    0,    2,   30,      1,       0,      0,        0)
upper_param        <-  c(1.0,    6.0,   5.0,   2.0,   3.0,     1.0,   600,      2,    2,   30,  180,    100,      10,     30,       50)
names(upper_param) <-  c("SCF", "DDF", "Tr",  "Ts",  "Tm", "LPrat",  "FC", "Beta", "k0", "k1", "k2", "lsuz", "cperc", "bmax", "croute")
names(lower_param) <-  names(upper_param) 

# Performance metric (KGE 2012)
KGE_params <- data.frame(matrix(ncol = 7+length(upper_param), nrow = 0))

# Evaluating the hydrological model using the parameter set
TUWhydromod <- function(param.values, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_i, obs=q_obs_i) {
  
  PP     <- subset(PP,  dates <= cal_period[2])
  T2M    <- subset(T2M, dates <= cal_period[2])
  PET    <- subset(PET, dates <= cal_period[2])
  
  tuwsim <- TUWmodel(param = param.values, prec = PP, airt = T2M, ep = PET, area = AREA)
  tuwsim <- zoo(as.numeric(tuwsim$q), dates[dates <= cal_period[2]])
  tuwsim <- as.numeric(daily2monthly(tuwsim,   FUN = sum, na.rm = TRUE))
  
  obs    <- subset(obs,    dates_m >= cal_period[1] & dates_m < cal_period[2])
  q_sim  <- subset(tuwsim, dates_m >= cal_period[1] & dates_m < cal_period[2])
  gof    <- KGE(sim=q_sim, obs=obs, method="2012", na.rm=TRUE)
  out     <- list("GoF" = gof, "sim" = q_sim)
  return(out)
  }

for (model in 1:4) { #number of scenarios
  for (basin in 1:(length(q_obs))) { #total number of basins
    if (areas$nbands[basin] > 0){ 
      
      q_obs_i <- q_obs[,basin]*1000*86400*days/(areas$area[basin]*10^6)
      pp_i    <- paste0(dirs[model],"/PP/PP_gridcode_",   sprintf("%03d", basin), ".csv")
      t2m_i   <- paste0(dirs[model],"/T2M/T2M_gridcode_", sprintf("%03d", basin), ".csv")
      pet_i   <- paste0(dirs[model],"/PET/PET_gridcode_", sprintf("%03d", basin), ".csv")
      pp_i    <- as.matrix(read.csv(pp_i,  sep = ",", row.names=1, header = TRUE))
      t2m_i   <- as.matrix(read.csv(t2m_i, sep = ",", row.names=1, header = TRUE))
      pet_i   <- as.matrix(read.csv(pet_i, sep = ",", row.names=1, header = TRUE))
      area_i  <- rep(1/areas$nbands[basin], areas$nbands[basin])
      
      out <- hydroPSO(fn="hydromodInR", lower=lower_param,  upper=upper_param, method="spso2011", model.FUN="TUWhydromod", 
                      control = list(write2disk=TRUE, MinMax="max", npart=60, maxit=100, normalise=TRUE, REPORT=10, reltol=1E-6, 
                                     parallel = "parallel", par.nnodes = 20), model.FUN.args= list(obs=q_obs_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_i))
      
      tuw_model_i <- TUWmodel(prec = pp_i, airt = t2m_i, ep = pet_i, area = area_i, param=out$par)
      tuw_model_i <- zoo(as.numeric(tuw_model_i$q), dates)
      tuw_model_i <- as.numeric(daily2monthly(tuw_model_i, FUN = sum, na.rm = TRUE))
      
      q_obs_is <- subset(q_obs_i,     dates_m >= cal_period[1] & dates_m < cal_period[2])
      q_sim_is <- subset(tuw_model_i, dates_m >= cal_period[1] & dates_m < cal_period[2])
      
      KGE_i <- KGE(sim = q_sim_is, obs = q_obs_is, method = "2012", out.type = "full", na.rm=TRUE)
      KGE_i <- as.numeric(c(KGE_i$KGE.value, KGE_i$KGE.elements)) 
      KGE_i <- c(colnames(q_obs)[basin], basename(dirs)[model], "Calibration", KGE_i, as.numeric(out$par))
      KGE_params <- rbind(KGE_params, KGE_i)
      
      q_obs_is <- subset(q_obs_i,     dates_m >= val_period[1] & dates_m < val_period[2])
      q_sim_is <- subset(tuw_model_i, dates_m >= val_period[1] & dates_m < val_period[2])
      
      KGE_i <- KGE(sim = q_sim_is, obs = q_obs_is, method = "2012", out.type = "full", na.rm=TRUE)
      KGE_i <- as.numeric(c(KGE_i$KGE.value, KGE_i$KGE.elements)) 
      KGE_i <- c(colnames(q_obs)[basin], basename(dirs)[model], "Validation", KGE_i, as.numeric(out$par))
      KGE_params <- rbind(KGE_params, KGE_i)

      plot.zoo(cbind(q_obs_i, tuw_model_i), type = "l", plot.type = "single", col = c("red", "black"),
               main = paste0(colnames(q_obs)[basin],": ", round(as.numeric(KGE_i[4]),3)))
      }
    print(paste0(basename(dirs)[model]," - ", basin))
    }
}

colnames(KGE_params) <- c("Name", "Model", "Stage","KGE", "r", "Beta", "Gamma", names(upper_param))
write.csv(KGE_params, "MS1 Results/Q_performance.csv", row.names = FALSE)
