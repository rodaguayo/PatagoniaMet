rm(list=ls())
cat("\014")  

library("TUWmodel")
library("hydroGOF")
library("hydroPSO")
library("readxl")
library("raster")

#General setting
q_obs<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/streamflow/Data_streamflow_v10.xlsx", sheet = "data_monthly"))
dirs<-list.dirs("C:/Users/rooda/Dropbox/Rstudio/TUWmodel", full.names = TRUE, recursive =FALSE)[1:4]
areas<-read.csv("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/data_area.csv")

#Perfomance metric (KGE 2012)
KGE<-data.frame(matrix(ncol = 6, nrow = 1))
colnames(KGE)<-c("Model", "Stage","KGE", "r", "Beta", "Gamma")

#Parameters: Lower and upper bound
lower_param        <-  c(1.0,    0.0,   1.0,  -5.0, -2.0,   0.0,      0,     0,    0,    2,    2,      1,     0,      0,      0)
upper_param        <-  c(1.0,    5.0,   5.0,   1.0,  2.0,   5.0,   2000,    20,    2,   30,   20,    100,     8,     30,      50)
ave_params         <-  c(1.0,    1.6,   2.8,  -2.5, -0.5,     3,   1200,   2.8,  0.8,  3.3,    8,     20,     2,     2,       10)
names(upper_param) <-  c("SCF", "DDF", "Tr",  "Ts", "Tm", "LPrat", "FC", "Beta", "k0", "k1", "k2", "lsuz", "cperc", "bmax", "croute")
names(lower_param) <-  c("SCF", "DDF", "Tr",  "Ts", "Tm", "LPrat", "FC", "Beta", "k0", "k1", "k2", "lsuz", "cperc", "bmax", "croute")
names(ave_params)  <-  c("SCF", "DDF", "Tr",  "Ts", "Tm", "LPrat", "FC", "Beta", "k0", "k1", "k2", "lsuz", "cperc", "bmax", "croute")

#Warm up, calibration and validation period 
cal_period <- c("1991-12-31", "2005-12-31")
val_period <- c("2006-01-01", "2019-12-31")
dates<-seq(as.Date("1990-01-01"), as.Date("2019-12-31"), by ="month")

# Evaluating the hydrological model at the param. values parameter set
TUWhydromod <- function(param.values, obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2) {

  simLump   <- TUWmodel::TUWmodel(param=param.values, prec=as.matrix(PP), airt=as.matrix(T2M), ep=as.matrix(PET), area=AREA, incon=c(250,200,250,250))
  q_sim     <- subset(as.numeric(simLump$q), dates >= cal_period[1] & dates < cal_period[2])
  
  gof       <- KGE(sim=q_sim, obs=obs, method="2012", na.rm=TRUE) #Computing the goodness-of-fit value (i.e, model performance) 
  
  out       <- vector("list", 2)   # Creating the output of the R function
  out[[1]]  <- gof
  out[[2]]  <- q_sim
  
  names(out) <- c("GoF", "sim")  # Mandatory names for the elements of the output
  return(out)
  }

hola<-TUWhydromod(ave_params, obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2)


for (j in 1:4) {
  for (i in 1:83) {
    if (areas$nbands[i] != 0){
      
      q_obsc_i<-subset(q_obs, Date >= cal_period[1] & Date < cal_period[2])[,i+1]
      q_obsc_i<-q_obsc_i*1000*24*3600*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)/(areas$area[i]*10^6)
    
      q_obsv_i<-subset(q_obs, Date > cal_period[2])[,i+1]
      q_obsv_i<-q_obsv_i*1000*24*3600*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)/(areas$area[i]*10^6)
      
      pp_i<-paste0(dirs[j],"/PP/PP_gridcode_", sprintf("%03d", i), ".csv")
      t2m_i<-paste0(dirs[j],"/T2M/T2M_gridcode_", sprintf("%03d", i), ".csv")
      pet_i<-paste0(dirs[j],"/PET/PET_gridcode_", sprintf("%03d", i), ".csv")
      pp_i<-read.table(pp_i, sep = ",", row.names=1, header = TRUE)
      t2m_i<-read.table(t2m_i, sep = ",", row.names=1, header = TRUE)
      pet_i<-read.table(pet_i, sep = ",", row.names=1, header = TRUE)
      area_m2<-areas[i,4:(areas$nbands[i]+3)]
      
      #tuw_model_q<-TUWmodel(prec = as.matrix(pp_i), airt= as.matrix(t2m_i), ep =  as.matrix(pet_i), 
                           # area= area_m2, param=ave_params, incon=c(100,10,150,150), itsteps=NULL)
      
      out <- hydroPSO(fn="hydromodInR", lower=lower_param,  upper=upper_param, method="spso2011", model.FUN="TUWhydromod",
                control=list(write2disk=TRUE, MinMax="max", npart=30, maxit=50, normalise=TRUE, REPORT=1, reltol=0.001),
                model.FUN.args= list(obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2))

      tuw_model_q<-TUWmodel(prec = as.matrix(pp_i), airt= as.matrix(t2m_i), ep =  as.matrix(pet_i), 
                            area= area_m2, param=out$par, itsteps=NULL)
      
      q_sim_cal     <- subset(as.numeric(tuw_model_q$q), dates >= cal_period[1] & dates < cal_period[2])
      q_sim_val     <- subset(as.numeric(tuw_model_q$q), dates > cal_period[2])
      
      KGE_cal_i<-KGE(sim=as.vector(q_sim_cal), obs=q_obsc_i, method="2012", out.type="full",na.rm=TRUE)
      KGE_val_i<-KGE(sim=as.vector(q_sim_val), obs=q_obsv_i, method="2012", out.type="full",na.rm=TRUE)
      
      KGE<-rbind(KGE, c(basename(dirs)[j], "Calibration", KGE_cal_i$KGE.value, KGE_cal_i$KGE.elements))
      KGE<-rbind(KGE, c(basename(dirs)[j], "Validation", KGE_val_i$KGE.value, KGE_val_i$KGE.elements))
      
      print(i)
      
    } 
    
  }
  
  }
  
write.csv(KGE, "C:/Users/rooda/Dropbox/Patagonia/MS1 Results/Q_performance.csv", row.names = FALSE)





