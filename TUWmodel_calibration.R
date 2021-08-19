rm(list=ls())
cat("\014")  

library("TUWmodel")
library("hydroGOF")
library("hydroPSO")
library("readxl")
library("raster")

#Warm up, calibration and validation period 
cal_period <- c("1992-12-31", "2006-12-31")
val_period <- c("2007-01-01", "2019-12-31")
dates<-seq(as.Date("1990-01-01"), as.Date("2019-12-31"), by ="month")

#General setting
q_obs<-as.data.frame(read_xlsx("/home/rooda/Dropbox/Patagonia/Data/Streamflow/Data_Streamflow_v10.xlsx", sheet = "data_monthly"))
q_obs<-subset(q_obs, Date >= "1989-12-31")
areas<-read.csv("/home/rooda/Dropbox/Rstudio/TUWmodel/data_area.csv")
dirs<-list.dirs("/home/rooda/Dropbox/Rstudio/TUWmodel", full.names = TRUE, recursive =FALSE)[1:4]

#Parameters: Lower and upper bound
names_param        <-  c("SCF", "DDF", "Tr",  "Ts", "Tm", "LPrat",  "FC","Beta", "k0", "k1", "k2", "lsuz", "cperc", "bmax", "croute")
lower_param        <-  c(1.0,    3.0,   1.0,  -5.0, -2.0,   0.1,      10,     0,    0,  1.5,    6,      1,     0,      0,       10)
upper_param        <-  c(1.0,     40,   6.0,   1.0,  2.0,   1.0,    3000,    20,  1.5,    6,   18,     50,     8,     30,       40)
ave_params         <-  c(1.0,     20,   3.0,  -2.5,  0.0,   0.6,    1000,    10,  1.0,    3,   14,     25,     4,     15,       25)
names(upper_param) <-  names_param
names(lower_param) <-  names_param
names(ave_params)  <-  names_param

#Perfomance metric (KGE 2012)
KGE_params<-data.frame(matrix(ncol = 7+15, nrow = 55*4))
colnames(KGE_params)<-c("Name", "Model", "Stage","KGE", "r", "Beta", "Gamma", names_param)
seq<-seq(1,55*2,2)
q_sim_final<-q_obs

# Evaluating the hydrological model using the parameter set
TUWhydromod <- function(param.values, obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2, INITIAL= t0_cond) {

  simLump   <- TUWmodel::TUWmodel(param=param.values, prec=as.matrix(PP), airt=as.matrix(T2M), ep=as.matrix(PET), area=AREA, incon=INITIAL)
  q_sim     <- subset(as.numeric(simLump$q), dates >= cal_period[1] & dates < cal_period[2])
  gof       <- KGE(sim=q_sim, obs=obs, method="2012", na.rm=TRUE)
  
  out       <- vector("list", 2)   # Creating the output of the R function
  out[[1]]  <- gof
  out[[2]]  <- q_sim
  names(out) <- c("GoF", "sim")  # Mandatory names for the elements of the output
  
  return(out)}

#test<-TUWhydromod(param.values = ave_params, obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2, INITIAL = t0_cond)
KGE_params<-read.csv(file)

for (j in 1:4) {
  for (i in 1:83) {
    if (areas$nbands[i] != 0){
      
      q_obs_i<-q_obs[,i+1]*1000*24*3600*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)/(areas$area[i]*10^6)
      q_obsc_i<-subset(q_obs_i, dates >= cal_period[1] & dates < cal_period[2])
      q_obsv_i<-subset(q_obs_i, dates > cal_period[2])
      
      pp_i<-paste0(dirs[j],"/PP/PP_gridcode_", sprintf("%03d", i), ".csv")
      t2m_i<-paste0(dirs[j],"/T2M/T2M_gridcode_", sprintf("%03d", i), ".csv")
      pet_i<-paste0(dirs[j],"/PET/PET_gridcode_", sprintf("%03d", i), ".csv")
      pp_i<-read.table(pp_i, sep = ",", row.names=1, header = TRUE)
      t2m_i<-read.table(t2m_i, sep = ",", row.names=1, header = TRUE)
      pet_i<-read.table(pet_i, sep = ",", row.names=1, header = TRUE)
      area_m2<-areas[i,4:(areas$nbands[i]+3)]
      t0_cond   <- 1.5*matrix(mean(q_obs_i[seq(1, length(q_obs_i), 12)], na.rm = TRUE),4,areas$nbands[i])
      
      #out <- hydroPSO(fn="hydromodInR", lower=lower_param,  upper=upper_param, method="spso2007", model.FUN="TUWhydromod",
      #          control=list(write2disk=FALSE, MinMax="max", maxit=200, normalise=TRUE, REPORT=5, reltol= 1E-6, parallel = "parallel"),
      #          model.FUN.args= list(obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2, INITIAL= t0_cond))

      out <- hydroPSO(fn="hydromodInR", lower=lower_param,  upper=upper_param, method="spso2011", model.FUN="TUWhydromod",
                      control=list(write2disk=FALSE, MinMax="max", npart=100, maxit=100, normalise=TRUE, REPORT=5, reltol=1E-6, parallel = "parallel"),
                      model.FUN.args= list(obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2, INITIAL= t0_cond))
      
      tuw_model_q<-TUWmodel(prec = as.matrix(pp_i), airt= as.matrix(t2m_i), ep =  as.matrix(pet_i), 
                            area= area_m2, param=out$par, itsteps=NULL, incon=t0_cond)
      
      q_sim_final[,i+1] <- as.numeric(tuw_model_q$q)
      q_sim_cal         <- subset(as.numeric(tuw_model_q$q), dates >= cal_period[1] & dates < cal_period[2])
      q_sim_val         <- subset(as.numeric(tuw_model_q$q), dates > cal_period[2])
      
      KGE_cal_i<-KGE(sim=as.vector(q_sim_cal), obs=q_obsc_i, method="2012", out.type="full", na.rm=TRUE)
      KGE_val_i<-KGE(sim=as.vector(q_sim_val), obs=q_obsv_i, method="2012", out.type="full", na.rm=TRUE)
      
      index<-sum(areas$nbands[1:i]>0)
      KGE_params[seq[index]+110*(j-1),]  <-c(colnames(q_obs)[i+1],colnames(q_obsc_i),basename(dirs)[j], "Calibration", KGE_cal_i$KGE.value, KGE_cal_i$KGE.elements, out$par)
      KGE_params[seq[index]+110*(j-1)+1,]<-c(colnames(q_obs)[i+1],colnames(q_obsv_i),basename(dirs)[j], "Validation",  KGE_val_i$KGE.value, KGE_val_i$KGE.elements, out$par)
      
      plot.zoo(cbind(q_obs_i, q_sim_final[,i+1]), type = "l", plot.type = "single", col = c("blue", "black"), 
               main = paste0(colnames(q_obs)[i+1],": ", round(KGE_cal_i$KGE.value,3)," - " , round(KGE_val_i$KGE.value,3)))
      }
    print(paste0(j," - ",i))
    print(out$par)
    }
}

file<-"/home/rooda/Dropbox/Patagonia/MS1 Results/Q_performance.csv"
ifelse(file.exists(file), file.rename(file, "/home/rooda/Dropbox/Patagonia/MS1 Results/Q_performance_old.csv"))
write.csv(KGE_params, file, row.names = FALSE)
