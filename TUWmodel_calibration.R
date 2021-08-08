rm(list=ls())
cat("\014")  

library("TUWmodel")
library("hydroGOF")
library("hydroPSO")
library("readxl")
library("raster")

q_obs<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/streamflow/Data_streamflow_v10.xlsx", sheet = "data_monthly"))
dirs<-list.dirs("C:/Users/rooda/Dropbox/Rstudio/TUWmodel", full.names = TRUE,recursive =FALSE)

areas<-read.csv("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/data_area.csv")
KGE<-data.frame(matrix(ncol = 6, nrow = 1))
colnames(KGE)<-c("Model", "Stage","KGE", "r", "Beta", "Gamma")
models<-c("CR2MET","ERA5d", "MSWEP", "PMET")

param = list(c("SCF", "DDF", "Tr", "Ts" ,  "Tm", "LPrat", "FC","BETA", "k0", "k1", "k2", "lsuz", "cperc", "bmax", "croute"),
             c(1,      1.6,   2.8, -2.5,   -0.5,     3,   1200,  2.8,  0.8,  3.3,    8,    20,    2,       2,       10))
  
for (j in 1:4) {
  for (i in 1:83) {
    if (areas$nbands[i] != 0){
      
      q_obsc_i<-subset(q_obs, Date > "1989-12-31" & Date < "2005-12-31")[,i+1]
      q_obsc_i<-q_obsc_i*1000*24*3600*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)/(areas$area[i]*10^6)
    
      q_obsv_i<-subset(q_obs, Date > "2005-12-31")[,i+1]
      q_obsv_i<-q_obsv_i*1000*24*3600*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)/(areas$area[i]*10^6)
      
      ok<-sum(is.na(q_obsc_i))/length(q_obsc_i)
      if (ok <= 0.333){
      
      pp_i<-paste0(dirs[j],"/PP/PP_gridcode_", sprintf("%03d", i), ".csv")
      t2m_i<-paste0(dirs[j],"/T2M/T2M_gridcode_", sprintf("%03d", i), ".csv")
      pet_i<-paste0(dirs[j],"/PET/PET_gridcode_", sprintf("%03d", i), ".csv")
      pp_i<-read.table(pp_i, sep = ",", row.names=1, header = TRUE)
      t2m_i<-read.table(t2m_i, sep = ",", row.names=1, header = TRUE)
      pet_i<-read.table(pet_i, sep = ",", row.names=1, header = TRUE)
    
      tuw_model_q<-TUWmodel(prec = as.matrix(pp_i), airt= as.matrix(t2m_i), ep =  as.matrix(pet_i), 
                            area= areas[i,4:(areas$nbands[i]+3)], param=param[[2]], incon=c(100,10,150,150), itsteps=NULL)
      
      KGE_cal_i<-KGE(sim=as.vector(tuw_model_q$q)[1:192], obs=q_obsc_i, method="2012", out.type="full",na.rm=TRUE)
      KGE_val_i<-KGE(sim=as.vector(tuw_model_q$q)[193:360], obs=q_obsv_i, method="2012", out.type="full",na.rm=TRUE)
      
      KGE<-rbind(KGE, c(models[j], "Calibration", KGE_cal_i$KGE.value, KGE_cal_i$KGE.elements))
      KGE<-rbind(KGE, c(models[j], "Validation", KGE_val_i$KGE.value, KGE_val_i$KGE.elements))
      }
      
      print(i)
      
    } 
    
  }
  
  }
  
write.csv(KGE, "C:/Users/rooda/Dropbox/Patagonia/MS1 Results/Q_performance.csv", row.names = FALSE)
  