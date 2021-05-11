rm(list=ls())
cat("\014")  

library("TUWmodel")
library("hydroGOF")
library("readxl")
library("raster")

q_obs<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/streamflow/Data_streamflow_v10.xlsx", sheet = "data_monthly"))
dirs<-list.dirs("C:/Users/rooda/Dropbox/Rstudio/TUWmodel", full.names = TRUE,recursive =FALSE)

areas<-read.csv("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/data_area.csv")
KGE<-matrix(0,length(basins_int),4)
colnames(KGE)<-c("KGE", "r", "Beta", "Gamma")

param = list(c("SCF", "DDF", "Tr", "Ts" ,  "Tm", "LPrat", "FC","BETA", "k0", "k1", "k2", "lsuz", "cperc", "bmax", "croute"),
             c(1,      1.6,   2.8, -2.5,   -0.5,     3,   1200,  2.8,  0.8,  3.3,    8,    20,    2,       2,       10))
  
for (j in 1:4) {
  for (i in 1:length(basins_int)) {
    if (areas$nbands[i] != 0){
      
      q_obs_i<-subset(q_obs, Date > "1989-12-31")[,i+1]
      q_obs_i<-q_obs_i*1000*24*3600*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)/(areas$area[i]*10^6)
    
      #Change the folder location if you need to change the meteorological forcing
      pp_i<-paste0(dirs[j],"/PP/PP_gridcode_", sprintf("%03d", i))
      t2m_i<-paste0(dirs[j],"/T2M/T2M_gridcode_", sprintf("%03d", i))
      pet_i<-paste0(dirs[j],"/PET/PET_gridcode_", sprintf("%03d", i))
      pp_i<-read.table(pp_i, sep = ",", row.names=1, header = TRUE)
      t2m_i<-read.table(t2m_i, sep = ",", row.names=1, header = TRUE)
      pet_i<-read.table(pet_i, sep = ",", row.names=1, header = TRUE)
    
      tuw_model_q<-TUWmodel(prec = as.matrix(pp_i), airt= as.matrix(t2m_i), ep =  as.matrix(pet_i), 
                            area= areas[i,4:(areas$nbands[i]+3)], param=param[[2]], incon=c(100,10,150,150), itsteps=NULL)
      
      if (areas$nbands[i] == 1){
      
      spin_off<-c(mean(matrix(tuw_model_q$moist, ncol = 12, byrow = TRUE)[5:30,1]), 
                  mean(matrix(tuw_model_q$swe, ncol = 12, byrow = TRUE)[5:30,1]), 
                  mean(matrix(tuw_model_q$suz, ncol = 12, byrow = TRUE)[5:30,1]), 
                  mean(matrix(tuw_model_q$slz, ncol = 12, byrow = TRUE)[5:30,1]))
      
      } else {
        
      spin_off<-c(mean(matrix(tuw_model_q$moist[,1], ncol = 12, byrow = TRUE)[5:30,1]), 
                    mean(matrix(tuw_model_q$swe[,1], ncol = 12, byrow = TRUE)[5:30,1]), 
                    mean(matrix(tuw_model_q$suz[,1], ncol = 12, byrow = TRUE)[5:30,1]), 
                    mean(matrix(tuw_model_q$slz[,1], ncol = 12, byrow = TRUE)[5:30,1]))
      }
      
      tuw_model_q<-TUWmodel(prec = as.matrix(pp_i), airt= as.matrix(t2m_i), ep =  as.matrix(pet_i), 
                            area= areas[i,4:(areas$nbands[i]+3)], param=param[[2]], incon=spin_off, itsteps=NULL)
      
      KGE_i<-KGE(sim=as.vector(tuw_model_q$q), obs=q_obs_i, method="2012", out.type="full",na.rm=TRUE)
      KGE[i,]<-c(KGE_i$KGE.value, KGE_i$KGE.elements)
      print(i)
      
    } else {
      print (i)
      
      }
    }
  print (j)
}
  
  write.csv(KGE, "Q_performance.csv")
