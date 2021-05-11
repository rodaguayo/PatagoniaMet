rm(list=ls())
cat("\014")  

library("xts")
library("raster")
library("readxl")
library("hydroTSM")
library("changepoint")

#Daily air temperature outliers (Tmax > Tmin already ok!)
t2m_obs<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/Data_Temperature_v08.xlsx", sheet = "data_daily", guess_max = 30000)
t2m_mean<-t(monthlyfunction(xts(t2m_obs[,2:length(t2m_obs)], order.by = t2m_obs$Date), FUN = mean, na.rm = TRUE))
t2m_sd<-t(monthlyfunction(xts(t2m_obs[,2:length(t2m_obs)], order.by = t2m_obs$Date), FUN = sd, na.rm = TRUE))
t2m_anno<-t2m_obs

upper = 3*t2m_sd + t2m_mean
lower = t2m_mean - 3*t2m_sd

for(j in 1:12) {
  for(i in 2:length(t2m_obs)) {
    t2m_anno[which(as.numeric(format(t2m_anno$Date, "%m"))==j),][,i]<- 
      replace(t2m_anno[which(as.numeric(format(t2m_anno$Date, "%m"))==j),][,i], 
          t2m_anno[which(as.numeric(format(t2m_anno$Date, "%m"))==j),][,i] > upper[j,i-1] | 
            t2m_anno[which(as.numeric(format(t2m_anno$Date, "%m"))==j),][,i] < lower[j,i-1], NA)
  }
}

write.csv(t2m_anno, "Data_Temperature_v09.csv")

delete<-vector(0)
for(j in 2:99) {delete[j-1]<-sum(table(t2m_obs[,j]))-sum(table(t2m_anno[,j]))}
names(delete)<-colnames(t2m_obs)[2:length(t2m_obs)]

#Monthly temperature outliers and stationarity test 
t2m_shape<-shapefile("C:/Users/rooda/Dropbox/Patagonia/GIS South/Temperature_v10.shp")
t2m_obs<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/Data_temperature_v10.xlsx", sheet = "data_monthly", guess_max = 30000)
t2m_obs$Date <- NULL

t2m_era5<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_ERA5_1950_2019.nc", varname = "tas")
t2m_sim_era5<-as.data.frame(t(raster::extract(t2m_era5,t2m_shape, method='simple')))
colnames(t2m_sim_era5) <- t2m_shape$Name

t2m_obs2<-t2m_obs
t2m_test<-matrix(0,length(t2m_obs),3, dimnames= list(names(t2m_obs), c("cp_mean", "cp_var", "value>3sd")))

FindOutliers <- function(data,sd) {
  mean_data <- mean(data, na.rm = TRUE)
  sd_data <- sd(data, na.rm = TRUE)
  upper = sd*sd_data + mean_data
  lower = mean_data - sd*sd_data
  replace(data, data > upper | data < lower, NA)
}

for(i in 1:length(t2m_obs)) {
  print(i)
  t2m_bias<-t2m_obs[[i]]-t2m_sim_era5[[i]]
  t2m_obs2[[i]]<-FindOutliers(t2m_bias,4)
  
  #Stationarity test 
  cp_test_mean<-cpt.mean(na.omit(t2m_bias), method="PELT", penalty="MBIC", minseglen=12)
  cp_test_var<-cpt.var(na.omit(t2m_bias), method="PELT", penalty="MBIC", minseglen=12)

  t2m_test[i,1]<-ncpts(cp_test_mean)
  t2m_test[i,2]<-ncpts(cp_test_var)
  t2m_test[i,3]<-sum(table(t2m_obs[[i]]))-sum(table(t2m_obs2[[i]]))
}

write.xlsx(t2m_test, "t2m_test.xlsx")
write.xlsx(t2m_sim_era5-t2m_obs, "t2m_bias.xlsx")
write.xlsx(t2m_obs2, "t2m_corrected.xlsx")
