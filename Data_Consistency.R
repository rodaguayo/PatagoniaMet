rm(list=ls())
cat("\014")  

library("raster")
library("hydroTSM")
library("reddPrec")
library("xlsx")
library("readxl")
library("lmtest")
library("tseries")
library("fUnitRoots")
library("changepoint")

#Precipitation anomalies (daily)
{
pp_obs<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v08.xlsx", sheet = "data_daily", guess_max = 30000)
pp_data<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v08.xlsx", sheet = "info")
pp_obs<-10*pp_obs[,2:ncol(pp_obs)]

qcPrec(prec=pp_obs,sts=pp_data,inidate=as.Date('1950-01-01'),enddate=as.Date('2019-12-31'),
                     parallel=TRUE,ncpu=8,printmeta=TRUE,thres=125)

txt_files_ls <- list.files(path="C:/Users/rooda/Dropbox/Rstudio/meta", pattern="*.txt", full.names = TRUE) 
txt_files_df <- lapply(txt_files_ls, function(x) {read.table(file = x, header = T, sep ="")})
combined_df <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 

write.csv(combined_df, "pp_qc125.csv")
write.csv(prec/10, "prec.csv")
}

#Temperature anomalies (daily)
{
t2m_obs<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/Data_Temperature_v08.xlsx", sheet = "data_daily", guess_max = 30000)
t2m_mean<-t(monthlyfunction(xts(t2m_obs[,2:99], order.by = t2m_obs$Date), FUN = mean, na.rm = TRUE))
t2m_sd<-t(monthlyfunction(xts(t2m_obs[,2:99], order.by = t2m_obs$Date), FUN = sd, na.rm = TRUE))
t2m_anno<-t2m_obs

upper = 3*t2m_sd + t2m_mean
lower = t2m_mean - 3*t2m_sd

for(j in 1:12) {
  for(i in 2:99) {
    t2m_anno[which(as.numeric(format(t2m_anno$Date, "%m"))==j),][,i]<-
      replace(t2m_anno[which(as.numeric(format(t2m_anno$Date, "%m"))==j),][,i], 
          t2m_anno[which(as.numeric(format(t2m_anno$Date, "%m"))==j),][,i] > upper[j,i-1] | 
            t2m_anno[which(as.numeric(format(t2m_anno$Date, "%m"))==j),][,i] < lower[j,i-1], NA)
  }
  print(j)
}

delete<-matrix(1,98)
rownames(delete)<-colnames(t2m_obs)[2:99]
for(j in 2:99) {
  delete[j-1]<-sum(table(t2m_obs[,j]))-sum(table(t2m_anno[,j]))
  print(j)
}

write.csv(t2m_anno, "Data_t2m_v09.csv")
}

#Homoscedasticity test: T2M
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

  cp_test_mean<-cpt.mean(na.omit(t2m_bias), method="PELT", penalty="MBIC", minseglen=12)
  cp_test_var<-cpt.var(na.omit(t2m_bias), method="PELT", penalty="MBIC", minseglen=12)

  t2m_test[i,1]<-ncpts(cp_test_mean)
  t2m_test[i,2]<-ncpts(cp_test_var)
  t2m_test[i,3]<-sum(table(t2m_obs[[i]]))-sum(table(t2m_obs2[[i]]))
}

write.xlsx(t2m_test, "t2m_test.xlsx")
write.xlsx(t2m_sim_era5-t2m_obs, "t2m_bias.xlsx")
write.xlsx(t2m_obs2, "t2m_corrected.xlsx")

#Homoscedasticity test: PP
pp_shape<-shapefile("C:/Users/rooda/Dropbox/Patagonia/GIS South/Precipitation_v0.shp")
pp_obs<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v10.xlsx", sheet = "data_monthly", guess_max = 30000)
pp_obs$Date <- NULL

pp_era5<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_ERA5_1950_2019.nc", varname = "tp")
pp_sim_era5<-as.data.frame(t(raster::extract(pp_era5,pp_shape, method='simple')))
colnames(pp_sim_era5) <- pp_shape$ID

pp_obs2<-pp_obs
pp_test<-matrix(0,length(pp_obs),3, dimnames= list(names(pp_obs), c("cp_mean", "cp_var", "value>3sd")))
FindOutliers <- function(data,sd) {
  mean_data <- mean(data, na.rm = TRUE)
  sd_data <- sd(data, na.rm = TRUE)
  upper = sd*sd_data + mean_data
  lower = mean_data - sd*sd_data
  replace(data, data > upper | data < lower, NA)
}

for(i in 1:length(pp_obs)) {
  print(i)
  pp_bias<-pp_obs[[i]]-pp_sim_era5[[i]]
  pp_obs2[[i]]<-FindOutliers(pp_bias,4)
  
  cp_test_mean<-cpt.mean(na.omit(pp_bias), penalty="Manual", pen.value=50000, method="PELT", minseglen=24)
  cp_test_var<-cpt.var(na.omit(pp_bias),  method="PELT", penalty="MBIC", minseglen=24)
  
  pp_test[i,1]<-ncpts(cp_test_mean)
  pp_test[i,2]<-ncpts(cp_test_var)
  pp_test[i,3]<-sum(table(pp_obs[[i]]))-sum(table(pp_obs2[[i]]))
}

write.xlsx(pp_test, "pp_test.xlsx")
write.xlsx(pp_sim_era5-pp_obs, "pp_bias.xlsx")
write.xlsx(pp_obs2, "pp_corrected.xlsx")

