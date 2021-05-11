rm(list=ls())
cat("\014")  

library("readxl")
library("reddPrec")
library("raster")
library("changepoint")

#Daily precipitation outliers
pp_obs<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v08.xlsx", sheet = "data_daily", guess_max = 30000)
pp_data<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v08.xlsx", sheet = "info")
pp_obs<-10*pp_obs[,2:ncol(pp_obs)]

qcPrec(prec=pp_obs,sts=pp_data,inidate=as.Date('1950-01-01'),enddate=as.Date('2019-12-31'),
                     parallel=TRUE,ncpu=8,printmeta=TRUE,thres=125)

txt_files_ls <- list.files(path="C:/Users/rooda/Dropbox/Rstudio/meta", pattern="*.txt", full.names = TRUE) 
txt_files_df <- lapply(txt_files_ls, function(x) {read.table(file = x, header = T, sep ="")})
combined_df <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 

write.csv(combined_df, "Precipitation_Quality_control_t125.csv")
write.csv(prec/10, "Data_precipitation_v09.csv")


#Monthly precipitation outliers and stationarity test 
pp_shape<-shapefile("C:/Users/rooda/Dropbox/Patagonia/GIS South/Precipitation_v0.shp")
pp_obs<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v10.xlsx", sheet = "data_monthly", guess_max = 30000)
pp_obs$Date <- NULL

pp_era5<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_ERA5_1950_2019.nc", varname = "tp")
pp_era5<-t(raster::extract(pp_era5,pp_shape, method='simple'))
colnames(pp_era5) <- pp_shape$ID

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
  pp_bias<-pp_obs[[i]]-pp_era5[[i]]
  pp_obs2[[i]]<-FindOutliers(pp_bias,4)
  
  #Stationarity test 
  cp_test_mean<-cpt.mean(na.omit(pp_bias), penalty="Manual", pen.value=50000, method="PELT", minseglen=24)
  cp_test_var<-cpt.var(na.omit(pp_bias),  method="PELT", penalty="MBIC", minseglen=24)
  
  pp_test[i,1]<-ncpts(cp_test_mean)
  pp_test[i,2]<-ncpts(cp_test_var)
  pp_test[i,3]<-sum(table(pp_obs[[i]]))-sum(table(pp_obs2[[i]]))
}

write.csv(pp_test, "pp_test.csv")
write.csv(pp_era5-pp_obs, "pp_bias.csv")
write.csv(pp_obs2, "pp_corrected.csv")

