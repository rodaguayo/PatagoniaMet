# Code for precipitation bias correction
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("stats")
library("qmap")
library("terra")
library("hydroGOF")

setwd("/home/rooda/Dropbox/Patagonia/")

# Data: observations and gridded product
pp_stack_hr<-rast("Data/Precipitation/PP_ERA5_hr_1990_2019m.nc")
names(pp_stack_hr)<-time(pp_stack_hr)
period <- c(min(time(pp_stack_hr))-86400, max(time(pp_stack_hr))+86400)

pp_shape <- read.csv("Data/Precipitation/Metadata_Precipitation_v10.csv")
pp_shape <- vect(pp_shape, geom=c("Longitude", "Latitude"), crs="epsg:4326")
pp_obs   <- read.csv("Data/Precipitation/Data_Precipitation_v10_monthly.csv")
pp_obs$Date<-as.POSIXct(pp_obs$Date) #The date is the first column
pp_obs   <- subset(pp_obs, Date >= period[1] & Date <= period[2])
pp_sim   <- as.data.frame(t(extract(pp_stack_hr, pp_shape, method='simple')))[-1,]
pp_obs$Date <- NULL

QM_parameters<-data.frame(matrix(0,length(pp_shape),4), row.names = names(pp_obs))
colnames(QM_parameters)<-c("a_power","b_power","a_linear","b_linear") 
KGE<-data.frame(matrix(0,length(pp_shape),3*4),  row.names = names(pp_obs)) # for small verification

for (i in 1:length(pp_shape)) {
  fit_ptf_power      <- fitQmapPTF(pp_obs[,i], pp_sim[,i], transfun="power",        wet.day=0, cost="RSS")
  fit_ptf_linear     <- fitQmapPTF(pp_obs[,i], pp_sim[,i], transfun="linear",       wet.day=0, cost="RSS")

  QM_parameters[i,]<-c(fit_ptf_power$par, fit_ptf_linear$par)

  KGE[i,] <- c(unlist(KGE(sim=pp_sim[,i],                           obs=pp_obs[,i], method="2012", out.type="full",na.rm=TRUE)),
               unlist(KGE(sim=doQmapPTF(pp_sim[,i], fit_ptf_power), obs=pp_obs[,i], method="2012", out.type="full",na.rm=TRUE)),
               unlist(KGE(sim=doQmapPTF(pp_sim[,i],fit_ptf_linear), obs=pp_obs[,i], method="2012", out.type="full",na.rm=TRUE)))
  print(i)
}

# Second stage: Random forest predictors
covariates     <- c(rast("GIS South/west_gradient005.tif"),
                    rast("GIS South/clouds_005.tif"),
                    rast("GIS South/aspect_dem005.tif"),
                    rast("GIS South/climate_class005.tif"),
                    rast("GIS South/dist_coast005l.tif"),
                    rast("GIS South/dem_patagonia005.tif"),
                    rast("Data/Temperature/T2M_ERA5_hr_1990_2019m.tif"),
                    rast("Data/Precipitation/PP_ERA5_hr_1990_2019m.tif"))
names(covariates)<-c("west_gradient", "cloud_cover", "aspect", "climate_class", "distance_coast", "elevation", "mean_t2m", "mean_pp")

model_mean <- pa ~ west_gradient + cloud_cover + aspect + climate_class + distance_coast + elevation + mean_t2m + mean_pp

#Third stage: Distributed bias correction: Parametric quantile mapping 
dem <- covariates[[6]]


dem[dem <= 1] <- NA
pp_era5 <- mask(pp_era5, dem)


b_linear[b_linear == 0] <- NA

gf <- focalWeight(pp_era5, 0.04, "Gauss")
a_linear <- mask(focal(a_linear, w=gf, pad = TRUE), dem)
b_linear <- mask(focal(b_linear, w=gf, pad = TRUE), dem)

pp_era5_v2=a_linear+pp_era5*b_linear
pp_era5_v2[pp_era5_v2 < 0] <- 0

p_era5_v2_mean<-mean(stackApply(pp_era5_v2, indices<-format(pp_era5_v2@z$time,"%y"), fun=sum))

# writeCDF(pp_stack_hr, "Data/Precipitation/PP_PMET_1990_2019m.nc",  overwrite=TRUE, varname="pp", unit="mm", longname="Precipitation", zname="time", compression = 9)
# writeRaster(pp_stack_hr_m, "Data/Precipitation/PP_PMET_1990_2019m.tif", overwrite=TRUE)


#Estimation of "w" from National Water Balance
basins <- vect("C:/Users/rooda/Dropbox/Patagonia/GIS South/Basins_Patagonia83.shp")

q_bh   <- read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/streamflow/Data_streamflow_v10.xlsx", sheet = "budyko")
q_data <- read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/streamflow/Data_streamflow_v10.xlsx", sheet = "info")
PET_PP <- as.numeric(q_bh$`PET/PP`)
ET_PP  <- as.numeric(q_bh$`ET/PP`)
Ri     <- as.numeric(q_data$R_int_mm_year)
PETi   <- as.numeric(q_data$pet_gleam)

w_inf <- function(PET_PP, ET_PP, w) 1 + PET_PP - ET_PP - (1+((PET_PP)^w))^(1/w)
vec_w <- numeric(0)

#Only basins in Chile (79 of 83)
for (j in 1:length(q_bh$Name)) {
  vec_w[j] <- uniroot(w_inf, interval=c(0, 10), tol = 0.001, PET_PP=PET_PP[j], ET_PP=ET_PP[j])$root
}
write.csv(vec_w, "w_estimation.csv")


#Estimation of long-term "real" precipitation
pp_inf <- function(PP, R, PET, w) 1 + PET/PP - ((PP-R)/PP) - (1+((PET/PP)^w))^(1/w)
w      <- round(median(vec_w),1)
vec_pp <- numeric(0)

for (j in 1:length(basins)) {
  if (is.na(Ri[j])){
    vec_pp[j]<-NA
    print(j)
    
  } else {
    pp <- uniroot(pp_inf, interval=c(10, 15000), tol = 0.000001, R=Ri[j], PET=PETi[j], w=w)
    vec_pp[j] <- pp$root
    print(j)
  }
}

write.csv(round(vec_pp,1), "C:/Users/rooda/Dropbox/Rstudio/PP_REAL_1990_2019.csv")










#Third stage: Budyko correction
pp_pmet<- stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_PMET_1990_2019_v1.nc", varname = "PP")
bias_factor<-resample(raster("C:/Users/rooda/Dropbox/Patagonia/GIS South/bias_factor005.tif"), pp_pmet)
bias_factor[bias_factor == 0] <- NA

bias_factor <- focal(bias_factor, w=focalWeight(bias_factor, 0.04, "Gauss"))
bias_factor[bias_factor <= 1] <- 1
bias_factor<-mask(bias_factor,pp_patagoniamet[[1]])

pp_pmet_v2<-pp_pmet*bias_factor
pp_pmet_v2<-setZ(pp_pmet_v2,seq(as.Date("1990/1/1"), as.Date("2019/12/1"), "month"))

pp_pmet_v2_mean<-mean(stackApply(pp_pmet_v2, indices<-format(pp_pmet_v2@z$time,"%y"), fun=sum))
pp_pmet_v2_mean[pp_pmet_v2_mean == 0] <- NA

writeRaster(pp_pmet_v2, "PP_PMET_1990_2019_v2.nc", format = "CDF", overwrite=TRUE, varname="PP", 
            varunit="mm", longname="precipitation", xname="X", yname="Y", zname="time", zunit="month")
writeRaster(pp_pmet_v2_mean, "PP_PMET_1990-2019_mean_v2.tif", format = "GTiff", overwrite = TRUE)

