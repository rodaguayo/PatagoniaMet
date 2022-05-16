# Code for temperature bias correction: Variance and mean scaling
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("randomForest")
library("hydroGOF")
library("terra")

setwd("/home/rooda/Dropbox/Patagonia/")

# Data: observations and gridded product
t2m_stack_hr<-rast("Data/Temperature/T2M_ERA5_hr_1990_2019m.nc")
names(t2m_stack_hr)<-time(t2m_stack_hr)
period <- c(min(time(t2m_stack_hr))-86400, max(time(t2m_stack_hr))+86400)

t2m_shape <- read.csv("Data/Temperature/Metadata_Temperature_v10.csv")
t2m_shape <- vect(t2m_shape, geom=c("Longitude", "Latitude"), crs="epsg:4326")
t2m_obs   <- read.csv("Data/Temperature/Data_Temperature_mean_v10_monthly.csv")
t2m_obs$Date<-as.POSIXct(t2m_obs$Date) #The date is the first column
t2m_obs   <- subset(t2m_obs, Date >= period[1] & Date <= period[2])

# First stage: Mean and variance parameters for random forest regressions 
t2m_sim   <- as.data.frame(t(extract(t2m_stack_hr, t2m_shape, method='simple')))[-1,]

mean_value  <- as.data.frame(me(sim=t2m_sim, obs=t2m_obs[,-1], na.rm=TRUE))

t2m_sim2  <- t2m_sim
for (i in 1:length(t2m_shape)) {t2m_sim2[,i]<-t2m_sim[,i]-ME_t2m[i]}
for (i in 1:length(t2m_shape)) {t2m_sim2[,i]<-t2m_sim2[,i]-mean(t2m_sim2[,i])}

rSD_t2m2<-1/rSD(sim=t2m_sim2, obs=t2m_obs[,-1], na.rm=TRUE)

rSD_t2m<-rSD(sim=t2m_sim, obs=t2m_obs[,-1], na.rm=TRUE)




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



rf1 <- randomForest(model, data=envtrain)
## Warning in randomForest.default(m, y, ...): The response has five or fewer
## unique values. Are you sure you want to do regression?
model <- factor(pa) ~ bio1 + bio5 + bio6 + bio7 + bio8 + bio12 + bio16 + bio17
rf2 <- randomForest(model, data=envtrain)
rf3 <- randomForest(envtrain[,1:8], factor(pb_train))
erf <- evaluate(testpres, testbackg, rf1)
erf
## class          : ModelEvaluation
## n presences    : 23
## n absences     : 200
## AUC            : 0.8580435
## cor            : 0.5010053
## max TPR+TNR at : 0.1060667
pr <- predict(predictors, rf1, ext=ext)
par(mfrow=c(1,2))
plot(pr, main='Random Forest, regression')
plot(wrld_simpl, add=TRUE, border='dark grey')
tr <- threshold(erf, 'spec_sens')
plot(pr > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backg_train, pch='-', cex=0.25)



# Third stage: Mean and variance scaling 
delta_value    <- resample(rast("MS1 Results/T2M_mean_value.tif"),    t2m_stack_hr)
variance_value <- resample(rast("MS1 Results/T2M_variance_value.tif"),t2m_stack_hr)
variance_value[variance_value == 0 ] <- NA


t2m_stack_hrc  <- t2m_stack_hr - delta_value
t2m_pmet       <- t2m_stack_hrc-mean(t2m_stack_hrc)
t2m_pmet       <- t2m_pmet*variance_value
t2m_pmet       <- t2m_pmet + mean(t2m_stack_hrc)

t2m_stack_hr_m <- mean(tapp(t2m_stack_hr, strftime(time(t2m_stack_hr),format="%Y"), fun = mean, na.rm = TRUE)) # mean annual value

writeCDF(t2m_stack_hr, "Data/Temperature/T2M_PMET_1990_2019m.nc",  overwrite=TRUE, varname="t2m", unit="degC", longname="Temperature", zname="time", compression = 9)
writeRaster(t2m_stack_hr_m, "Data/Temperature/T2M_PMET_1990_2019m.tif", overwrite=TRUE)

