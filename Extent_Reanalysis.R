rm(list=ls())
cat("\014")  

library("raster")

#Patagonia: ERA5
cut<-extent(-79,-64,-57,-40)

stack_pp<-stack("C:/Users/rooda/Desktop/PP_ERA5_1950_1978.nc", varname = "tp")
stack_pp<-crop(stack_pp, cut)*c(31000, 28000, 31000, 30000, 31000, 30000, 31000, 31000, 30000, 31000, 30000, 31000)
stack_pp <- setZ(stack_pp, seq(as.Date('1950-01-01'), as.Date('1978-12-31'), by = 'month'))

stack_t2m<-stack("C:/Users/rooda/Desktop/T2M_ERA5_1950_1978.nc", varname = "t2m")
stack_t2m<-crop(stack_t2m, cut)-273.15
stack_t2m <- setZ(stack_t2m, seq(as.Date('1950-01-01'), as.Date('1978-12-31'), by = 'month'))

stack_pp_post<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_ERA5_1979_2019.nc", varname = "Precipitation")
stack_t2m_post<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_ERA5_1979_2019.nc", varname = "Temperature")

stack_pp<-stack(stack_pp, stack_pp_post)
stack_t2m<-stack(stack_t2m, stack_t2m_post)
stack_pp <- setZ(stack_pp, seq(as.Date('1950-01-01'), as.Date('2019-12-31'), by = 'month'))
stack_t2m <- setZ(stack_t2m, seq(as.Date('1950-01-01'), as.Date('2019-12-31'), by = 'month'))

writeRaster(stack_pp, "PP_ERA5_1950_2019.nc", format = "CDF", datatype='INT2S', overwrite=TRUE, varname="tp", varunit="mm", 
            longname="precipitation", xname="X", yname="Y", zname="time", zunit="month")

writeRaster(stack_t2m, "T2M_ERA5_1950_2019.nc", format = "CDF", overwrite=TRUE, varname="tas", varunit="degC", 
            longname="temperature", xname="X", yname="Y", zname="time", zunit="month")

#Patagonia: ERA5-land
cut<-extent(-79,-64,-57,-40)

stack_pp<-stack("C:/Users/rooda/Desktop/PP_ERA5-land_1981_2019.nc")
stack_pp<-rotate(stack_pp)
stack_pp_crop<-crop(stack_pp, cut)*c(31000, 28000, 31000, 30000, 31000, 30000, 31000, 31000, 30000, 31000, 30000, 31000)
stack_pp_crop <- setZ(stack_pp_crop, seq(as.Date('1981-01-01'), as.Date('2019-12-31'), by = 'month'))
writeRaster(stack_pp_crop, "pp_ERA5", format = "CDF", datatype='INT2S', overwrite=TRUE, varname="Precipitation", varunit="mm", xname="Longitude",   yname="Latitude", zname="Time (Month)")

stack_t2m<-stack("C:/Users/rooda/Desktop/T2M_ERA5-land_1981_2019.nc")
stack_t2m<-rotate(stack_t2m)
stack_t2m_crop<-crop(stack_t2m, cut)-273.15
stack_t2m_crop<-round(stack_t2m_crop, 2)
stack_t2m_crop <- setZ(stack_t2m_crop, seq(as.Date('1981-01-01'), as.Date('2019-12-31'), by = 'month'))
writeRaster(stack_t2m_crop, "t2m_ERA5", format = "CDF",overwrite=TRUE, varname="Temperature", varunit="degC", xname="Longitude",   yname="Latitude", zname="Time", zunit = "Month")

#Patagonia: MERRA2
cut<-extent(-79,-64,-57,-40)

stack_pp<-stack(list.files("C:/Users/rooda/Downloads/MERRA2/PP/", full.names = TRUE))
stack_pp_crop<-crop(stack_pp, cut)*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)*86400
stack_pp_crop <- setZ(stack_pp_crop, seq(as.Date('1980-01-01'), as.Date('2019-12-31'), by = 'month'))
writeRaster(stack_pp_crop, "pp_MERRA2", format = "CDF", datatype='INT2S', overwrite=TRUE, varname="Precipitation", varunit="mm", xname="Longitude",   yname="Latitude", zname="Time (Month)")

stack_t2m<-stack(list.files("C:/Users/rooda/Downloads/MERRA2/T2M/", full.names = TRUE))
stack_t2m_crop<-crop(stack_t2m, cut)-273.15
stack_t2m_crop <- setZ(stack_t2m_crop, seq(as.Date('1980-01-01'), as.Date('2019-12-31'), by = 'month'))
writeRaster(stack_t2m_crop, "t2m_MERRA2", format = "CDF", overwrite=TRUE, varname="Temperature", varunit="mm", xname="Longitude",   yname="Latitude", zname="Time (Month)")

#Patagonia: CFS
cut<-extent(-79,-64,-57,-40)

stack_pp1<-stack(list.files("C:/Users/rooda/Downloads/CSFR/pp/1979-2010/", full.names = TRUE), varname="A_PCP_L1_AccumAvg")
stack_pp2<-stack(list.files("C:/Users/rooda/Downloads/CSFR/pp/2011-2019/", full.names = TRUE), varname="A_PCP_L1_AccumAvg")
stack_pp<-stack(stack_pp1, stack_pp2)
stack_pp<-rotate(stack_pp)

stack_pp_crop<-crop(stack_pp, cut)*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)*4
stack_pp_crop <- setZ(stack_pp_crop, seq(as.Date('1979-01-01'), as.Date('2019-12-31'), by = 'month'))
writeRaster(stack_pp_crop, "pp_CSFR", format = "CDF", datatype='INT2S', overwrite=TRUE, varname="Precipitation", varunit="mm", xname="Longitude",   yname="Latitude", zname="Time (Month)")

stack_t2m1<-stack(list.files("C:/Users/rooda/Downloads/CSFR/t2m/1979-2010/", full.names = TRUE), varname="TMP_L103_Avg")
stack_t2m2<-stack(list.files("C:/Users/rooda/Downloads/CSFR/t2m/2011-2019/", full.names = TRUE), varname="TMP_L103_Avg")
stack_t2m2<-resample(stack_t2m2,stack_t2m1, method="bilinear")
stack_t2m<-stack(stack_t2m1, stack_t2m2)
stack_t2m_crop<-round(stack_t2m_crop, 2)
stack_t2m<-rotate(stack_t2m)

stack_t2m_crop<-crop(stack_t2m, cut)-273.15
stack_t2m_crop<-round(stack_t2m_crop, 2)
stack_t2m_crop <- setZ(stack_t2m_crop, seq(as.Date('1979-01-01'), as.Date('2019-12-31'), by = 'month'))
writeRaster(stack_t2m_crop, "t2m_CSFR", format = "CDF", overwrite=TRUE, varname="Temperature", varunit="degC", xname="Longitude",   yname="Latitude", zname="Time(Month)")

#Patagonia: MSWEP
stack_pp<-stack(list.files("C:/Users/rooda/Downloads/MSWEP/", full.names = TRUE), varname="precipitation")
stack_pp<-crop(stack_pp, cut)
stack_pp<- setZ(stack_pp, seq(as.Date('1979-02-01'), as.Date('2020-11-01'), by = 'month'))
stack_pp<-subset(stack_pp, which(getZ(stack_pp) <= '2019-12-31'))
stack_pp<- setZ(stack_pp, seq(as.Date('1979-02-01'), as.Date('2019-12-01'), by = 'month'))
writeRaster(stack_pp, "PP_MSWEP_1979_2019", format = "CDF", datatype='INT2S', overwrite=TRUE, varname="Precipitation", varunit="mm", xname="Longitude",   yname="Latitude", zname="Time (Month)")

#Patagonia: CR2MET v2.0
cut<-extent(-79,-64,-57,-40)

stack_pp<-stack("E:/Datasets/CR2MET/CR2MET_pr_v2.0_mon_1979_2019_005deg.nc", varname="pr")
stack_pp<-crop(stack_pp, cut)
stack_pp<- setZ(stack_pp, seq(as.Date('1979-01-01'), as.Date('2019-12-01'), by = 'month'))
writeRaster(stack_pp, "PP_CR2MET_1979_2019.nc", format = "CDF", datatype='INT2S', overwrite=TRUE, varname="Precipitation", varunit="mm", xname="Longitude",   yname="Latitude", zname="Time (Month)")

stack_pp<-stack("E:/Datasets/CR2MET/CR2MET_t2m_v2.0_mon_1979_2019_005deg.nc", varname="t2m")
stack_pp<-crop(stack_pp, cut)
stack_pp<- setZ(stack_pp, seq(as.Date('1979-01-01'), as.Date('2019-12-01'), by = 'month'))
writeRaster(stack_pp, "T2M_CR2MET_1979_2019.nc", format = "CDF", overwrite=TRUE, varname="Temperature", varunit="degC", xname="Longitude",   yname="Latitude", zname="Time (Month)")

#Patagonia: CR2MET-RegCM4
cut<-extent(-76,-66,-57,-40)

stack_pp<-stack(list.files("C:/Users/rooda/Desktop/pr/", full.names = TRUE), varname="pr")
stack_pp_crop<-crop(stack_pp, cut)*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)*86400
stack_pp_crop<- setZ(stack_pp_crop, seq(as.Date('1980-01-01'), as.Date('2015-12-31'), by = 'month'))
writeRaster(stack_pp_crop, "PP_CR2MET_RegCM4_1980_2015.nc", format = "CDF", datatype='INT2S', overwrite=TRUE, varname="Precipitation", varunit="mm", xname="Longitude",   yname="Latitude", zname="Time (Month)")

stack_t2m<-stack(list.files("C:/Users/rooda/Desktop/tas/", full.names = TRUE), varname="tas")
stack_t2m_crop<-crop(stack_t2m, cut)-273.15
stack_t2m_crop <- setZ(stack_t2m_crop, seq(as.Date('1980-01-01'), as.Date('2015-12-31'), by = 'month'))
writeRaster(stack_t2m_crop, "T2M_CR2MET_RegCM4__1980_2015.nc", format = "CDF", overwrite=TRUE, varname="tas", varunit="degC", xname="Longitude",   yname="Latitude", zname="Time(Month)")

#Patagonia: PET-ET Gleam
cut<-extent(-76,-66,-57,-40)
fill.na <- function(x, i=5) {if( is.na(x)[i] ) {return(mean(x, na.rm=TRUE))} else {return(x[i])}}  

pet_gleam<-flip(t(stack("C:/Users/rooda/Downloads/Ep_1980-2020_GLEAM_v3.5a_MO.nc", varname = "Ep")), 1)
crs(pet_gleam)<-CRS("+init=epsg:4326")
pet_gleam<-crop(pet_gleam, cut)

for(i in 1:492) {pet_gleam[[i]]<- focal(pet_gleam[[i]], w = matrix(1,3,3), fun = fill.na, pad = TRUE, na.rm = FALSE)}
pet_gleam<-setZ(pet_gleam, seq(as.Date("1980/1/1"), as.Date("2020/12/31"), "month"))
pet_gleam<-subset(pet_gleam, which(getZ(pet_gleam) >= '1990-01-01' & (getZ(pet_gleam) <= '2019-12-31')))
pet_gleam_mean<-mean(stackApply(pet_gleam, indices<-format(pet_gleam@z$time,"%y"), fun=sum))
pet_gleam_mean[pet_gleam_mean == 0] <- NA
writeRaster(pet_gleam, "PET_GLEAM_1990_2019.nc", datatype = "INT2S" ,format = "CDF", overwrite=TRUE, varname="pet", varunit="mm", xname="Longitude",   yname="Latitude", zname="Time(Month)")
writeRaster(pet_gleam_mean, "PET_GLEAM_1990_2019_mean.tif", format = "GTiff", overwrite = TRUE)

et_gleam<-flip(t(stack("C:/Users/rooda/Downloads/E_1980-2020_GLEAM_v3.5a_MO.nc", varname = "E")), 1)
crs(et_gleam)<-CRS("+init=epsg:4326")
et_gleam<-crop(et_gleam, cut)

for(i in 1:492) {et_gleam[[i]]<- focal(et_gleam[[i]], w = matrix(1,3,3), fun = fill.na, pad = TRUE, na.rm = FALSE)}
et_gleam<-setZ(et_gleam, seq(as.Date("1980/1/1"), as.Date("2020/12/31"), "month"))
et_gleam<-subset(et_gleam, which(getZ(et_gleam) >= '1990-01-01' & (getZ(et_gleam) <= '2019-12-31')))
et_gleam_mean<-mean(stackApply(et_gleam, indices<-format(et_gleam@z$time,"%y"), fun=sum))
et_gleam_mean[et_gleam_mean == 0] <- NA
writeRaster(et_gleam, "ET_GLEAM_1990_2019.nc", datatype = "INT2S" ,format = "CDF", overwrite=TRUE, varname="pet", varunit="mm", xname="Longitude",   yname="Latitude", zname="Time(Month)")
writeRaster(et_gleam_mean, "ET_GLEAM_1990_2019_mean.tif", format = "GTiff", overwrite = TRUE)


