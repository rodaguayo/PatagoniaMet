# Code for extranting attributes
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

setwd("/home/rooda/Dropbox/Patagonia/")

library("terra")

#Merge polygons by gridcode
list <-list.files(path="/GIS South/Shapefiles2", pattern = "shp$", full.names = TRUE)
ab<-shapefile(lista[1])

for(i in 1:82){
b <- shapefile(lista[i+1])
b <- spTransform(b, crs(ab))
ab<-rbind(ab,b, makeUniqueIDs = TRUE)
print(i)
}

ab <- spTransform(ab, crs("+init=epsg:32719"))
ab$area<-1:nrow(ab)
ab$area <- area(ab) / 1000000

ab <- subset(ab, area > 1)

ab<-ab[order(as.numeric(ab$gridcode)),]
ab <- spTransform(ab, crs("+init=epsg:4326"))
shapefile(ab,"Basins_Patagonia83d.shp", overwrite = TRUE)




basin_data    <-read.csv("Data/Streamflow/Metadata_Streamflow_v10.csv")
basin_shp     <-vect("GIS South/Basins_Patagonia83d.shp")
basin_shp_int <-shapefile("GIS South/Basins_Patagonia83.shp")

dem    <-rast("GIS South/dem_patagonia3f.tif")
basin_data$mean_elevation<-extract(dem, basin_shp, fun=mean, na.rm=TRUE)

pp_stack <- rast("Data/Precipitation/PP_ERA5_hr_1990_2019m.nc")
pp_stack <- pp_era5[[which(getZ(pp_era5) >= as.Date("1989-12-31"))]]
pp_era5<-mean(stackApply(pp_era5, indices<-format(pp_era5@z$time,"%y"), fun=sum))

pp_merra2<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_MERRA2_1980_2019.nc")
pp_merra2<-setZ(pp_merra2,seq(as.Date("1980/1/1"), as.Date("2019/12/1"), "month"))
pp_merra2 <- pp_merra2[[which(getZ(pp_merra2) >= as.Date("1989-12-31"))]]
pp_merra2<-mean(stackApply(pp_merra2, indices<-format(pp_merra2@z$time,"%y"), fun=sum))

pp_csfr<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_CSFR_1979_2019.nc")
pp_csfr<-setZ(pp_csfr,seq(as.Date("1979/1/1"), as.Date("2019/12/1"), "month"))
pp_csfr <- pp_csfr[[which(getZ(pp_csfr) >= as.Date("1989-12-31"))]]
pp_csfr<-mean(stackApply(pp_csfr, indices<-format(pp_csfr@z$time,"%y"), fun=sum))

pp_mswep<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_MSWEP_1979_2017.nc")
pp_mswep<-setZ(pp_mswep,seq(as.Date("1979/1/1"), as.Date('2017-10-31'), "month"))
pp_mswep <- pp_mswep[[which(getZ(pp_mswep) >= as.Date("1989-12-31"))]]
pp_mswep<-mean(stackApply(pp_mswep, indices<-format(pp_mswep@z$time,"%y"), fun=sum))

pp_cr2met<-stack("C:/Users/rooda/Downloads/CR2MET/PP_CR2MET_1979_2018.nc")
pp_cr2met<-setZ(pp_cr2met,seq(as.Date("1979/1/1"), as.Date("2018/12/1"), "month"))
pp_cr2met <- pp_cr2met[[which(getZ(pp_cr2met) >= as.Date("1989-12-31"))]]
pp_cr2met<-mean(stackApply(pp_cr2met, indices<-format(pp_cr2met@z$time,"%y"), fun=sum))

pp_patagoniamet<-raster("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_PMET_1990-2019_v1.tif")
pet_patagoniamet<-raster("C:/Users/rooda/Dropbox/Patagonia/Data/Evapotranspiration/PET_Pmet_1990_2019_v2_mean.tif")
pet_gleam<-raster("C:/Users/rooda/Dropbox/Patagonia/Data/Evapotranspiration/PET_GLEAM_1990_2019_mean.tif")

basins_pp<-matrix(0,83,8)
colnames(basins_pp)<-c("pp_era5","pp_merra2", "pp_csfr","pp_mswep", "pp_cr2metv2.0", "pp_patagonia", "pet_patagonia","pet_gleam")
rownames(basins_pp)<-basins_int$gridcode

basins_pp[,1]<-extract(pp_era5,basins_int,fun=mean,na.rm=TRUE)
basins_pp[,2]<-extract(pp_merra2,basins_int,fun=mean,na.rm=TRUE)
basins_pp[,3]<-extract(pp_csfr,basins_int,fun=mean,na.rm=TRUE)
basins_pp[,4]<-extract(pp_mswep,basins_int,fun=mean,na.rm=TRUE)
basins_pp[,5]<-extract(pp_cr2met,basins_int,fun=mean,na.rm=TRUE)
basins_pp[,6]<-extract(pp_patagoniamet,basins_int,fun=mean,na.rm=TRUE)
basins_pp[,7]<-extract(pet_patagoniamet,basins_int,fun=mean,na.rm=TRUE)
basins_pp[,8]<-extract(pet_gleam,basins_int,fun=mean,na.rm=TRUE)

write.csv(basins_pp, "pp_pet_basins83.csv")
