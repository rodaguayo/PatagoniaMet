rm(list=ls())
cat("\014")  

library("TUWmodel")
library("readxl")
library("raster")

dem_patagonia<-raster("C:/Users/rooda/Dropbox/ArcGIS/Chile/dem_patagonia1.tif")

basins_int<-shapefile("C:/Users/rooda/Dropbox/Patagonia/GIS South/Basins_Patagonia83d.shp")
basins_int<-spTransform(basins_int, crs(dem_patagonia))
basins_int<-basins_int[order(as.numeric(basins_int$gridcode)),]

#PatagoniaMet v1.0, CR2MET v2.0, MSWEP 2.8 and ERA5d
pp_pmet<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_PMET_1990_2019_v2.nc", varname = "PP")
t2m_pmet<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_PMET_1990_2019.nc", varname = "tas")
pet_gleam<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Evapotranspiration/PET_GLEAM_1990_2019.nc", varname = "pet")

pp_cr2met<-stack("E:/Datasets/CR2MET/PP_CR2MET_1979_2019.nc", varname = "Precipitation")
pp_cr2met<- setZ(pp_cr2met, seq(as.Date('1979-01-01'), as.Date('2019-12-01'), by = 'month'))
pp_cr2met<-subset(pp_cr2met, which(getZ(pp_cr2met) >= '1990-01-01'))
t2m_cr2met<-stack("E:/Datasets/CR2MET/T2M_CR2MET_1979_2019.nc", varname = "Temperature")
t2m_cr2met<- setZ(t2m_cr2met, seq(as.Date('1979-01-01'), as.Date('2019-12-01'), by = 'month'))
t2m_cr2met<-subset(t2m_cr2met, which(getZ(t2m_cr2met) >= '1990-01-01'))

pp_mswep<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_MSWEP_1979_2019.nc", varname = "Precipitation")
pp_mswep<- setZ(pp_mswep, seq(as.Date('1979-02-01'), as.Date('2019-12-01'), by = 'month'))
pp_mswep<-subset(pp_mswep, which(getZ(pp_mswep) >= '1990-01-01'))

pp_era5d<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_ERA5_1990_2019.nc", varname = "tp")
t2m_era5d<-stack("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/T2M_ERA5_1990_2019_v1.nc", varname = "tas")

data_area<-matrix(0,length(basins_int),7)
data_area[,1]<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/streamflow/Data_streamflow_v10.xlsx", sheet = "info"))$Area_km2

for (i in 1:length(basins_int)) {
  if (basins_int[i,]$Use == 1){
    
    elev_zones_i<-elevationZones(x=basins_int[i,], dem=dem_patagonia, max.zones = 5, min.elevZ = 300, elev.thres = NULL)
    data_area[i,2]<-length(elev_zones_i$area)
    data_area[i,3:((length(elev_zones_i$area))+2)]<-elev_zones_i$area
    elev_zones_i<-aggregate(elev_zones_i$zonesRaster,9)
    
    if (class(elev_zones_i) == "RasterLayer"){
      elev_zones_i<-rasterToPolygons(elev_zones_i, na.rm = TRUE, dissolve = TRUE)
      
      pet_i<-round(t(raster::extract(pet_gleam, elev_zones_i, fun = mean)),0)
      write.csv(pet_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/PMET/PET/PET_gridcode_", sprintf("%03d", i),".csv"))
      write.csv(pet_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/CR2MET/PET/PET_gridcode_", sprintf("%03d", i),".csv"))
      write.csv(pet_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/MSWEP/PET/PET_gridcode_", sprintf("%03d", i),".csv"))
      write.csv(pet_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/ERA5/PET/PET_gridcode_", sprintf("%03d", i),".csv"))
      
      t2m_i<-round(t(raster::extract(t2m_era5d, elev_zones_i, fun = mean)),3)
      write.csv(t2m_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/ERA5/T2M/T2M_gridcode_", sprintf("%03d", i),".csv"))
      t2m_i<-round(t(raster::extract(t2m_pmet, elev_zones_i, fun = mean)),3)
      write.csv(t2m_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/PMET/T2M/T2M_gridcode_", sprintf("%03d", i),".csv"))
      t2m_i<-round(t(raster::extract(t2m_cr2met, elev_zones_i, fun = mean)),3)
      write.csv(t2m_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/CR2MET/T2M/T2M_gridcode_", sprintf("%03d", i),".csv"))
      write.csv(t2m_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/MSWEP/T2M/T2M_gridcode_", sprintf("%03d", i),".csv"))
      
      pp_i<-round(t(raster::extract(pp_pmet, elev_zones_i, fun = mean)),0)
      write.csv(pp_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/PMET/PP/PP_gridcode_", sprintf("%03d", i),".csv"))
      pp_i<-round(t(raster::extract(pp_cr2met, elev_zones_i, fun = mean)),0)
      write.csv(pp_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/CR2MET/PP/PP_gridcode_", sprintf("%03d", i),".csv"))
      pp_i<-round(t(raster::extract(pp_mswep, elev_zones_i, fun = mean)),0)
      write.csv(pp_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/MSWEP/PP/PP_gridcode_", sprintf("%03d", i),".csv"))
      pp_i<-round(t(raster::extract(pp_era5d, elev_zones_i, fun = mean)),0)
      write.csv(pp_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/ERA5/PP/PP_gridcode_", sprintf("%03d", i),".csv"))
      
      print(i)
      
    } else {
      elev_zones_i<-lapply(as.list(elev_zones_i), rasterToPolygons, na.rm = TRUE, dissolve = TRUE)
      elev_zones_i<-do.call(bind, elev_zones_i) 
      
      pet_i<-round(t(raster::extract(pet_gleam, elev_zones_i, fun = mean)),0)
      write.csv(pet_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/PMET/PET/PET_gridcode_", sprintf("%03d", i),".csv"))
      write.csv(pet_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/CR2MET/PET/PET_gridcode_", sprintf("%03d", i),".csv"))
      write.csv(pet_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/MSWEP/PET/PET_gridcode_", sprintf("%03d", i),".csv"))
      write.csv(pet_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/ERA5/PET/PET_gridcode_", sprintf("%03d", i),".csv"))
      
      t2m_i<-round(t(raster::extract(t2m_era5d, elev_zones_i, fun = mean)),3)
      write.csv(t2m_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/ERA5/T2M/T2M_gridcode_", sprintf("%03d", i),".csv"))
      t2m_i<-round(t(raster::extract(t2m_pmet, elev_zones_i, fun = mean)),3)
      write.csv(t2m_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/PMET/T2M/T2M_gridcode_", sprintf("%03d", i),".csv"))
      t2m_i<-round(t(raster::extract(t2m_cr2met, elev_zones_i, fun = mean)),3)
      write.csv(t2m_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/CR2MET/T2M/T2M_gridcode_", sprintf("%03d", i),".csv"))
      write.csv(t2m_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/MSWEP/T2M/T2M_gridcode_", sprintf("%03d", i),".csv"))
      
      pp_i<-round(t(raster::extract(pp_pmet, elev_zones_i, fun = mean)),0)
      write.csv(pp_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/PMET/PP/PP_gridcode_", sprintf("%03d", i),".csv"))
      pp_i<-round(t(raster::extract(pp_cr2met, elev_zones_i, fun = mean)),0)
      write.csv(pp_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/CR2MET/PP/PP_gridcode_", sprintf("%03d", i),".csv"))
      pp_i<-round(t(raster::extract(pp_mswep, elev_zones_i, fun = mean)),0)
      write.csv(pp_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/MSWEP/PP/PP_gridcode_", sprintf("%03d", i),".csv"))
      pp_i<-round(t(raster::extract(pp_era5d, elev_zones_i, fun = mean)),0)
      write.csv(pp_i, paste0("C:/Users/rooda/Dropbox/Rstudio/TUWmodel/ERA5/PP/PP_gridcode_", sprintf("%03d", i),".csv"))
      
      print(i)
    }
    
  } else {
    print (i)
  }
}

colnames(data_area)<-c("area",	"nbands",	"n1",	"n2",	"n3",	"n4",	"n5")
write.csv(data_area, "C:/Users/rooda/Dropbox/Rstudio/TUWmodel/data_area.csv")
