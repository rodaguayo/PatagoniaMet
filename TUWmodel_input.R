# TUWmodel inputs: precipitation, air temperature and potential evapotranspiration
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

source("elevationZones.R")
library("exactextractr")
library("terra")
library("sf")

terra::gdalCache(17000)
setwd("/home/rooda/Dropbox/Patagonia/")
period  <- c(as.POSIXct("1980-01-01", tz = "UTC"), as.POSIXct("2019-12-31", tz = "UTC"))

dem <- rast("GIS South/dem_patagonia3f.tif")
dem <- aggregate(dem, fact=6, fun="mean")
dem <- disagg(dem, fact=2, method = "bilinear") # avoid bug in elevation bands

q_data  <- read.csv("Data/Streamflow/Data_Streamflow_v10_daily.csv")

basins_int <- vect("GIS South/Basins_Patagonia83d.shp")
basins_int <- project(basins_int, crs(dem))
basins_int <- basins_int[order(as.numeric(basins_int$gridcode)),]

# PP and T2M: PMET v1.0; PET: GLEAM v3.6
pp_pmet    <- rast("Data/Precipitation/PP_PMET_1980_2020d.nc")
t2m_pmet   <- rast("Data/Temperature/Tavg_PMET_1980_2020d.nc")
pet_gleam  <- rast("Data/Evapotranspiration/PET_GLEAM36a_1980_2021d.nc")
time(pp_pmet)   <- as.POSIXct(time(pp_pmet))
time(t2m_pmet)  <- as.POSIXct(time(t2m_pmet))
time(pet_gleam) <- as.POSIXct(time(pet_gleam))
pp_pmet   <- pp_pmet[[time(pp_pmet)  >= period[1] & time(pp_pmet)  <= period[2]]]
t2m_pmet  <- t2m_pmet[[time(t2m_pmet)  >= period[1] & time(t2m_pmet)  <= period[2]]]
pet_gleam <- pet_gleam[[time(pet_gleam)  >= period[1] & time(pet_gleam)  <= period[2]]]

# PP and T2M: CR2MET v2.0; PET: GLEAM v3.6
pp_cr2met  <- rast("Data/Precipitation/PP_CR2MET_1979_2020d.nc")
t2m_cr2met <- rast("Data/Temperature/Tavg_CR2MET_1979_2020d.nc")
pp_cr2met  <- pp_cr2met[[time(pp_cr2met)  >= period[1] & time(pp_cr2met)  <= period[2]]]
t2m_cr2met <- t2m_cr2met[[time(t2m_cr2met)  >= period[1] & time(t2m_cr2met)  <= period[2]]]

# PP: MSWEP v2.8; T2M: CR2MET v2.0; PET: GLEAM v3.6
pp_mswep   <- rast("Data/Precipitation/PP_MSWEPv28_1979_2020d.nc")
time(pp_mswep) <- as.POSIXct(time(pp_mswep))
pp_mswep  <- pp_mswep[[time(pp_mswep)  >= period[1] & time(pp_mswep)  <= period[2]]]

# PP and T2M: ERA5d; PET: GLEAM v3.6
pp_era5d   <- rast("Data/Precipitation/PP_ERA5_hr_1980_2020d.nc")
t2m_era5d  <- rast("Data/Temperature/Tavg_ERA5_hr_1980_2020d.nc")
time(pp_era5d)  <- as.POSIXct(time(pp_era5d))
time(t2m_era5d) <- as.POSIXct(time(t2m_era5d))
pp_era5d   <- pp_era5d[[time(pp_era5d)  >= period[1] & time(pp_era5d)  <= period[2]]]
t2m_era5d  <- t2m_era5d[[time(t2m_era5d)  >= period[1] & time(t2m_era5d)  <= period[2]]]

data_area <- data.frame(matrix(0, length(basins_int), 7), row.names = names(basins_int$gridcode))
colnames(data_area)<-c("area", "nbands",	"n1",	"n2",	"n3",	"n4",	"n5")

for (i in 1:length(basins_int)) {
  q_data_i <- subset(q_data, Date > period[1] & Date < period[2])[,i+1]
  
  if (sum(!is.na(q_data_i))/length(q_data_i) >= 0.5 & basins_int$Use[i] == 1){
    
    elev_zones_i   <- elevationZones(x=basins_int[i,], dem=dem, max.zones = 5, min.elevZ = 300)
    data_area[i,2] <- basins_int[i,]$area
    data_area[i,3:((length(elev_zones_i$area))+2)]<-elev_zones_i$area
    elev_zones_i <- as.polygons(elev_zones_i$zonesRaster, na.rm = TRUE, dissolve = TRUE)
    elev_zones_i <- st_as_sf(elev_zones_i)
    
    # Daily potential evapotranspiration 
    pet_i <- round(t(exact_extract(pet_gleam, elev_zones_i, "mean", progress = F)), 1)
    write.csv(pet_i, paste0("MS1 Results/TUWmodel/PMET/PET/PET_gridcode_",   sprintf("%03d", i),".csv"))
    write.csv(pet_i, paste0("MS1 Results/TUWmodel/CR2MET/PET/PET_gridcode_", sprintf("%03d", i),".csv"))
    write.csv(pet_i, paste0("MS1 Results/TUWmodel/MSWEP/PET/PET_gridcode_",  sprintf("%03d", i),".csv"))
    write.csv(pet_i, paste0("MS1 Results/TUWmodel/ERA5/PET/PET_gridcode_",   sprintf("%03d", i),".csv"))
      
    # Daily air temperature
    t2m_i <- round(t(exact_extract(t2m_era5d, elev_zones_i, "mean", progress = F)), 2)
    write.csv(t2m_i, paste0("MS1 Results/TUWmodel/ERA5/T2M/T2M_gridcode_",   sprintf("%03d", i),".csv"))
    t2m_i <- round(t(exact_extract(t2m_pmet, elev_zones_i, "mean", progress = F)),  2)
    write.csv(t2m_i, paste0("MS1 Results/TUWmodel/PMET/T2M/T2M_gridcode_",   sprintf("%03d", i),".csv"))
    t2m_i <- round(t(exact_extract(t2m_cr2met, elev_zones_i, "mean", progress = F)),2)
    write.csv(t2m_i, paste0("MS1 Results/TUWmodel/CR2MET/T2M/T2M_gridcode_", sprintf("%03d", i),".csv"))
    write.csv(t2m_i, paste0("MS1 Results/TUWmodel/MSWEP/T2M/T2M_gridcode_",  sprintf("%03d", i),".csv"))
      
    # Daily precipitation
    pp_i <- round(t(exact_extract(pp_pmet, elev_zones_i, "mean", progress = F)),  1)
    write.csv(pp_i, paste0("MS1 Results/TUWmodel/PMET/PP/PP_gridcode_",   sprintf("%03d", i),".csv"))
    pp_i <- round(t(exact_extract(pp_cr2met, elev_zones_i, "mean", progress = F)),1)
    write.csv(pp_i, paste0("MS1 Results/TUWmodel/CR2MET/PP/PP_gridcode_", sprintf("%03d", i),".csv"))
    pp_i <- round(t(exact_extract(pp_mswep, elev_zones_i, "mean", progress = F)), 1)
    write.csv(pp_i, paste0("MS1 Results/TUWmodel/MSWEP/PP/PP_gridcode_",  sprintf("%03d", i),".csv"))
    pp_i <- round(t(exact_extract(pp_era5d, elev_zones_i, "mean", progress = F)), 1)
    write.csv(pp_i, paste0("MS1 Results/TUWmodel/ERA5/PP/PP_gridcode_",   sprintf("%03d", i),".csv"))
    
    print(i)
    
    } else {
      print (i)
    }
  }

write.csv(data_area, "MS1 Results/TUWmodel/data_area.csv")
