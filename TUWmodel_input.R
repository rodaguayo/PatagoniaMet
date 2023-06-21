# TUWmodel inputs ---------------------------------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2023)

rm(list=ls())
cat("\014")  

source("elevationZones.R")
library("exactextractr")
library("terra")
library("sf")

terra::gdalCache(20000)
setwd("/home/rooda/Dropbox/Patagonia/")
period  <- c(as.POSIXct("1987-01-01", tz = "UTC"), as.POSIXct("2019-12-31", tz = "UTC"))

dem <- rast("GIS South/dem_patagonia3f.tif")
dem <- aggregate(dem, fact=6, fun="mean")
dem <- disagg(dem, fact=2, method = "bilinear") # avoid bug in elevation bands

basin_data <- read.csv("Data/Streamflow/Q_PMETobs_v10_metadata.csv")
basins_int <- vect("GIS South/Basins_PMET_v10.shp")
basins_int <- project(basins_int, crs(dem))
basins_int$area_km2 <- expanse(basins_int, unit="km")

# PP, PET and T2M: PMET v1.0
pp_pmet    <- rast("Data/Precipitation/PP_PMET_1980_2020d.nc")
t2m_pmet   <- rast("Data/Temperature/Tavg_PMET_1980_2020d.nc")
pet_pmet <- rast("Data/Evapotranspiration/PET_PMET_1980_2020d.nc")
terra::time(pp_pmet)  <- as.POSIXct(time(pp_pmet))
terra::time(t2m_pmet) <- as.POSIXct(time(t2m_pmet))
terra::time(pet_pmet) <- as.POSIXct(time(pet_pmet))
pp_pmet   <- pp_pmet[[time(pp_pmet)  >= period[1] & time(pp_pmet)  <= period[2]]]
t2m_pmet  <- t2m_pmet[[time(t2m_pmet)  >= period[1] & time(t2m_pmet)  <= period[2]]]
pet_pmet  <- pet_pmet[[time(pet_pmet)  >= period[1] & time(pet_pmet)  <= period[2]]]

# PP, PET and T2M: CR2MET v2.5
pp_cr2met  <- rast("Data/Precipitation/PP_CR2MET_1960_2021d.nc")
t2m_cr2met <- rast("Data/Temperature/Tavg_CR2MET_1960_2021d.nc")
pet_cr2met <- rast("Data/Evapotranspiration/PET_CR2MET_1980_2020d.nc")
terra::time(pp_cr2met)  <- as.POSIXct(time(pp_cr2met))
terra::time(t2m_cr2met) <- as.POSIXct(time(t2m_cr2met))
terra::time(pet_cr2met) <- as.POSIXct(time(pet_cr2met))
pp_cr2met  <- pp_cr2met[[ time(pp_cr2met)   >= period[1] & time(pp_cr2met)   <= period[2]]]
t2m_cr2met <- t2m_cr2met[[time(t2m_cr2met)  >= period[1] & time(t2m_cr2met)  <= period[2]]]
pet_cr2met <- pet_cr2met[[time(pet_cr2met)  >= period[1] & time(pet_cr2met)  <= period[2]]]

# PP, PET and T2M: MSWEP and  MSWX
pp_mswep   <- rast("Data/Precipitation/PP_MSWEPv28_1979_2020d.nc")
t2m_mswx   <- rast("Data/Temperature/Tavg_MSWX_1979_2019d.nc")
pet_mswx   <- rast("Data/Evapotranspiration/PET_MSWX_1980_2020d.nc")
terra::time(pp_mswep) <- as.POSIXct(time(pp_mswep))
terra::time(t2m_mswx) <- as.POSIXct(time(t2m_mswx))
terra::time(pet_mswx) <- as.POSIXct(time(pet_mswx))
pp_mswep   <- pp_mswep[[time(pp_mswep)  >= period[1] & time(pp_mswep)  <= period[2]]]
t2m_mswx   <- t2m_mswx[[time(t2m_mswx)  >= period[1] & time(t2m_mswx)  <= period[2]]]
pet_mswx   <- pet_mswx[[time(pet_mswx)  >= period[1] & time(pet_mswx)  <= period[2]]]

# PP, PET and T2M: ERA5d
pp_era5d   <- rast("Data/Precipitation/PP_ERA5_hr_1980_2020d.nc")
t2m_era5d  <- rast("Data/Temperature/Tavg_ERA5_hr_1980_2020d.nc")
pet_era5d <- rast("Data/Evapotranspiration/PET_ERA5_hr_1980_2020d.nc")
terra::time(pp_era5d)  <- as.POSIXct(time(pp_era5d))
terra::time(t2m_era5d) <- as.POSIXct(time(t2m_era5d))
terra::time(pet_era5d) <- as.POSIXct(time(pet_era5d))
pp_era5d   <- pp_era5d[[time(pp_era5d)  >= period[1] & time(pp_era5d)  <= period[2]]]
t2m_era5d  <- t2m_era5d[[time(t2m_era5d)  >= period[1] & time(t2m_era5d)  <= period[2]]]
pet_era5d  <- pet_era5d[[time(pet_era5d)  >= period[1] & time(pet_era5d)  <= period[2]]]

# PP, PET T2M: W5E5 v2.0
pp_w5e5   <- rast("Data/Precipitation/PP_W5E5_1979_2019d.nc")
t2m_w5e5  <- rast("Data/Temperature/Tavg_W5E5_1979_2019d.nc")
pet_w5e5  <- rast("Data/Evapotranspiration/PET_W5E5_1980_2019d.nc")
terra::time(pp_w5e5)  <- as.POSIXct(time(pp_w5e5))
terra::time(t2m_w5e5) <- as.POSIXct(time(t2m_w5e5))
terra::time(pet_w5e5) <- as.POSIXct(time(pet_w5e5))
pp_w5e5   <- pp_w5e5[[time(pp_w5e5)  >= period[1] & time(pp_w5e5)  <= period[2]]]
t2m_w5e5  <- t2m_w5e5[[time(t2m_w5e5)  >= period[1] & time(t2m_w5e5)  <= period[2]]]
pet_w5e5  <- pet_w5e5[[time(pet_w5e5)  >= period[1] & time(pet_w5e5)  <= period[2]]]

data_area <- data.frame(matrix(0, length(basins_int), 7), row.names = names(basins_int$gauge_id))
colnames(data_area)<-c("area", "nbands",	"n1",	"n2",	"n3",	"n4",	"n5")

for (i in 1:length(basins_int)) {
  if (basin_data$Modeled[i] == 1){
    
    elev_zones_i   <- elevationZones(x=basins_int[i,], dem=dem, max.zones = 5, min.elevZ = 300)
    data_area[i,1] <- basins_int[i,]$area_km2
    data_area[i,2] <- length(elev_zones_i$area)
    data_area[i,3:((length(elev_zones_i$area))+2)] <- elev_zones_i$area
    elev_zones_i <- as.polygons(elev_zones_i$zonesRaster, na.rm = TRUE, dissolve = TRUE)
    elev_zones_i <- st_as_sf(elev_zones_i)
    
    # Daily potential evapotranspiration 
    pet_i <- round(t(exact_extract(pet_pmet, elev_zones_i, "mean", progress = F)), 1)
    write.csv(pet_i, paste0("MS1 Results/TUWmodel/PMET/PET/PET_gridcode_",   sprintf("%03d", i),".csv"))
    pet_i <- round(t(exact_extract(pet_cr2met, elev_zones_i, "mean", progress = F)), 1)    
    write.csv(pet_i, paste0("MS1 Results/TUWmodel/CR2MET/PET/PET_gridcode_", sprintf("%03d", i),".csv"))
    pet_i <- round(t(exact_extract(pet_mswx, elev_zones_i, "mean", progress = F)), 1)    
    write.csv(pet_i, paste0("MS1 Results/TUWmodel/MSWEP/PET/PET_gridcode_",  sprintf("%03d", i),".csv"))
    pet_i <- round(t(exact_extract(pet_era5d, elev_zones_i, "mean", progress = F)), 1)   
    write.csv(pet_i, paste0("MS1 Results/TUWmodel/ERA5/PET/PET_gridcode_",   sprintf("%03d", i),".csv"))
    pet_i <- round(t(exact_extract(pet_w5e5, elev_zones_i, "mean", progress = F)), 1)    
    write.csv(pet_i, paste0("MS1 Results/TUWmodel/W5E5/PET/PET_gridcode_",   sprintf("%03d", i),".csv"))
    print("pet")
    
    # Daily air temperature
    t2m_i <- round(t(exact_extract(t2m_era5d, elev_zones_i, "mean", progress = F)), 2)
    write.csv(t2m_i, paste0("MS1 Results/TUWmodel/ERA5/T2M/T2M_gridcode_",   sprintf("%03d", i),".csv"))
    t2m_i <- round(t(exact_extract(t2m_pmet, elev_zones_i, "mean", progress = F)),  2)
    write.csv(t2m_i, paste0("MS1 Results/TUWmodel/PMET/T2M/T2M_gridcode_",   sprintf("%03d", i),".csv"))
    t2m_i <- round(t(exact_extract(t2m_cr2met, elev_zones_i, "mean", progress = F)),2)
    write.csv(t2m_i, paste0("MS1 Results/TUWmodel/CR2MET/T2M/T2M_gridcode_", sprintf("%03d", i),".csv"))
    t2m_i <- round(t(exact_extract(t2m_mswx, elev_zones_i, "mean", progress = F)),2)
    write.csv(t2m_i, paste0("MS1 Results/TUWmodel/MSWEP/T2M/T2M_gridcode_",  sprintf("%03d", i),".csv"))
    t2m_i <- round(t(exact_extract(t2m_w5e5, elev_zones_i, "mean", progress = F)),2)
    write.csv(t2m_i, paste0("MS1 Results/TUWmodel/W5E5/T2M/T2M_gridcode_",  sprintf("%03d", i),".csv"))    
    print("t2m")
    
    # Daily precipitation
    pp_i <- round(t(exact_extract(pp_pmet, elev_zones_i, "mean", progress = F)),  1)
    write.csv(pp_i, paste0("MS1 Results/TUWmodel/PMET/PP/PP_gridcode_",   sprintf("%03d", i),".csv"))
    pp_i <- round(t(exact_extract(pp_cr2met, elev_zones_i, "mean", progress = F)),1)
    write.csv(pp_i, paste0("MS1 Results/TUWmodel/CR2MET/PP/PP_gridcode_", sprintf("%03d", i),".csv"))
    pp_i <- round(t(exact_extract(pp_mswep, elev_zones_i, "mean", progress = F)), 1)
    write.csv(pp_i, paste0("MS1 Results/TUWmodel/MSWEP/PP/PP_gridcode_",  sprintf("%03d", i),".csv"))
    pp_i <- round(t(exact_extract(pp_era5d, elev_zones_i, "mean", progress = F)), 1)
    write.csv(pp_i, paste0("MS1 Results/TUWmodel/ERA5/PP/PP_gridcode_",   sprintf("%03d", i),".csv"))
    pp_i <- round(t(exact_extract(pp_w5e5, elev_zones_i, "mean", progress = F)), 1)
    write.csv(pp_i, paste0("MS1 Results/TUWmodel/W5E5/PP/PP_gridcode_",   sprintf("%03d", i),".csv"))
    print("pp")    
    print(i)
    
    } else {
      print (i)
    }
  }

rownames(data_area) <- basin_data$gauge_id
write.csv(data_area, "MS1 Results/TUWmodel/data_area.csv", row.names = TRUE)
