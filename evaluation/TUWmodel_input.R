# TUWmodel inputs ---------------------------------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2023)

rm(list=ls())
cat("\014")  

library("foreach")
library("doParallel")

source("elevationZones.R")
library("exactextractr")
library("terra")
library("sf")

terra::gdalCache(20000)
setwd("/home/rooda/Dropbox/Patagonia/")
period  <- c(as.POSIXct("1987-01-01", tz = "UTC"), as.POSIXct("2019-12-31", tz = "UTC"))

# 1. Elevation bands for each basin ---------------------------------------------------------------
dem <- rast("GIS South/dem_patagonia3f.tif")
dem <- aggregate(dem, fact=5, fun="mean")
dem <- disagg(dem, fact=2, method = "bilinear") # avoid bug in elevation bands

basin_data <- read.csv("Data/Streamflow/Q_PMETobs_v10_metadata.csv")
basins_int <- vect("GIS South/Basins_PMETobs_v10.shp")
basins_int <- project(basins_int, crs(dem))
basins_int$area_km2 <- expanse(basins_int, unit="km")

data_area <- data.frame(matrix(0, length(basins_int), 7), row.names = names(basins_int$gauge_id))
colnames(data_area) <- c("area", "nbands",	"n1",	"n2",	"n3",	"n4",	"n5")
basins_iter <- (1:length(basins_int))[basin_data$Modeled == 1]

for(i in basins_iter) {
  
  elev_zones_i   <- elevationZones(x=basins_int[i,], dem=dem, max.zones = 5, min.elevZ = 300)
  data_area[i,1] <- basins_int[i,]$area_km2
  data_area[i,2] <- length(elev_zones_i$area)
  data_area[i,3:((length(elev_zones_i$area))+2)] <- elev_zones_i$area
  elev_zones_i <- as.polygons(elev_zones_i$zonesRaster, na.rm = TRUE, dissolve = TRUE)
  
  path_i <- paste0("MS1 Results/TUWmodel/BASINS/", basins_int[i,]$gauge_id, ".shp")
  writeVector(elev_zones_i, path_i, overwrite = T)

}

rownames(data_area) <- basin_data$gauge_id
write.csv(data_area, "MS1 Results/TUWmodel/data_area.csv", row.names = TRUE)



# 2. Reference ------------------------------------------------------------------------------------

models = c("PMET", "CR2MET", "MSWEP", "ERA5", "W5E5")
basins_list <- list.files("MS1 Results/TUWmodel/BASINS", pattern = ".shp", full.names = T)

paths <- list(PMET   = c("Data/Precipitation/PP_PMETsim_1980_2020_v10d.nc", 
                         "Data/Temperature/Tavg_PMET_1980_2020d.nc", 
                         "Data/Evapotranspiration/Ep_PMET_1980_2020d.nc"),
              
              CR2MET = c("Data/Precipitation/PP_CR2MET_1960_2021d.nc", 
                         "Data/Temperature/Tavg_CR2MET_1960_2021d.nc", 
                         "Data/Evapotranspiration/Ep_CR2MET_1980_2020d.nc"),
              
              MSWEP  = c("Data/Precipitation/PP_MSWEPv28_1979_2020d.nc", 
                         "Data/Temperature/Tavg_MSWX_1979_2019d.nc", 
                         "Data/Evapotranspiration/Ep_MSWX_1980_2020d.nc"),
              
              ERA5  = c("Data/Precipitation/PP_ERA5_hr_1980_2020d.nc", 
                         "Data/Temperature/Tavg_ERA5_hr_1980_2020d.nc", 
                         "Data/Evapotranspiration/Ep_ERA5_hr_1980_2020d.nc"),
              
              W5E5  = c("Data/Precipitation/PP_W5E5_1979_2019d.nc", 
                         "Data/Temperature/Tavg_W5E5_1979_2019d.nc", 
                         "Data/Evapotranspiration/Ep_W5E5_1980_2019d.nc"))

for (model in models) {
  
  cl <- makeCluster(20) 
  registerDoParallel(cl)
  
  climate <- foreach(gauge_id = basins_list, .packages = c("terra", "exactextractr", "sf")) %dopar% {
    setwd("/home/rooda/Dropbox/Patagonia/")
    elev_zones_i <- st_read(gauge_id)
    
    # PP, PET and T2M: PMET v1.0
    pp  <- rast(paths[[model]][1])
    t2m <- rast(paths[[model]][2])
    pet <- rast(paths[[model]][3])
    terra::time(pp)  <- as.POSIXct(time(pp))
    terra::time(t2m) <- as.POSIXct(time(t2m))
    terra::time(pet) <- as.POSIXct(time(pet))
    pp   <- pp[[time(pp)    >= period[1] & time(pp)   <= period[2]]]
    t2m  <- t2m[[time(t2m)  >= period[1] & time(t2m)  <= period[2]]]
    pet  <- pet[[time(pet)  >= period[1] & time(pet)  <= period[2]]]
    
    pp_i <- round(t(exact_extract(pp, elev_zones_i, "mean", progress = F)),   2)
    t2m_i <- round(t(exact_extract(t2m, elev_zones_i, "mean", progress = F)),  2)
    pet_i <- round(t(exact_extract(pet, elev_zones_i, "mean", progress = F)),  2)
    climate <- list(PP = pp_i , T2M = t2m_i, PET = pet_i)
  }
  
  stopCluster(cl)
  
  names(climate) <- tools::file_path_sans_ext(basename(basins_list))
  lapply(seq_along(climate), function(x) {
    write.csv(climate[[x]]$PP,  paste0("MS1 Results/TUWmodel/", model, "/PP/PP_", names(climate)[x], ".csv"))
    write.csv(climate[[x]]$T2M, paste0("MS1 Results/TUWmodel/", model, "/T2M/T2M_", names(climate)[x], ".csv"))
    write.csv(climate[[x]]$PET, paste0("MS1 Results/TUWmodel/", model, "/PET/PET_", names(climate)[x], ".csv"))
    })
  print(model)
}


# 3. Cross-validation -----------------------------------------------------------------------------

for (cv in 1:10) {

  cl <- makeCluster(20) 
  registerDoParallel(cl)
  
  basins_list <- list.files("MS1 Results/TUWmodel/BASINS", pattern = ".shp", full.names = T)
  start <- Sys.time()
  
  climate <- foreach(gauge_id = basins_list, .packages = c("terra", "exactextractr", "sf")) %dopar% {
      
    setwd("/home/rooda/Dropbox/Patagonia/")
    elev_zones_i <- st_read(gauge_id)
  
    # PP, PET and T2M: PMET v1.0
    pp  <- rast(paste0("/home/rooda/PMET_results/Precipitation/PP_PMETsim_1980_2020d_", cv, ".nc"))
    t2m <- rast(paste0("/home/rooda/PMET_results/Temperature/Tavg_PMETsim_1980_2020d_", cv, ".nc"))
    pet <- rast(paste0("/home/rooda/PMET_results/Evapotranspiration/Ep_PMET_1980_2020d_", cv, ".nc"))
    terra::time(pp)  <- as.POSIXct(time(pp))
    terra::time(t2m) <- as.POSIXct(time(t2m))
    terra::time(pet) <- as.POSIXct(time(pet))
    pp   <- pp[[time(pp)    >= period[1] & time(pp)   <= period[2]]]
    t2m  <- t2m[[time(t2m)  >= period[1] & time(t2m)  <= period[2]]]
    pet  <- pet[[time(pet)  >= period[1] & time(pet)  <= period[2]]]
    
    pp_i <- round(t(exact_extract(pp, elev_zones_i, "mean", progress = F)),   2)
    t2m_i <- round(t(exact_extract(t2m, elev_zones_i, "mean", progress = F)),  2)
    pet_i <- round(t(exact_extract(pet, elev_zones_i, "mean", progress = F)),  2)
    climate <- list(PP = pp_i , T2M = t2m_i, PET = pet_i)
  }
  
  stopCluster(cl)
  print( Sys.time() - start )
  
  names(climate) <- tools::file_path_sans_ext(basename(basins_list))
  
  lapply(seq_along(climate), function(x) {
    write.csv(climate[[x]]$PP,  paste0("MS1 Results/TUWmodel/PMET_CV",cv,"/PP/PP_", names(climate)[x], ".csv"))
    write.csv(climate[[x]]$T2M, paste0("MS1 Results/TUWmodel/PMET_CV",cv,"/T2M/T2M_", names(climate)[x], ".csv"))
    write.csv(climate[[x]]$PET, paste0("MS1 Results/TUWmodel/PMET_CV",cv,"/PET/PET_", names(climate)[x], ".csv"))
  })

}















