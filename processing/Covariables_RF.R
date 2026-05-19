# Code for spatial regression predictors ---------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2023)


rm(list=ls())
cat("\014")  

library("terra")

setwd("/home/rooda/Dropbox/Patagonia/")

# Sample dataset
sample <- rast(extent = ext(c(-75.75, -68.25, -55.75, -40.5)), crs = "+init=epsg:4326", resolution = 0.05)

# Elevation from NASADEM 90m
dem <- rast("GIS South/dem_patagonia3f.tif")
dem[is.na(dem)] <- 0
dem <- resample(dem, sample)
#dem[dem <= 0] <- NA
writeRaster(dem, "GIS South/dem_patagonia005.tif", overwrite=TRUE)

# Aspect from NASADEM 90m
aspect  <- terrain(dem, v='aspect', unit='degrees')
writeRaster(aspect, "GIS South/aspect_dem005.tif", overwrite=TRUE)

# Distance to coast from NASADEM 90m
distance <- rast("GIS South/dem_patagonia3f.tif")
distance <- resample(distance, sample)
distance[is.na(distance)] <- -1
distance[distance > 0] <- 1
distance[distance == 1] <- NA
distance <- distance(distance)
distance <- log10(distance+1)
writeRaster(distance, "GIS South/dist_coast005l.tif", overwrite=TRUE)

# West gradient from NASADEM 90m
gradient <- rast("GIS South/west_gradient_raw.tif")
gradient <- resample(gradient, sample)
writeRaster(gradient, "GIS South/west_gradient005.tif", overwrite=TRUE)

# Cloud cover from Wilson et al. (2016)
clouds <- rast("GIS South/clouds_raw.tif")*0.01
clouds <- resample(clouds, sample)
writeRaster(clouds, "GIS South/clouds_005.tif", overwrite=TRUE)

# Average temperature from max and min 
tavg <- (rast("Data/Temperature/Tmin_ERA5_hr_1980_2020.tif") + rast("Data/Temperature/Tmin_ERA5_hr_1980_2020.tif"))/2
tavg <- resample(tavg, sample)
writeRaster(tavg, "Data/Temperature/T2M_ERA5_hr_1980_2020.tif", overwrite=TRUE)

# Average precipitation 
pp <- rast("Data/Precipitation/PP_ERA5_hr_1980_2020.tif")
writeRaster(pp, "Data/Precipitation/PP_ERA5_hr_1980_2020.tif", overwrite=TRUE)

# Aridity index
pet <- rast("Data/Evapotranspiration/PET_ERA5_hr_1980_2020d.nc")
pet <- mean(tapp(pet,   strftime(time(pet), format="%Y"), fun = sum, na.rm = TRUE))
ai  <- pp/pet
writeRaster(ai, "GIS South/aridity_index_ERA5_hr_005.tif", overwrite=TRUE)

