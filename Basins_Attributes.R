# Code for extracting attributes
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

setwd("/home/rooda/Dropbox/Patagonia/")
library("exactextractr")
library("terra")
library("sf")

# Data of polygons (and streamflow data) already merged.
period        <- c(as.POSIXct("1979-12-31"), as.POSIXct("2019/12/31"))
basin_data    <- read.csv("Data/Streamflow/Metadata_Streamflow_v10.csv")
basin_shp     <- st_read("GIS South/Basins_Patagonia83.shp")
basin_shp_int <- st_read("GIS South/Basins_Patagonia83_int.shp") 

# Area
basin_data$total_area_km2  <- round(expanse(vect(basin_shp),     unit="km"), 2)
basin_data$int_area_km2    <- round(expanse(vect(basin_shp_int), unit="km"), 2)

# Topographic attributes
dem   <- rast("GIS South/dem_patagonia3f.tif")
slope <- terrain(dem, v='aspect', unit='degrees') 
basin_data$mean_elevation   <- round(exact_extract(dem, basin_shp, "mean"), 1)
basin_data$median_elevation <- round(exact_extract(dem, basin_shp, "median"), 1)
basin_data$slope            <- round(exact_extract(slope, basin_shp, "median"), 1)

# Land cover
forest_cover <- rast("GIS South/lc_forest_500m.tif")
lake_cover   <- rast("GIS South/lc_water_500m.tif")
basin_data$forest_cover <- round(exact_extract(forest_cover, basin_shp, "mean"), 1)
basin_data$lake_cover   <- round(exact_extract(lake_cover,   basin_shp, "mean"), 1)

# Glacier area (this might be improve with rasterization)
glaciers  <- vect("GIS South/Glaciers/RGI6.shp")
glaciers  <- subset(glaciers, glaciers$CenLat <= -40)
glaciers  <- rasterize(glaciers, forest_cover, background = 0) * 100
basin_data$glacier_cover <- round(exact_extract(glaciers,   basin_shp, "mean"), 1)

# Glacier change (elevation in m y-1)
dh_dt <- vrt(list.files("GIS South/Glaciers/dhdt", full.names = TRUE))
dh_dt <- project(dh_dt, crs(basin_shp))
dh_dt <- mask(dh_dt, vect(basin_shp_int), overwrite=TRUE)
dh_dt <- crop(dh_dt, vect(basin_shp_int))*1000 # from m to mm
dh_dt <- dh_dt/1.091 # from ice to water
dh_dt[is.na(dh_dt)] <- 0

basin_data$glacier_dhdt <- round(exact_extract(dh_dt, basin_shp_int, "mean"), 1)

# Streamflow for each basin
basin_q <- read.csv("Data/Streamflow/Data_Streamflow_v10_annual.csv")
basin_q <- subset(basin_q, Date >= "1980-01-01")[,-1] # Only years with more than 9 months
basin_data$Q_m3_s <- round(colMeans(basin_q[sapply(basin_q, is.numeric)], na.rm = TRUE), 2)
basin_data$Q_m3_s[colSums(basin_q > 0, na.rm = T)/40 <= 0.25] <- NA
basin_data$Q_m3_s[basin_data$Dam == 1] <- NA # remove data for basins with dams

for (i in 10:35) {
  basin_data_i <- subset(basin_data, substr(basin_data$Code,1,2) == i)
  
  for (j in 1:nrow(basin_data_i)) {
    code     <- basin_data_i[j,]$Code
    code_pot <- code*10 + seq(1,9)
    
    if (sum(basin_data_i$Code %in% code_pot) >= 1) {
      q_minus   <- sum(basin_data_i$Q_m3_s[basin_data_i$Code %in% code_pot])
      basin_data$Qint_m3_s[match(code, basin_data$Code)] <- basin_data_i$Q_m3_s[j] - q_minus
      
      } else {
        basin_data$Qint_m3_s[match(code, basin_data$Code)] <-    basin_data_i$Q_m3_s[j]
      }
    }
}

basin_data$Qint_mm_y <- (basin_data$Qint_m3_s*1e03*365*86400) / (basin_data$int_area_km2*1e6)
basin_data$Qint_mm_y <- round(basin_data$Qint_mm_y, 1)

basin_q  <- read.csv("Data/Streamflow/Data_Streamflow_v10_monthly.csv")
basin_q  <- subset(basin_q, Date > as.POSIXct("1989-12-31") & Date < as.POSIXct("2005-12-31"))[,-1]
basin_q  <- colSums(!is.na(basin_q))/nrow(basin_q)
basin_data$Modeled <- as.numeric(basin_q > 2/3)
basin_data$Modeled[basin_data$Dam == 1]       <- 0 # Remove basins with dams
basin_data$Modeled[is.na(basin_data$BF_PMET)] <- 0 # Remove high pp factor values

# Precipitation datasets
pp_stacks <- list(PP_ERA5   = rast("Data/Precipitation/PP_ERA5_hr_1980_2020m.nc"),
                  PP_MSWEP  = rast("Data/Precipitation/PP_MSWEPv28_1979_2020m.nc"),
                  PP_CR2MET = rast("Data/Precipitation/PP_CR2MET_1979_2020m.nc"),
                  PP_PMET   = rast("Data/Precipitation/PP_PMET_1980_2020m.nc"))

for (i in 1:length(pp_stacks)) {
  pp_stack <- pp_stacks[[i]]
  time(pp_stack) <- as.POSIXct(time(pp_stack), tz= "UTC") 
  pp_stack <- subset(pp_stack, which(time(pp_stack) > period[1] & time(pp_stack) <= period[2]))
  pp_stack <- mean(tapp(pp_stack, strftime(time(pp_stack),format="%Y"), fun = sum, na.rm = TRUE))
  basin_data[names(pp_stacks)[[i]]] <- round(exact_extract(pp_stack, basin_shp_int, "mean"), 0)
  print(names(pp_stacks)[[i]])
}

# Potential evapotranspiration datasets
pet_stack <- rast("Data/Evapotranspiration/PET_GLEAM36a_1980_2021m.nc")
time(pet_stack) <- as.POSIXct(time(pet_stack), tz= "UTC") 
pet_stack <- subset(pet_stack, which(time(pet_stack) > period[1] & time(pet_stack) <= period[2]))
pet_stack <- mean(tapp(pet_stack, strftime(time(pet_stack),format="%Y"), fun = sum, na.rm = TRUE))
basin_data$PET_PMET <- round(exact_extract(pet_stack, basin_shp_int, "mean"), 0)

# Climate indexes
pet_stack<-resample(pet_stack, pp_stack)
basin_data$AI <- round(exact_extract(pp_stack/pet_stack, basin_shp_int, "mean"), 2)

# Water balance indexes for Chile
pp_stack  <- rast("Data/Precipitation/PP_WB_DGA_1985_2015.tif")
pet_stack <- rast("Data/Evapotranspiration/PET_WB_DGA_1985_2015.tif")
et_stack  <- rast("Data/Evapotranspiration/ET_WB_DGA_1985_2015.tif")
basin_data$PP_BH  <- round(exact_extract(pp_stack,  basin_shp, "mean"), 0)
basin_data$PET_BH <- round(exact_extract(pet_stack, basin_shp, "mean"), 0)
basin_data$ET_BH  <- round(exact_extract(et_stack,  basin_shp, "mean"), 0)

write.csv(basin_data, "Data/Streamflow/Metadata_Streamflow_v10.csv", row.names = FALSE)
