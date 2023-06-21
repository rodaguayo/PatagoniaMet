# Code for extracting attributes ------------------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2023)

rm(list=ls())
cat("\014")  

setwd("/home/rooda/Dropbox/Patagonia/")
library("exactextractr")
library("hydromad")
library("terra")
library("zoo")
library("sf")
terra::gdalCache(15000)

# Data of polygons (and streamflow data) already merged.
period        <- c(as.POSIXct("1979-12-31"), as.POSIXct("2019/12/31"))
basin_data    <- read.csv("Data/Streamflow/Q_PMETobs_v10_metadata.csv")
basin_shp     <- st_read("GIS South/Basins_PMET_v10.shp")
basin_shp_int <- st_read("GIS South/Basins_PMET_v10_int.shp") 

# Area
basin_data$total_area <- round(expanse(vect(basin_shp),     unit="km"), 2)
basin_data$int_area   <- round(expanse(vect(basin_shp_int), unit="km"), 2)

# Topographic attributes
dem   <- rast("GIS South/dem_patagonia3f.tif")
slope <- terrain(dem, v='aspect', unit='degrees') 
basin_data$elev_mean   <- round(exact_extract(dem, basin_shp,   "mean"), 1)
basin_data$elev_median <- round(exact_extract(dem, basin_shp,   "median"), 1)
basin_data$slope_mean  <- round(exact_extract(slope, basin_shp, "median"), 1)

# Land cover
forest_cover <- rast("GIS South/lc_forest_500m.tif")
basin_data$forest_cover <- round(exact_extract(forest_cover, basin_shp, "mean"), 1)

lake_cover   <- rast("GIS South/lc_water_500m.tif")
basin_data$lake_cover   <- round(exact_extract(lake_cover,   basin_shp, "mean"), 3)

lai_data   <- rast("GIS South/LAI_climatology_1981-2015.nc4")
lai_data   <- round(exact_extract(lai_data, basin_shp, "mean"), 3)
basin_data$lai_max  <- apply(lai_data, 1, max, na.rm=TRUE)
basin_data$lai_diff <- apply(lai_data, 1, max, na.rm=TRUE) - apply(lai_data, 1, min, na.rm=TRUE)

#gvf_data   <- round(exact_extract(gvf_data, basin_shp, "mean"), 3)
#basin_data$gvf_max  <- apply(gvf_data, 1, max, na.rm=TRUE)
#basin_data$gvf_diff <- apply(gvf_data, 1, max, na.rm=TRUE) - apply(lai_data, 1, min, na.rm=TRUE)

# Glacier area (this might be improve with rasterization)
glaciers  <- vect("GIS South/Glaciers/RGI6.shp")
glaciers  <- subset(glaciers, glaciers$CenLat <= -40)
glaciers  <- rasterize(glaciers, forest_cover, background = 0) * 100
basin_data$glacier_cover <- round(exact_extract(glaciers,   basin_shp, "mean"), 1)

# Glacier change (elevation in m y-1)
dh_dt <- rast("GIS South/Glaciers/dhdt_2021.tif")
dh_dt <- project(dh_dt, crs(basin_shp))
dh_dt <- mask(dh_dt, vect(basin_shp), overwrite=TRUE)
dh_dt <- crop(dh_dt, vect(basin_shp))*1000 # from m to mm
dh_dt <- dh_dt/1.091 # from ice to water
dh_dt[is.na(dh_dt)] <- 0

basin_data$glacier_dhdt <- round(exact_extract(dh_dt, basin_shp, "mean"), 1)

# Streamflow for each basin
basin_q <- read.csv("Data/Streamflow/Q_PMETobs_v10a.csv")
basin_q <- subset(basin_q, Date >= "1980-01-01")[,-1] # Only years with >= 10 months
basin_data$Q_m3_s <- round(colMeans(basin_q, na.rm = TRUE), 2)
basin_data$Q_m3_s[colSums(basin_q > 0, na.rm = T)/nrow(basin_q) <= 0.20] <- NA
basin_data$Q_m3_s[basin_data$dam == 1] <- NA # remove data for basins with dams

basin_codes <- as.numeric(substr(basin_data$gauge_code,0,2))

for (i in min(basin_codes):max(basin_codes)) { # if NA?
  basin_data_i <- subset(basin_data, substr(basin_data$gauge_code,1,2) == i)
  
  for (j in 1:nrow(basin_data_i)) {
    code     <- basin_data_i[j,]$gauge_code
    code_pot <- code*10 + seq(1,9)
    
    if (sum(basin_data_i$gauge_code %in% code_pot) >= 1) {
      q_minus   <- sum(basin_data_i$Q_m3_s[basin_data_i$gauge_code %in% code_pot])
      basin_data$Qint_m3_s[match(code, basin_data$gauge_code)] <- basin_data_i$Q_m3_s[j] - q_minus
      
      } else {
        basin_data$Qint_m3_s[match(code, basin_data$gauge_code)] <- basin_data_i$Q_m3_s[j]
      }
    }
}

basin_data$Qint_mm_y <- (basin_data$Qint_m3_s*1e3*365*86400) / (basin_data$int_area*1e6)
basin_data$Qint_mm_y <- round(basin_data$Qint_mm_y, 1)

basin_q  <- read.csv("Data/Streamflow/Q_PMETobs_v10m.csv")
basin_q  <- subset(basin_q, Date > as.POSIXct("1989-12-31") & Date < as.POSIXct("2005-12-31"))[,-1]
basin_q  <- colSums(!is.na(basin_q))/nrow(basin_q)
basin_data$Modeled <- as.numeric(basin_q > 2/3)
basin_data$Modeled[basin_data$mam == 1]       <- 0 # Remove basins with dams
basin_data$Modeled[is.na(basin_data$BF_PMET)] <- 0 # Remove high pp factor values

# Climate indices

# p_mean for all alternatives
pp_stacks <- list(p_mean_ERA5   = rast("Data/Precipitation/PP_ERA5_hr_1980_2020m.nc"),
                  p_mean_MSWEP  = rast("Data/Precipitation/PP_MSWEPv28_1979_2020m.nc"),
                  p_mean_CR2MET = rast("Data/Precipitation/PP_CR2MET_1960_2021m.nc"),
                  p_mean_W5E5   = rast("Data/Precipitation/PP_W5E5_1979_2019m.nc"),
                  p_mean_PMET   = rast("Data/Precipitation/PP_PMET_1980_2020m.nc"))

for (i in 1:length(pp_stacks)) {
  pp_stack <- pp_stacks[[i]]
  terra::time(pp_stack) <- as.POSIXct(time(pp_stack), tz= "UTC") 
  pp_stack <- subset(pp_stack, which(time(pp_stack) > period[1] & time(pp_stack) <= period[2]))
  pp_stack <- mean(tapp(pp_stack, strftime(time(pp_stack),format="%Y"), fun = sum, na.rm = TRUE))
  basin_data[names(pp_stacks)[[i]]] <- round(exact_extract(pp_stack, basin_shp, "mean"), 0)
  print(names(pp_stacks)[[i]])
}

# pet_mean from PMET
pet_stack <- rast("Data/Evapotranspiration/Ep_PMET_1980_2020d.nc")
terra::time(pet_stack) <- as.POSIXct(time(pet_stack), tz= "UTC") 
pet_stack <- subset(pet_stack, which(time(pet_stack) > period[1] & time(pet_stack) <= period[2]))
pet_stack <- mean(tapp(pet_stack, strftime(time(pet_stack),format="%Y"), fun = sum, na.rm = TRUE))
basin_data$pet_mean_PMET <- round(exact_extract(pet_stack, basin_shp, "mean"), 0)

# pet_mean from GLEAM
pet_stack <- rast("Data/Evapotranspiration/Ep_GLEAM36a_1980_2021m.nc")
terra::time(pet_stack) <- as.POSIXct(time(pet_stack), tz= "UTC") 
pet_stack <- subset(pet_stack, which(time(pet_stack) > period[1] & time(pet_stack) <= period[2]))
pet_stack <- mean(tapp(pet_stack, strftime(time(pet_stack),format="%Y"), fun = sum, na.rm = TRUE))
basin_data$pet_mean_GLEAM <- round(exact_extract(pet_stack, basin_shp, "mean"), 0)

# aridity 
basin_data$aridity_PMET<- round(basin_data$p_mean_PMET/basin_data$pet_mean_PMET, 3)

# several precipitation metrics
pp_stack <- rast("Data/Precipitation/PP_PMET_1980_2020d.nc")
time_pp <- time(pp_stack)
pp_stack <- t(exact_extract(pp_stack, basin_shp, "mean"))
row.names(pp_stack) <- as.character(time_pp)
colnames(pp_stack)  <- basin_shp$Name
pp_stack <- zoo(pp_stack,  order.by = time_pp)

# several precipitation metrics
t2m_stack <- rast("Data/Temperature/Tavg_PMET_1980_2020d.nc")
time_t2m  <- time(t2m_stack)
t2m_stack <- t(exact_extract(t2m_stack, basin_shp, "mean"))
row.names(t2m_stack) <- as.character(time_t2m)
colnames(t2m_stack)  <- basin_shp$Name
t2m_stack <- zoo(t2m_stack,  order.by = time_t2m)

for (i in 1:ncol(pp_stack)) {
  metric_i <- eventseq(pp_stack[,i], thresh = 5*mean(pp_stack[,i]))
  metric_i <- eventinfo(pp_stack[,i], metric_i)
  
  # high_prec_freq (≥ 5 times mean daily precipitation) in days yr−1 
  basin_data$high_prec_freq_PMET[i] <- mean(aggregate(metric_i$Duration, by = list(metric_i$Year), FUN = sum)$x)
  
  # high_prec_dur (number of consecutive days ≥ 5 times mean daily pp)
  basin_data$high_prec_dur_PMET[i] <- mean(metric_i$Duration)
  
  metric_i <- eventseq(pp_stack[,i], thresh = 1, below = TRUE)
  metric_i <- eventinfo(pp_stack[,i], metric_i)
  
  # low_prec_freq (frequency of dry days (< 1 mm day−1) )
  basin_data$low_prec_freq_PMET[i] <- mean(aggregate(metric_i$Duration, by = list(metric_i$Year), FUN = sum)$x)
  
  # low_prec_dur (frequency of dry days (< 1 mm day−1) )
  basin_data$low_prec_dur_PMET[i] <- mean(metric_i$Duration)
  print(i)
}

# p_frac_snow (days colder than 0◦C)
pp_total <- colMeans(aggregate(pp_stack, by = list(substr(row.names(pp_stack),1,4)), FUN = sum))
pp_stack <- as.data.frame(pp_stack)
pp_stack[as.data.frame(t2m_stack) > 0] <- 0
pp_snow  <- aggregate(pp_stack, by = list(substr(row.names(pp_stack),1,4)), FUN = sum)
pp_snow  <- colMeans(pp_snow[,-1])
basin_data$frac_snow_PMET     <- round(pp_snow/pp_total, 3)

# TODO: p_seasonality Eq. (14) in Woods et al. (2009)
#basin_data$p_seasonality_PMET <- round(exact_extract(pet_stack, basin_shp, "mean"), 3)

# Water balance indexes for Chile
pp_stack  <- rast("Data/Precipitation/PP_WB_DGA_1985_2015.tif")
pet_stack <- rast("Data/Evapotranspiration/Ep_WB_DGA_1985_2015.tif")
et_stack  <- rast("Data/Evapotranspiration/E_WB_DGA_1985_2015.tif")
basin_data$PP_BH  <- round(exact_extract(pp_stack,  basin_shp, "mean"), 0)
basin_data$PET_BH <- round(exact_extract(pet_stack, basin_shp, "mean"), 0)
basin_data$ET_BH  <- round(exact_extract(et_stack,  basin_shp, "mean"), 0)

write.csv(basin_data, "Data/Streamflow/Q_PMETobs_v10_metadata.csv", row.names = FALSE)
