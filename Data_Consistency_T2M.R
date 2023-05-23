# Code for basin delimitation  -------------------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("zoo")
library("terra")
library("changepoint.np")
source("TimeResample.R")

setwd("/home/rooda/Dropbox/Patagonia/")
check = FALSE # do you need to visually check the time series?

# import raw data
t2m_meta  <- read.csv("Data/Temperature/Tavg_PMETobs_v10_metadata_raw.csv")
tmax_data <- read.csv("Data/Temperature/Tmax_PMETobs_v10d_raw.csv")
tmax_data <- zoo(tmax_data[,-1], order.by = as.Date(tmax_data$Date))
tmax_raw  <- tmax_data # for the final comparison

tmin_data <- read.csv("Data/Temperature/Tmin_PMETobs_v10d_raw.csv")
tmin_data <- zoo(tmin_data[,-1], order.by = as.Date(tmin_data$Date))
tmin_raw  <- tmin_data # for the final comparison

# check names and sort by latitude
sum(colnames(tmax_data) == colnames(tmin_data)) == nrow(t2m_meta)
sum(colnames(tmax_data) == t2m_meta$gauge_id) == nrow(t2m_meta)
sum(colnames(tmin_data) == t2m_meta$gauge_id) == nrow(t2m_meta)
t2m_meta <- t2m_meta[order(-t2m_meta$gauge_lat),] 
tmax_data <- tmax_data[, t2m_meta$gauge_id]
tmin_data <- tmin_data[, t2m_meta$gauge_id]

# check gauge_alt
t2m_shape <- vect(t2m_meta, geom=c("gauge_lon", "gauge_lat"), crs="epsg:4326")
dem_hr    <- rast("GIS South/dem_patagonia1.tif")
dem_hr    <- terra::extract(dem_hr, t2m_shape, method='simple')$dem_patagonia1
dem_hr[is.na(dem_hr)] <- 0 # NA in the oceans (islands)

# replace altitude (consistent with location)
cat("something is wrong in: ", t2m_meta[abs(dem_hr-t2m_shape$gauge_alt) > 100,]$gauge_name)
t2m_meta$gauge_alt <- dem_hr

# Tavg needs Tmax and Tmin (check consistency between Tmax and Tmin (is the code ok?))
delta <- as.numeric(colSums(!is.na(tmax_data))) - as.numeric(colSums(!is.na(tmin_data)))
delta <- round((delta/as.numeric(colSums(!is.na(tmax_data))))*100,2) # values larger than 10% ?
coredata(tmax_data)[is.na(tmin_data) | is.na(tmax_data)] <- NA
coredata(tmin_data)[is.na(tmin_data) | is.na(tmax_data)] <- NA

# requirements 
t2m_meta$length <- as.numeric(colSums(!is.na(tmax_data)))   
operation       <- as.numeric(colSums(!is.na(tmax_data[(index(tmax_data) > "2000-01-01")])))

# more than 4 years and continue in operation during 2000-2020
t2m_meta  <- t2m_meta[(t2m_meta$length > 365*4) & (operation > 365*1),] 
tmax_data <- tmax_data[, t2m_meta$gauge_id]
tmin_data <- tmin_data[, t2m_meta$gauge_id]
n_gauges  <- nrow(t2m_meta)
n_values  <- sum(t2m_meta$length)

## 1. Quality check step (the complete time series doesnt makes sense) ---------------------------

# visual check of the complete time series
if (check) {
  tmax_data_m <- MonthlyResample(tmax_data,  20, FUN = mean)
  tmin_data_m <- MonthlyResample(tmin_data,  20, FUN = mean)
  for (i in colnames(tmax_data)[100:143]) {
    plot(tmax_data_m[,i], ylim = c(-5, 25), main =  paste(i, t2m_meta[t2m_meta$gauge_id == i,]$gauge_name))
    lines(tmin_data_m[,i], col = 3)
    }
  }

tmax_data$X10425001 <- NULL 
tmax_data$X12125001 <- NULL 
tmax_data$X12000000 <- NULL 
tmax_data$X11540000 <- NULL 
tmax_data$X11440000 <- NULL 
t2m_meta <- subset(t2m_meta,  t2m_meta$gauge_id %in% colnames(tmax_data))
tmin_data <- tmin_data[, t2m_meta$gauge_id]

# friendly message and update 
cat((n_values - sum(!is.na(tmax_data)))*100/n_values, 
    "% of the total data and", n_gauges- nrow(t2m_meta), "stations were removed")
t2m_meta$length <- as.numeric(colSums(!is.na(tmax_data))) # save length
n_values      <- sum(t2m_meta$length)
n_gauges      <- nrow(t2m_meta)

## 2. Daily outliers -----------------------------------------------------------------------------

# Tmax > Tmin  (or >=?)
coredata(tmax_data)[tmax_data < tmin_data] <- NA
coredata(tmin_data)[tmax_data < tmin_data] <- NA

# value >< mean +- 3sd
date_m <- seq(from = min(time(tmax_data)), to = max(time(tmax_data)), by = "month")
tmax_mean <- aggregate(tmax_data,  strftime(time(tmax_data), "%m"), FUN = mean, na.rm = TRUE)
tmin_mean <- aggregate(tmin_data,  strftime(time(tmin_data), "%m"), FUN = mean, na.rm = TRUE)
tmax_sd   <- aggregate(tmax_data,  strftime(time(tmax_data), "%m"), FUN = sd,   na.rm = TRUE)
tmin_sd   <- aggregate(tmin_data,  strftime(time(tmin_data), "%m"), FUN = sd,   na.rm = TRUE)

tmax_upper <- zoo(tmax_mean + 3*tmax_sd, order.by = date_m)
tmax_upper <- na.locf(merge(tmax_upper, zoo(NA, time(tmax_data))))
tmax_lower <- zoo(tmax_mean - 3*tmax_sd, order.by = date_m)
tmax_lower <- na.locf(merge(tmax_lower,zoo(NA, time(tmax_data))))
tmin_upper <- zoo(tmin_mean + 3*tmin_sd, order.by = date_m)
tmin_upper <- na.locf(merge(tmin_upper, zoo(NA, time(tmin_data))))
tmin_lower <- zoo(tmin_mean - 3*tmin_sd, order.by = date_m)
tmin_lower <- na.locf(merge(tmin_lower, zoo(NA, time(tmin_data))))

# remove last column
tmax_upper <- tmax_upper[,-ncol(tmax_upper)]
tmax_lower <- tmax_lower[,-ncol(tmax_lower)]
tmin_upper <- tmin_upper[,-ncol(tmin_upper)]
tmin_lower <- tmin_lower[,-ncol(tmin_lower)]

coredata(tmax_data)[(tmax_data > tmax_upper) | (tmax_data < tmax_lower)] <- NA
coredata(tmin_data)[(tmin_data > tmin_upper) | (tmin_data < tmin_lower)] <- NA

# last check: Tavg needs Tmax and Tmin
coredata(tmax_data)[is.na(tmin_data) | is.na(tmax_data)] <- NA
coredata(tmin_data)[is.na(tmin_data) | is.na(tmax_data)] <- NA

# friendly message and update 
cat((n_values - sum(!is.na(tmax_data)))*100/n_values, 
    "% of the total data and", n_gauges- nrow(t2m_meta), "stations were removed")
t2m_meta$length <- as.numeric(colSums(!is.na(tmax_data))) # save length
n_values      <- sum(t2m_meta$length)
n_gauges      <- nrow(t2m_meta)

## 3. Monthly residuals outliers -----------------------------------------------------------------

tmax_data_m <- MonthlyResample(tmax_data, 20, FUN = mean)
tmin_data_m <- MonthlyResample(tmin_data, 20, FUN = mean)

# reference data (ERA5): Tmax and Tmin
t2m_shape <- vect(t2m_meta, geom=c("gauge_lon", "gauge_lat"), crs="epsg:4326")
tmax_ref <- rast("Data/Temperature/Tmax_ERA5_1959_2021m.nc")
tmax_ref <- t(terra::extract(tmax_ref, t2m_shape, method='simple'))[-1,] # remove wrong ID
tmax_ref <- zoo(tmax_ref, order.by = date_m[date_m >= as.Date("1959-01-01")])
tmax_anno <- tmax_data_m - tmax_ref

tmax_anno_mean <- aggregate(tmax_anno, strftime(time(tmax_anno), "%m"), mean, na.rm = TRUE)
tmax_anno_sd   <- aggregate(tmax_anno, strftime(time(tmax_anno), "%m"), sd,   na.rm = TRUE)
tmax_anno_upper <- zoo(tmax_anno_mean + 3*tmax_anno_sd, order.by = date_m)
tmax_anno_lower <- zoo(tmax_anno_mean - 3*tmax_anno_sd, order.by = date_m)
tmax_anno_flag  <- (tmax_anno > tmax_anno_upper) | (tmax_anno < tmax_anno_lower)

tmin_ref <- rast("Data/Temperature/Tmin_ERA5_1959_2021m.nc")
tmin_ref <- t(terra::extract(tmin_ref, t2m_shape, method='simple'))[-1,] # remove wrong ID
tmin_ref <- zoo(tmin_ref, order.by = date_m[date_m >= as.Date("1959-01-01")])
tmin_anno <- tmin_data_m - tmin_ref

tmin_anno_mean <- aggregate(tmin_anno, strftime(time(tmin_anno), "%m"), mean, na.rm = TRUE)
tmin_anno_sd   <- aggregate(tmin_anno, strftime(time(tmin_anno), "%m"), sd,   na.rm = TRUE)
tmin_anno_upper <- zoo(tmin_anno_mean + 3*tmin_anno_sd, order.by = date_m)
tmin_anno_lower <- zoo(tmin_anno_mean - 3*tmin_anno_sd, order.by = date_m)
tmin_anno_flag  <- (tmin_anno > tmin_anno_upper) | (tmin_anno < tmin_anno_lower)

# delete daily values in those months
tmax_anno_daily <- na.locf(merge(tmax_anno_flag, zoo(NA, time(tmax_data)))) 
tmin_anno_daily <- na.locf(merge(tmin_anno_flag, zoo(NA, time(tmin_data)))) 

for (i in t2m_meta$gauge_id) { 
  tmax_data[,i][tmax_anno_daily[,i]] <- NA 
  tmin_data[,i][tmin_anno_daily[,i]] <- NA 
  }

# check: Tavg needs Tmax and Tmin
coredata(tmax_data)[is.na(tmin_data) | is.na(tmax_data)] <- NA
coredata(tmin_data)[is.na(tmin_data) | is.na(tmax_data)] <- NA

# friendly message and update 
cat((n_values - sum(!is.na(tmax_data)))*100/n_values, 
    "% of the total data and", n_gauges- nrow(t2m_meta), "stations were removed")
t2m_meta$length <- as.numeric(colSums(!is.na(tmax_data))) # save length
t2m_meta$n_outliers <- colSums(tmax_anno_flag, na.rm = T) + colSums(tmin_anno_flag, na.rm = T)
n_values      <- sum(t2m_meta$length)
n_gauges      <- nrow(t2m_meta)

## 4. Monthly residuals stationary ---------------------------------------------------------------

tmax_data_m <- MonthlyResample(tmax_data, 20, FUN = mean)
tmin_data_m <- MonthlyResample(tmin_data, 20, FUN = mean)
tmax_anno <- tmax_data_m - tmax_ref
tmin_anno <- tmin_data_m - tmin_ref

# initialization
t2m_meta$n_changepoints_tmax <- NA
t2m_meta$n_changepoints_tmin <- NA

for(i in 1:ncol(tmax_anno)) {
  tmax_flag_i <- as.numeric(na.omit(tmax_anno[,i]))
  tmin_flag_i <- as.numeric(na.omit(tmin_anno[,i]))  
  
  cp_np_i <- cpt.np(tmax_flag_i, method = "PELT",   # Pruned Exact Linear Time(PELT) 
                    penalty    = "Manual", # the method that worked best for this  
                    pen.value  = 40, # the value of the penalty when using the Manual penalty option
                    minseglen  = 24, #  no. of observations between changes (no more than 24)
                    nquantiles = 4*log(length(tmax_flag_i))) # The number of quantiles to calculate
  t2m_meta$n_changepoints_tmax[i] <- ncpts(cp_np_i)
  
  cp_np_i <- cpt.np(tmin_flag_i, method = "PELT", penalty = "Manual",  pen.value  = 40, 
                    minseglen  = 24, nquantiles = 4*log(length(tmin_flag_i)))
  t2m_meta$n_changepoints_tmin[i] <- ncpts(cp_np_i)
  }

cat("Number of suspicios stations (at least one changepoint):", 
    sum(((t2m_meta$n_changepoints_tmax > 0) + (t2m_meta$n_changepoints_tmin > 0)) > 0))

# visual check of the flagged time series
if (check) {
  for (i in t2m_meta$gauge_id) {  # only time-series with problems
    if (sum(t2m_meta[t2m_meta$gauge_id == i,][,9:10]) > 0) {
      plot(tmax_anno[,i], main = paste(i, t2m_meta[t2m_meta$gauge_id == i,]$gauge_name),
           ylim = c(min(tmin_anno[,i], na.rm = T), max(tmax_anno[,i], na.rm = T)))
      lines(tmin_anno[,i], col = 3)
    }
  }
}

tmax_data$X00000015 <- NULL 
tmax_data$X10360002 <- NULL 
tmax_data$X00000019 <- NULL 
tmax_data$X00000021 <- NULL 
tmax_data$X00002273 <- NULL 
tmax_data$X00087814 <- NULL 
tmax_data$X11440001 <- NULL 
tmax_data$X12293001 <- NULL 
tmax_data$X00002297 <- NULL 
tmax_data$X12280004[(index(tmax_data$X12280004) < "2010-01-01")] <- NA
tmax_data$X00087774[(index(tmax_data$X00087774) < "1976-01-01")] <- NA
tmax_data$X00001806[(index(tmax_data$X00001806) < "1995-01-01")] <- NA
tmax_data$X00002206[(index(tmax_data$X00002206) < "2009-01-01")] <- NA
tmax_data$X00000023[(index(tmax_data$X00000023) < "2011-01-01")] <- NA
tmax_data$X00002204[(index(tmax_data$X00002204) < "2005-01-01")] <- NA
tmax_data$X00002202[(index(tmax_data$X00002202) < "2009-01-01")] <- NA
tmax_data$X00002207[(index(tmax_data$X00002207) < "2002-01-01")] <- NA
tmax_data$X00002270[(index(tmax_data$X00002270) < "1999-01-01")] <- NA
tmax_data$X11304001[(index(tmax_data$X11304001) > "2010-01-01")] <- NA
tmax_data$X11316004[(index(tmax_data$X11316004) < "2013-01-01")] <- NA
tmax_data$X11317005[(index(tmax_data$X11317005) < "2010-01-01")] <- NA
tmax_data$X00002269[(index(tmax_data$X00002269) < "1998-01-01")] <- NA
tmax_data$X11523001[(index(tmax_data$X11523001) < "2000-01-01")] <- NA
tmax_data$X11541000[(index(tmax_data$X11541000) < "2017-01-01")] <- NA
tmax_data$X12283001[(index(tmax_data$X12283001) < "2000-01-01")] <- NA
tmax_data$X12286001[(index(tmax_data$X12286001) < "1980-01-01")] <- NA
tmax_data$X12622002[(index(tmax_data$X12622002) < "2015-01-01")] <- NA
tmax_data$X12449001[(index(tmax_data$X12449001) < "2005-01-01")] <- NA
tmax_data$X12863001[(index(tmax_data$X12863001) < "2000-01-01")] <- NA
tmax_data$X12873001[(index(tmax_data$X12873001) < "2015-01-01")] <- NA
tmax_data$X12876003[(index(tmax_data$X12876003) < "2010-01-01")] <- NA
tmax_data$X12930001[(index(tmax_data$X12930001) < "2014-01-01")] <- NA
tmax_data$X12809001[(index(tmax_data$X12809001) > "2005-01-01") & (index(tmax_data$X12809001) < "2015-01-01")] <- NA

# replicate NAs
t2m_meta <- subset(t2m_meta,  t2m_meta$gauge_id %in% colnames(tmax_data))
tmin_data <- tmin_data[, t2m_meta$gauge_id]
coredata(tmin_data)[is.na(tmin_data) | is.na(tmax_data)] <- NA

# friendly message and update 
cat((n_values - sum(!is.na(tmax_data)))*100/n_values, 
    "% of the total data and", n_gauges- nrow(t2m_meta), "stations were removed")
t2m_meta$length <- as.numeric(colSums(!is.na(tmax_data))) # save length
n_values      <- sum(t2m_meta$length)
n_gauges      <- nrow(t2m_meta)

## 5. final verification and save the data  -------------------------------------------------------

if (check) {
  tmax_data_m <- MonthlyResample(tmax_data,  20, FUN = mean)
  tmax_raw_m  <- MonthlyResample(tmax_raw,   20, FUN = mean)
  
  tmin_data_m <- MonthlyResample(tmin_data,  20, FUN = mean)
  tmin_raw_m  <- MonthlyResample(tmin_raw,   20, FUN = mean)  
  
  for (i in t2m_meta$gauge_id[100:130]) {
    par(mfrow = c(3, 1), oma = c(5,4,0,0) + 0.1, mar = c(0,0,1,1) + 0.1)
    
    # upper plot
    plot(tmax_anno[(index(tmax_anno[,i]) > "1960-01-01")][,i], ylab = "ref_anno", axes = F,
         main = paste(i, t2m_meta[t2m_meta$gauge_id == i,]$gauge_name))
    lines(tmin_anno[(index(tmin_anno[,i]) > "1960-01-01")][,i], axes = F, col = 3)
    
    # middle plot
    plot(tmin_raw_m[(index(tmin_raw_m[,i]) > "1960-01-01")][,i], axes = F)
    lines(tmin_data_m[(index(tmin_data_m[,i]) > "1960-01-01")][,i],col = 2)
    
    # lower plot
    plot(tmax_raw_m[(index(tmax_raw_m[,i]) > "1960-01-01")][,i])
    lines(tmax_data_m[(index(tmax_data_m[,i]) > "1960-01-01")][,i], col = 2)
  }
}

tavg_data <- (tmax_data+tmin_data)/2

# resampling
tmax_data_m <- MonthlyResample(tmax_data,  20, FUN = mean)
tmin_data_m <- MonthlyResample(tmin_data,  20, FUN = mean)
tavg_data_m <- MonthlyResample(tavg_data,  20, FUN = mean)

# save data (daily and monthly)
tmax_data_d <- cbind(Date = index(tmax_data),   as.data.frame(tmax_data))
tmin_data_d <- cbind(Date = index(tmin_data),   as.data.frame(tmin_data))
tavg_data_d <- cbind(Date = index(tavg_data),   as.data.frame(tavg_data))
tmax_data_m <- cbind(Date = index(tmax_data_m), as.data.frame(tmax_data_m))
tmin_data_m <- cbind(Date = index(tmin_data_m), as.data.frame(tmin_data_m))
tavg_data_m <- cbind(Date = index(tavg_data_m), as.data.frame(tavg_data_m))

write.csv(tavg_data_d, "Data/Temperature/Tavg_PMETobs_v10d.csv", row.names = FALSE, na = "")
write.csv(tavg_data_m, "Data/Temperature/Tavg_PMETobs_v10m.csv", row.names = FALSE, na = "")
write.csv(tmax_data_d, "Data/Temperature/Tmax_PMETobs_v10d.csv", row.names = FALSE, na = "")
write.csv(tmax_data_m, "Data/Temperature/Tmax_PMETobs_v10m.csv", row.names = FALSE, na = "")
write.csv(tmin_data_d, "Data/Temperature/Tmin_PMETobs_v10d.csv", row.names = FALSE, na = "")
write.csv(tmin_data_m, "Data/Temperature/Tmin_PMETobs_v10m.csv", row.names = FALSE, na = "")
write.csv(t2m_meta, "Data/Temperature/Tavg_PMETobs_v10_metadata.csv", row.names = FALSE)
