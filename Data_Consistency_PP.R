# Code for basin delimitation  -------------------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("zoo")
library("terra")
library("reddPrec")
library("changepoint.np")
source("TimeResample.R")

setwd("/home/rooda/Dropbox/Patagonia/")
check = FALSE # do you need to visually check the time series?

# import raw data
pp_meta <- read.csv("Data/Precipitation/PP_PMETobs_v10_metadata_raw.csv")
pp_data <- read.csv("Data/Precipitation/PP_PMETobs_v10d_raw.csv")
pp_data <- zoo(pp_data[,-1], order.by = as.Date(pp_data$Date))
coredata(pp_data)[pp_data < 0]   <- NA 
pp_raw  <- pp_data # for the final comparison

# check names and sort by latitude
sum(colnames(pp_data) == pp_meta$gauge_id) == nrow(pp_meta)
pp_meta <- pp_meta[order(-pp_meta$gauge_lat),] 
pp_data <- pp_data[, pp_meta$gauge_id]

# check gauge_alt
pp_shape <- vect(pp_meta, geom=c("gauge_lon", "gauge_lat"), crs="epsg:4326")
dem_hr    <- rast("GIS South/dem_patagonia1.tif")
dem_hr    <- terra::extract(dem_hr, pp_shape, method='simple')$dem_patagonia1
dem_hr[is.na(dem_hr)] <- 0 # NA in the oceans (islands)

# replace altitude (consistent with location)
cat("something is wrong in: ", pp_meta[abs(dem_hr-pp_shape$gauge_alt) > 100,]$gauge_name)
pp_meta$gauge_alt <- dem_hr

# requirements 
pp_meta$length <- as.numeric(colSums(!is.na(pp_data))) # save initial lengths
operation      <- as.numeric(colSums(!is.na(pp_data[(index(pp_data) > "2000-01-01")])))

# more than 4 years and continue in operation during 2000-2020
pp_meta   <- pp_meta[(pp_meta$length > 365*4) & (operation > 365*1),] 
pp_data   <- pp_data[, pp_meta$gauge_id]
n_gauges  <- nrow(pp_meta)
n_values  <- sum(pp_meta$length)

## 1. Quality check step (the complete time series doesnt makes sense) ---------------------------

# visual check of the complete time series
if (check) {
  pp_data_m <- MonthlyResample(pp_data,  20, FUN = sum)
  for (i in colnames(pp_data_m)[200:239]) {
    plot(pp_data_m[,i], main = paste(i, pp_meta[pp_meta$gauge_id == i,]$gauge_name))
    Sys.sleep(0.1)
  }
}

pp_data$X00000269 <- NULL
pp_data$X10410015 <- NULL
pp_data$X10355003 <- NULL
pp_data$X00001869 <- NULL
pp_data$X00001838 <- NULL
pp_data$X00000276 <- NULL
pp_data$X00000274 <- NULL
pp_data$X00000282 <- NULL
pp_data$X11140001 <- NULL
pp_data$X11700001 <- NULL
pp_data$X11541000 <- NULL
pp_data$X11533001 <- NULL
pp_data$X12280002 <- NULL
pp_data$X11706000 <- NULL
pp_data$X11700001 <- NULL
pp_data$X12600001 <- NULL
pp_data$X12582001 <- NULL
pp_data$X12930005 <- NULL
pp_data$X11033001 <- NULL
pp_data$X00002211 <- NULL
pp_data$X11540000 <- NULL
pp_data$X12286002 <- NULL
pp_meta <- subset(pp_meta,  pp_meta$gauge_id %in% colnames(pp_data))

# friendly message and update 
cat((n_values - sum(!is.na(pp_data)))*100/n_values, 
    "% of the total data and", n_gauges- nrow(pp_meta), "stations were removed")
pp_meta$length <- as.numeric(colSums(!is.na(pp_data))) # save length
n_values      <- sum(pp_meta$length)
n_gauges      <- nrow(pp_meta)

## 2. Daily outliers -----------------------------------------------------------------------------

# the obvious outlier 
coredata(pp_data)[pp_data > 250] <- NA

# remove isolate values over one year
for (i in 1:nrow(pp_meta)) { # the loop is necessary (problem with by.column in rollapply)
  pp_flag <- rollapply(pp_data[,i], 365, align = "center", function(x){sum(is.na(x)) > 0.7*length(x)})
  pp_data[,i][pp_flag] <- NA
}

# remove values when sd 0 0.01 over 3 months
for (i in 1:nrow(pp_meta)) {
  pp_flag <- rollapply(pp_data[,i], 90, align = "center", function(x){sd(x) == 0})
  pp_data[,i][pp_flag] <- NA
}

# application of reddPrec package
pp_data_df <- as.matrix(10*pp_data) # reddPrec needs pp in tenths (mm*10)
pp_meta_df <- pp_meta[,c("gauge_id", "gauge_lon", "gauge_lat","gauge_alt")]
colnames(pp_meta_df) <- c("ID", "X", "Y", "ALT") # ALT: altitude, X: Lon in UTM, and Y: Lat in UTM.
pp_meta_df[,c("X", "Y")] <- project(as.matrix(pp_meta_df[,c("X", "Y")]), 
                                    from = "epsg:4326", to = "epsg:32719")

if (check) {
qcPrec(prec = pp_data_df, sts = pp_meta_df, inidate=as.Date('1950-01-01'), 
       enddate=as.Date('2021-12-31'), parallel=TRUE, ncpu=18, printmeta=TRUE, thres=125)
  }

# 1) Suspect data: Observed > 0 & all their 10 NNS == 0; 
# 2) Suspect zero: Observed == 0 & all their 10 NNS > 0; 
# 3) Suspect outlier: Observed is 10 times higher or lower than RV; 
# 4) Suspect wet: Observed == 0, wet probability is over 99%, and predicted pp is over 5 litres  
# 5) Suspect dry: Observed > 5 litres, dry probability is over 99%, and predicted pp is under 0.1 litres.

pp_flag <- list.files(path="meta", pattern="*.txt", full.names = TRUE) 
pp_flag <- lapply(pp_flag, function(x) {read.table(file = x, header = T, sep ="")})
pp_flag <- do.call("rbind", lapply(pp_flag, as.data.frame))
pp_flag$data <- pp_flag$data * 0.1 # back to mm
pp_flag <- pp_flag[!((pp_flag$code == 1) & (pp_flag$data <= 5)),] # modifies original criterion
pp_flag <- pp_flag[!((pp_flag$code == 5) & (pp_flag$data <= 10)),]

for (i in pp_meta$gauge_id) {
  pp_flag_i <- pp_flag[pp_flag$ID == i, ]
  pp_flag_i <- zoo(pp_flag_i$code, order.by = as.Date(pp_flag_i$date))
  if (length(pp_flag_i) > 0) {pp_data[,i][pp_flag_i > 0] <- NA}
}

# friendly message and update 
cat((n_values - sum(!is.na(pp_data)))*100/n_values, 
    "% of the total data and", n_gauges- nrow(pp_meta), "stations were removed")
pp_meta$length <- as.numeric(colSums(!is.na(pp_data))) # save length
n_values      <- sum(pp_meta$length)
n_gauges      <- nrow(pp_meta)

## 3. Monthly residuals outliers -----------------------------------------------------------------

date_m <- seq(from = min(time(pp_data)), to = max(time(pp_data)), by = "month")
pp_data_m <- MonthlyResample(pp_data,  20, FUN = sum)

# reference data (ERA5)
pp_shape <- vect(pp_meta, geom=c("gauge_lon", "gauge_lat"), crs="epsg:4326")

pp_ref  <- rast("Data/Precipitation/PP_ERA5_1959_2021m.nc")
pp_ref  <- t(terra::extract(pp_ref, pp_shape, method='simple'))[-1,] # remove wrong ID
pp_ref  <- zoo(pp_ref, order.by = date_m[date_m >= as.Date("1959-01-01")])
pp_anno <- pp_data_m - pp_ref

pp_anno_mean <- aggregate(pp_anno, strftime(time(pp_anno), "%m"), mean, na.rm = TRUE)
pp_anno_sd   <- aggregate(pp_anno, strftime(time(pp_anno), "%m"), sd,   na.rm = TRUE)
pp_anno_upper <- zoo(pp_anno_mean + 3*pp_anno_sd, order.by = date_m)
pp_anno_lower <- zoo(pp_anno_mean - 3*pp_anno_sd, order.by = date_m)
pp_anno_flag  <- (pp_anno > pp_anno_upper) | (pp_anno < pp_anno_lower)

# delete daily values in those months
pp_anno_daily <- na.locf(merge(pp_anno_flag, zoo(NA, time(pp_data)))) # daily version
for (i in pp_meta$gauge_id) { pp_data[,i][pp_anno_daily[,i]] <- NA }

# friendly message and update 
cat((n_values - sum(!is.na(pp_data)))*100/n_values, 
    "% of the total data and", n_gauges- nrow(pp_meta), "stations were removed")
pp_meta$length <- as.numeric(colSums(!is.na(pp_data))) # save length
pp_meta$n_outliers <- colSums(pp_anno_flag, na.rm = T)
n_values      <- sum(pp_meta$length)
n_gauges      <- nrow(pp_meta)

## 4. Stationary monthly residuals  ---------------------------------------------------------------

pp_data_m <- MonthlyResample(pp_data,  20, FUN = sum)
pp_anno   <- pp_data_m - pp_ref

# initialization
pp_meta$n_changepoints <- NA
for(i in 1:ncol(pp_anno)) {
  pp_flag_i <- as.numeric(na.omit(pp_anno[,i]))
  cp_np_i <- cpt.np(pp_flag_i, 
                  method     = "PELT",   # Pruned Exact Linear Time(PELT) 
                  penalty    = "Manual",  
                  pen.value  = 40, # the value of the penalty when using the Manual penalty option
                  minseglen  = 24, #  no. of observations between changes (no more than 24)
                  nquantiles = 4*log(length(pp_flag_i))) # The number of quantiles to calculate
  pp_meta$n_changepoints[i] <- ncpts(cp_np_i)
}

cat("Number of suspicios stations (at least one changepoint):", sum(pp_meta$n_changepoints > 0))

if (check) {
  for (i in pp_meta$gauge_id) {
    if (pp_meta[pp_meta$gauge_id == i,]$n_changepoints > 0) { # only time-series with problems
      plot(pp_anno[,i],  main = paste(i, pp_meta[pp_meta$gauge_id == i,]$gauge_name))
      }
    }
  }

pp_data$X00085896 <- NULL
pp_data$X12289001 <- NULL
pp_data$X10425001[(index(pp_data$X10425001) < "1980-01-01")] <- NA
pp_data$X00002300[(index(pp_data$X00002300) > "2010-01-01")] <- NA
pp_data$X00001822[(index(pp_data$X00001822) < "1980-01-01")] <- NA
pp_data$X00001813[(index(pp_data$X00001813) < "1998-01-01")] <- NA
pp_data$X00001836[(index(pp_data$X00001836) < "1980-01-01")] <- NA
pp_data$X00001896[(index(pp_data$X00001896) > "2016-01-01")] <- NA
pp_data$X10900001[(index(pp_data$X10900001) > "2018-01-01")] <- NA
pp_data$X00002230[(index(pp_data$X00002230) < "1990-01-01")] <- NA
pp_data$X00002289[(index(pp_data$X00002289) < "2007-01-01")] <- NA
pp_data$X10902002[(index(pp_data$X10902002) > "2017-01-01")] <- NA
pp_data$X10710001[(index(pp_data$X10710001) > "2008-01-01")] <- NA
pp_data$X11023002[(index(pp_data$X11023002) > "2020-01-01")] <- NA
pp_data$X00430004[(index(pp_data$X00430004) > "2016-01-01")] <- NA
pp_data$X11034001[(index(pp_data$X11034001) < "2000-01-01")] <- NA
pp_data$X11342002[(index(pp_data$X11342002) > "2001-01-01")] <- NA
pp_data$X12403000[(index(pp_data$X12403000) > "2020-01-01")] <- NA
pp_data$X12820001[(index(pp_data$X12820001) > "2020-01-01")] <- NA
pp_data$X10430005[(index(pp_data$X10430005) > "2020-01-01")] <- NA
pp_data$X10610001[(index(pp_data$X10610001) > "2020-01-01")] <- NA
pp_data$X00002267[(index(pp_data$X00002267) > "2016-01-01")] <- NA
pp_data$X00002206[(index(pp_data$X00002206) > "2016-01-01")] <- NA
pp_data$X11546001[(index(pp_data$X11546001) < "2013-01-01")] <- NA
pp_data$X11536001[(index(pp_data$X11536001) > "2018-01-01")] <- NA
pp_data$X11532000[(index(pp_data$X11532000) < "2013-01-01")] <- NA
pp_data$X12449002[(index(pp_data$X12449002) > "2020-01-01")] <- NA
pp_data$X12620000[(index(pp_data$X12620000) > "2020-01-01")] <- NA
pp_data$X12454001[(index(pp_data$X12454001) < "2002-01-01")] <- NA
pp_data$X12582002[(index(pp_data$X12582002) > "2008-01-01")] <- NA
pp_data$X11316004[(index(pp_data$X11316004) < "2005-01-01")] <- NA
pp_data$X12293001[(index(pp_data$X12293001) < "2002-01-01")] <- NA
pp_data$X12586005[(index(pp_data$X12586005) < "2011-01-01")] <- NA
pp_data$X12876001[(index(pp_data$X12876001) > "2020-01-01")] <- NA
pp_data$X00001813[(index(pp_data$X00001813) < "2010-01-01")] <- NA
pp_data$X12825002[(index(pp_data$X12825002) > "2015-01-01")] <- NA
pp_data$X00450001[(index(pp_data$X00450001) > "1997-01-01") & (index(pp_data$X00450001) < "2009-01-01")] <- NA
pp_data$X11316003[(index(pp_data$X11316003) > "2010-01-01") & (index(pp_data$X11316003) < "2015-01-01")] <- NA
pp_data$X12806004[(index(pp_data$X12806004) > "2000-01-01") & (index(pp_data$X12806004) < "2008-01-01")] <- NA
pp_data$X00002213[(index(pp_data$X00002213) > "1980-01-01") & (index(pp_data$X00002213) < "1992-01-01")] <- NA
pp_data$X10417001[(index(pp_data$X10417001) > "2002-01-01") & (index(pp_data$X10417001) < "2010-01-01")] <- NA
pp_meta <- subset(pp_meta,  pp_meta$gauge_id %in% colnames(pp_data))

# friendly message and update 
cat((n_values - sum(!is.na(pp_data)))*100/n_values, 
    "% of the total data and", n_gauges- nrow(pp_meta), "stations were removed")
pp_meta$length <- as.numeric(colSums(!is.na(pp_data))) # save length
n_values      <- sum(pp_meta$length)
n_gauges      <- nrow(pp_meta)

## 5. final verification and save the data  -------------------------------------------------------

if (check) {
  pp_data_m <- MonthlyResample(pp_data,  20, FUN = sum)
  pp_raw_m  <- MonthlyResample(pp_raw,  20, FUN = sum)
  for (i in pp_meta$gauge_id) {
    par(mfrow = c(2, 1), oma = c(5,4,0,0) + 0.1, mar = c(0,0,1,1) + 0.1)
    plot(pp_anno[(index(pp_anno[,i]) > "1960-01-01")][,i], ylab = "ref_anno", axes = F,
         main = paste(i, pp_meta[pp_meta$gauge_id == i,]$gauge_name))
    plot(pp_raw_m[(index(pp_raw_m[,i]) > "1960-01-01")][,i])
    lines(pp_data_m[(index(pp_data_m[,i]) > "1960-01-01")][,i], col = 3)
  }
}

# resampling
pp_data_m <- MonthlyResample(pp_data,  20, FUN = sum)

# save data (daily and monthly)
pp_data_d <- cbind(Date = index(pp_data),   as.data.frame(pp_data))
pp_data_m <- cbind(Date = index(pp_data_m), as.data.frame(pp_data_m))

write.csv(pp_data_d, "Data/Precipitation/PP_PMETobs_v10d.csv", row.names = FALSE, na = "")
write.csv(pp_data_m, "Data/Precipitation/PP_PMETobs_v10m.csv", row.names = FALSE, na = "")
write.csv(pp_meta, "Data/Precipitation/PP_PMETobs_v10_metadata.csv", row.names = FALSE)

