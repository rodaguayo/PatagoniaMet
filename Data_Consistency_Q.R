# Code for streamflow quality control--------------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2023)

rm(list=ls())
cat("\014")  

library("zoo")
source("TimeResample.R")

setwd("/home/rooda/Dropbox/Patagonia/")
check = FALSE # do you need to visually check the time series?

q_meta <- read.csv("Data/Streamflow/Q_PMETobs_v10_metadata_raw.csv")
q_data <- read.csv("Data/Streamflow/Q_PMETobs_v10d_raw.csv")
q_data <- zoo(q_data[,-1], order.by = as.Date(q_data$Date))
q_raw  <- q_data # for the final comparison

q_meta$length <- as.numeric(colSums(!is.na(q_data))) # save length
n_gauges      <- nrow(q_meta)
n_values      <- sum(q_meta$length)

## 1. Quality check step (the complete time series doesnt makes sense) ---------------------------

# visual check of the complete time series
if (check) {
  for (i in 1:nrow(q_meta)) {
    plot(q_data[,i], ylab = paste(q_meta[i,1], q_meta[i,3]))
    Sys.sleep(3)
  }
}

q_data$X10322003 <- NULL # Rio_Gol_Gol_En_Puente_Nº_2
q_data$X00001815 <- NULL # Foyel_Confluencia_Manso (might be fixed?)
q_data$X00002315 <- NULL # Puelo_Hito_Fronterizo
q_data$X00002230 <- NULL # Rio_Cohihues_ PN_Los_Alerces
q_data$X00002218 <- NULL # Rio_Fontana_Estancia_Amacay
q_data$X00002239 <- NULL # Rio_Bagglits_Ruta_a_Chile
q_data$X00002825 <- NULL # Rio_Los_Antiguos_RP_43
q_data$X00002822 <- NULL # Rio_Mitre_RP_11
q_data$X12660001 <- NULL # Rio Ci-Aike Antes Frontera
q_data$X12861001 <- NULL # Rio Cullen En Frontera
q_data$X12291001 <- NULL # Rio Prat En Desembocadura
q_data$X00002828 <- NULL # Rio_Vizcachas_Cerro_Palique
q_data$X11536001 <- NULL # Rio_Cochrane_En_Cochrane
q_data$X11521001 <- NULL # Rio_El_Bagno_En_Chile_Chico
q_meta <- subset(q_meta,  q_meta$gauge_id %in% colnames(q_data))

# friendly message and update 
cat((n_values - sum(!is.na(q_data)))*100/n_values, 
    "% of the total data and", n_gauges- nrow(q_meta), "stations were removed")
q_meta$length <- as.numeric(colSums(!is.na(q_data))) # save length
n_values      <- sum(q_meta$length)
n_gauges      <- nrow(q_meta)

## 2. Daily outliers -----------------------------------------------------------------------------

# specific daily outliers
coredata(q_data)[q_data < 0]     <- NA  # negative values
coredata(q_data)[q_data >= 9999] <- NA  # very large values
coredata(q_data)[q_data == 0]    <- NA  # zero values (only 4 time series with problems)

# remove isolate values over one year
for (i in 1:nrow(q_meta)) { # the loop is necessary (problem with by.column in rollapply)
  q_flag <- rollapply(q_data[,i], 365, align = "center", function(x){sum(is.na(x)) > 0.8*length(x)})
  q_data[,i][q_flag] <- NA
}

# remove values when cv < 0.01 over one month
for (i in 1:nrow(q_meta)) {
  q_flag <- rollapply(q_data[,i], 30, align = "center", function(x){(sd(x)/mean(x)) <= 0.01})
  q_data[,i][q_flag] <- NA
}

# friendly message and update 
cat((n_values - sum(!is.na(q_data)))*100/n_values, 
    "% of the total data and", n_gauges- nrow(q_meta), "stations were removed")
q_meta$length <- as.numeric(colSums(!is.na(q_data))) # save length
n_values      <- sum(q_meta$length)
n_gauges      <- nrow(q_meta)

### last check
q_data$X00001812[(index(q_data$X00001812) > "1997-01-01") & (index(q_data$X00001812) < "2000-01-01")] <- NA
q_data$X10411002[(index(q_data$X10411002) > "2000-01-01") & (index(q_data$X10411002) < "2003-01-01")] <- NA
q_data$X11143002[(index(q_data$X11143002) > "2016-01-01") & (index(q_data$X11143002) < "2017-01-01")] <- NA
q_data$X00002288[(index(q_data$X00002288) > "2006-01-01") & (index(q_data$X00002288) < "2008-01-01")] <- NA
q_data$X00002821[(index(q_data$X00002821) > "2000-01-01") & (index(q_data$X00002821) < "2001-01-01")] <- NA
q_data$X12452001[(index(q_data$X12452001) > "2015-01-01") & (index(q_data$X12452001) < "2016-01-01")] <- NA
q_data$X11310002[(index(q_data$X11310002) > "1990-01-01") & (index(q_data$X11310002) < "1995-01-01")] <- NA
q_data$X00002818[(index(q_data$X00002818) > "2015-01-01")] <- NA
q_data$X12289002[(index(q_data$X12289002) > "2020-01-01")] <- NA # problem after 2020
q_data$X12876001[(index(q_data$X12876001) > "2020-01-01")] <- NA
q_data$X12585001[(index(q_data$X12585001) > "2020-01-01")] <- NA
q_data$X11711000[q_data$X11711000 < 100] <- NA # a few very low/high values
q_data$X11545000[q_data$X11545000 < 250] <- NA 
q_data$X11530000[q_data$X11530000 < 200] <- NA 
q_data$X11307001[q_data$X11307001 > 170] <- NA 
q_data$X12284007[q_data$X12284007 > 200] <- NA 

# do you want to check the results? 
if (check) {
  for (i in q_meta$gauge_id) {
    plot(q_raw[(index(q_raw[,i]) > "1980-01-01")][,i],  
         main = paste(i, q_meta[q_meta$gauge_id == i,]$gauge_name))
    lines(q_data[(index(q_data[,i]) > "1980-01-01")][,i], col = 3)
    }
  }

# friendly message and update 
cat((n_values - sum(!is.na(q_data)))*100/n_values, 
    "% of the total data and", n_gauges- nrow(q_meta), "stations were removed")
q_meta$length <- as.numeric(colSums(!is.na(q_data))) # save length
n_values      <- sum(q_meta$length)
n_gauges      <- nrow(q_meta)

# resampling
q_data_m <- MonthlyResample(q_data,  20, FUN = mean)
q_data_a <- AnnualResample(q_data_m, 10, FUN = mean)

# save data (daily, monthly and annually)
q_data_d <- cbind(Date = index(q_data), as.data.frame(q_data))
q_data_m <- cbind(Date = index(q_data_m), as.data.frame(q_data_m))
q_data_a <- cbind(Date = index(q_data_a), as.data.frame(q_data_a))

write.csv(q_data_d, "Data/Streamflow/Q_PMETobs_v10d.csv", row.names = FALSE, na = "")
write.csv(q_data_m, "Data/Streamflow/Q_PMETobs_v10m.csv", row.names = FALSE, na = "")
write.csv(q_data_a, "Data/Streamflow/Q_PMETobs_v10a.csv", row.names = FALSE, na = "")
write.csv(q_meta, "Data/Streamflow/Q_PMETobs_v10_metadata.csv", row.names = FALSE)
