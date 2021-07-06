rm(list=ls())
cat("\014")  

library("TUWmodel")
library("hydroGOF")
library("readxl")
library("raster")

model<-list.dirs("C:/Users/rooda/Downloads/output", recursive = FALSE)
i=1

gauge_id<-list.dirs(paste0(model[i],"/Figures/Calibration/"), full.names = FALSE)[2:56]

KGE_df <-list.files(paste0(model[i],"/KGE"), pattern="*.txt", full.names = TRUE) 
KGE_df <- lapply(KGE_df, function(x) {read.csv(file = x, header = T)})
KGE_df <- do.call("rbind", lapply(KGE_df, as.data.frame)) 
KGE_df$gauge_id <- gauge_id

median(KGE_df$KGE_calibration)
median(KGE_df$KGE_validation_1)

PARAMS_df <-list.files(paste0(model[i],"/Parameters"), pattern="*.txt", full.names = TRUE) 
PARAMS_df <- lapply(PARAMS_df, function(x) {read.csv(file = x, header = T)})
PARAMS_df <- do.call("rbind", lapply(PARAMS_df, as.data.frame)) 
PARAMS_df$gauge_id <- gauge_id

model<-list.files(paste0(model[i],"/Model"), pattern="*.rds", full.names = TRUE) 
model<-readRDS(model[i])
