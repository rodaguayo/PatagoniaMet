rm(list=ls())
cat("\014")  

library("raster")
library("readxl")
library("stats")

#Estimation of "w" from National Water Balance
basins <- shapefile("C:/Users/rooda/Dropbox/Patagonia/GIS South/Basins_Patagonia83.shp")

q_bh <- read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/streamflow/Data_streamflow_v10.xlsx", sheet = "budyko")
q_data <- read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/streamflow/Data_streamflow_v10.xlsx", sheet = "info")
PET_PP <- as.numeric(q_bh$`PET/PP`)
ET_PP  <- as.numeric(q_bh$`ET/PP`)
Ri     <- as.numeric(q_data$R_int_mm_year)
PETi   <- as.numeric(q_data$pet_gleam)

w_inf <- function(PET_PP, ET_PP, w) 1 + PET_PP - ET_PP - (1+((PET_PP)^w))^(1/w)
vec_w <- numeric(0)

#Only basins in Chile (79 of 83)
for (j in 1:length(q_bh$Name)) {
  vec_w[j] <- uniroot(w_inf, interval=c(0, 10), tol = 0.001, PET_PP=PET_PP[j], ET_PP=ET_PP[j])$root
}
write.csv(vec_w, "w_estimation.csv")


#Estimation of long-term "real" precipitation
pp_inf <- function(PP, R, PET, w) 1 + PET/PP - ((PP-R)/PP) - (1+((PET/PP)^w))^(1/w)
w      <- round(median(vec_w),1)
vec_pp <- numeric(0)
  
for (j in 1:length(basins)) {
    if (is.na(Ri[j])){
      vec_pp[j]<-NA
      print(j)
      
    } else {
      pp <- uniroot(pp_inf, interval=c(10, 15000), tol = 0.000001, R=Ri[j], PET=PETi[j], w=w)
      vec_pp[j] <- pp$root
      print(j)
    }
  }

write.csv(round(vec_pp,1), "C:/Users/rooda/Dropbox/Rstudio/PP_REAL_1990_2019.csv")

