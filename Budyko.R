rm(list=ls())
cat("\014")  

library("raster")
library("ncdf4")
library("xlsx")
library("readxl")

basins<-shapefile("C:/Users/rooda/Dropbox/Patagonia/GIS South/Basins_Patagonia83.shp")

nc_bh3<-nc_open("E:/Datasets/DGA_BH/BH3/4_Base_de_datos/Archivos_netcdf/1_Historico/regionalizacion_1979_2015.nc")
lon3 <- ncvar_get(nc_bh3,"lon")
lat3 <- ncvar_get(nc_bh3,"lat")

stack_pp3<-stack("E:/Datasets/DGA_BH/BH3/4_Base_de_datos/Archivos_netcdf/1_Historico/regionalizacion_1979_2015.nc", varname = "pr")
stack_pp3<-flip(t(stack_pp3), 'x')
extent(stack_pp3) <- c(min(lon3), max(lon3), min(lat3), max(lat3))
crs(stack_pp3) <- CRS('+init=EPSG:4326')
stack_pp3<-setZ(stack_pp3,seq(as.Date("1979/1/1"), as.Date("2015/12/1"), "month"))
stack_pp3 <- stack_pp3[[which(getZ(stack_pp3) >= as.Date("1984-12-31"))]]
stack_pp3<-mean(stackApply(stack_pp3, indices<-format(stack_pp3@z$time,"%y"), fun=sum))
stack_pp3[stack_pp3 == 0] <- NA

stack_et3<-stack("E:/Datasets/DGA_BH/BH3/4_Base_de_datos/Archivos_netcdf/1_Historico/regionalizacion_1979_2015.nc", varname = "ET")
stack_et3<-flip(t(stack_et3), 'x')
extent(stack_et3) <- c(min(lon3), max(lon3), min(lat3), max(lat3))
crs(stack_et3) <- CRS('+init=EPSG:4326')
stack_et3 <- stack_et3*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
stack_et3<-setZ(stack_et3,seq(as.Date("1979/1/1"), as.Date("2015/12/1"), "month"))
stack_et3 <- stack_et3[[which(getZ(stack_et3) >= as.Date("1984-12-31"))]]
stack_et3<-mean(stackApply(stack_et3, indices<-format(stack_et3@z$time,"%y"), fun=sum))
stack_et3[stack_et3 == 0] <- NA

stack_pet3<-stack("E:/Datasets/DGA_BH/BH3/4_Base_de_datos/Archivos_netcdf/1_Historico/regionalizacion_1979_2015.nc", varname = "PET")
stack_pet3<-flip(t(stack_pet3), 'x')
extent(stack_pet3) <- c(min(lon3), max(lon3), min(lat3), max(lat3))
crs(stack_pet3) <- CRS('+init=EPSG:4326')
stack_pet3 <- stack_pet3*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
stack_pet3<-setZ(stack_pet3,seq(as.Date("1979/1/1"), as.Date("2015/12/1"), "month"))
stack_pet3 <- stack_pet3[[which(getZ(stack_pet3) >= as.Date("1984-12-31"))]]
stack_pet3<-mean(stackApply(stack_pet3, indices<-format(stack_pet3@z$time,"%y"), fun=sum))
stack_pet3[stack_pet3 == 0] <- NA


basins_pp<-extract(stack_pp3,basins,fun=mean,na.rm=TRUE)
basins_pet<-extract(stack_pet3,basins,fun=mean,na.rm=TRUE)
basins_et<-extract(stack_et3,basins,fun=mean,na.rm=TRUE)
rownames(basins_pp)<-basins$gridcode
rownames(basins_pet)<-basins$gridcode
rownames(basins_et)<-basins$gridcode

write.xlsx(cbind(basins$gridcode,basins_pp, basins_pet, basins_et), "eva_aridity_bh3.xlsx")

stack_pp4<- raster("E:/Datasets/DGA_BH/BH4/4_Base_de_datos/Archivos_raster/BH_85-15/Forzantes/1_Historico/pr_Anual_LatLon.tif")
stack_pet4<-raster("E:/Datasets/DGA_BH/BH4/4_Base_de_datos/Archivos_raster/BH_85-15/VIC/1_Historico/pet_Anual_LatLon.tif")
stack_et4<-raster("E:/Datasets/DGA_BH/BH4/4_Base_de_datos/Archivos_raster/BH_85-15/VIC/1_Historico/et_Anual_LatLon.tif")

basins_pp<-extract(stack_pp4,basins,fun=mean,na.rm=TRUE)
basins_pet<-extract(stack_pet4,basins,fun=mean,na.rm=TRUE)
basins_et<-extract(stack_et4,basins,fun=mean,na.rm=TRUE)
rownames(basins_pp)<-basins$gridcode
rownames(basins_pet)<-basins$gridcode
rownames(basins_et)<-basins$gridcode

write.xlsx(cbind(basins$gridcode,basins_pp, basins_pet, basins_et), "eva_aridity_bh4.xlsx")

#Estimation of long-term "real" precipitation
streamflow_info<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/streamflow/Data_streamflow_v10.xlsx", sheet = "info"))
Ri<-as.numeric(streamflow_info$R_int_mm_year)
PETi<-as.numeric(streamflow_info$pet_gleam)

pp_inf <- function(PP, R, PET, w) 1 + PET/PP - ((PP-R)/PP) - (1+((PET/PP)^w))^(1/w)
vec_pp <- numeric(0)
  
for (j in 1:83) {
    if (is.na(Ri[j])){
      vec_pp[j]<-0
      print(j)
      
    } else {
      pp <- uniroot(pp_inf, interval=c(10, 15000), tol = 0.000001, R=Ri[j], PET=PETi[j], w=1.2)
      vec_pp[j] <- pp$root
      print(j)
    }
  }

vec_pp[vec_pp == 0] <- NA
write.csv(round(vec_pp,2), "pp_estimation.csv")


#Estimation of "w" for National Water Balance
streamflow_info<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/streamflow/Data_streamflow_v10.xlsx", sheet = "budyko"))
PET_PP<-as.numeric(streamflow_info$`PET/PP`)
ET_PP<-as.numeric(streamflow_info$`ET/PP`)

w_inf <- function(PET_PP, ET_PP, w) 1 + PET_PP - ET_PP - (1+((PET_PP)^w))^(1/w)
vec_w <- numeric(0)

for (j in 1:79) {
    w <- uniroot(w_inf, interval=c(0, 10), tol = 0.001, PET_PP=PET_PP[j], ET_PP=ET_PP[j])
    vec_w[j] <- w$root
    print(j)
  }

write.csv(vec_w, "w_estimation.csv")
