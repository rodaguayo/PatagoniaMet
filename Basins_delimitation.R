# Code for basin delimitation  -------------------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("rgrass")
library("terra")
setwd("/home/rooda/Dropbox/Patagonia/")

# 1. Data: Station locations and DEM -------------------------------------------------------------
dem        <- rast("GIS South/dem_patagonia1.tif")
dem        <- aggregate(dem, fact=2, fun="mean")  # 32Gb of RAM is not enough
depre      <- is.na(dem) # depression DEM -> water/ocean -> use less RAM
q_location <- read.csv("Data/Streamflow/Metadata_Streamflow_v10.csv")
q_vect <- vect(q_location, geom=c("Longitude", "Latitude"), crs="epsg:4326")


# 2. Data: Initializate GRASS and files ----------------------------------------------------------
initGRASS(gisBase = "/usr/lib/grass80/", home="/home/rooda/Dropbox/Patagonia/", 
                 SG = dem, override = TRUE) 

write_RAST(dem,    vname = "dem_grass",   flags = c("overwrite", "o"))
write_RAST(depre,  vname = "depre_grass", flags = c("overwrite", "o"))
write_VECT(q_vect, vname = "q_grass",     flags = c("overwrite", "o"))

# sink removal using Lindsay et al. (2005) (really necessary?)
execGRASS("r.hydrodem", flags=c("overwrite"),
          parameters=list(input="dem_grass", depression = "depre_grass", output = "demf_grass"))


# 3. Flow direction, accumulation and stream from multiple flow direction (MFD) ------------------
execGRASS("r.watershed", flags=c("overwrite", "b"), 
          parameters = list(elevation="dem_grass", depression = "depre_grass", threshold=500, 
                            drainage= "fdir", stream="upstream", accumulation="accum", basin="basins"))

execGRASS("r.stream.snap", flags=c("overwrite"), # 25 -> 25*25m = 0.5 km
          parameters=list(input="q_grass", output = "q_grass_snap", stream_rast="upstream", radius = 20))

q_location <- geom(read_VECT("q_grass_snap"),  df=TRUE) # update location


# 4. Delimitate all basins based of stream gauge location (intersection file) -------------------
execGRASS("r.stream.basins", flags=c("overwrite"),
          parameters=list(direction="fdir", points = "q_grass_snap", basins="basins"))
rast_rbasin83 <- as.polygons(read_RAST("basins"))
writeVector(rast_rbasin83, "GIS South/Basins_Patagonia83_int.shp", overwrite=TRUE)


# 5. Delimitate all basins based of stream gauge location (1 polygon per basin) ------------------
for (i in 78:nrow(q_location)) {

  coords <- c(q_location$x[i], q_location$y[i])
  execGRASS("r.water.outlet", flags=c("overwrite"),
            parameters=list(input="fdir", output = "basin_i", coordinates = coords))
  rast_basin_i <- read_RAST("basin_i")
  rast_basin_i <- as.polygons(rast_basin_i)
  writeVector(rast_basin_i, paste0("GIS South/Basins/Basin_",   sprintf("%03d", i),".shp"), overwrite = TRUE)
  print(i)
}

rast_rbasin83 <- list.files(path="GIS South/Basins/", pattern = "shp$", full.names = TRUE)
vect_basins   <- vect(rast_rbasin83[1], crs = "epsg:4326")

for(i in 2:length(rast_rbasin83)){
  vect_basins_i <- vect(rast_rbasin83[i], crs = "epsg:4326")
  vect_basins   <- rbind(vect_basins, vect_basins_i)
  print(i)
}

vect_basins$ID           <- paste0("Basin_", seq(1, length(rast_rbasin83)))
vect_basins$Name         <- q_location$Name
vect_basins$Institution  <- q_location$Institution
vect_basins$Area_km2     <- round(expanse(vect_basins, unit="km"), 2)
vect_basins <- vect_basins[,c("ID", "Name", "Institution", "Area_km2")] # Delete file1a9158 etc....
writeVector(vect_basins, "GIS South/Basins_Patagonia83.shp", overwrite=TRUE)


