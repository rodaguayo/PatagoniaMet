# Code for basin delimitation  -------------------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2022)

rm(list=ls())
cat("\014")  

library("rgrass")
library("terra")
setwd("/home/rooda/Dropbox/Patagonia/")

# Data: Station locations and DEM (Initializate GRASS and files)--------------------------------
dem        <- rast("GIS South/dem_patagonia1.tif")
dem        <- aggregate(dem, fact=2, fun="mean")  # 32Gb of RAM is not enough
depre      <- is.na(dem) # depression DEM -> water/ocean -> use less RAM
q_location <- read.csv("Data/Streamflow/Metadata_Streamflow_v10.csv")
q_vect <- vect(q_location, geom=c("Longitude", "Latitude"), crs="epsg:4326")

initGRASS(gisBase = "/usr/lib/grass80/", home="/home/rooda/Dropbox/Patagonia/", 
                 SG = dem, override = TRUE) 

write_RAST(dem,    vname = "dem_grass",   flags = c("overwrite", "o"))
write_RAST(depre,  vname = "depre_grass", flags = c("overwrite", "o"))
write_VECT(q_vect, vname = "q_grass",     flags = c("overwrite", "o"))

# sink removal using Lindsay et al. (2005) (really necessary?)
#execGRASS("r.hydrodem", flags=c("overwrite"),
#          parameters=list(input="dem_grass", depression = "depre_grass", output = "demf_grass"))

# 1. Delimitate all basins ------------------------------------------------------------------------
execGRASS("r.watershed", flags=c("overwrite", "b"), 
          parameters = list(elevation="dem_grass", depression = "depre_grass", threshold=500, 
                            drainage= "fdir", accumulation="accum"))

execGRASS("r.stream.extract", flags=c("overwrite"), 
          parameters = list(elevation="dem_grass", depression = "depre_grass", threshold=10000, 
                            accumulation= "accum", stream_vector="stream_v", stream_raster="stream_r"))

execGRASS("r.stream.basins", flags=c("overwrite", "l"),
          parameters=list(direction="fdir", stream_rast = "stream_r", basins="basins"))

all_basins      <- read_RAST("basins")
all_basins      <- as.polygons(all_basins)
all_basins$area <- round(expanse(all_basins, unit="km"), 2)
writeVector(all_basins, "GIS South/Basins_Patagonia_all.shp", overwrite=TRUE)

# 2. Delimitate all basins based of stream gauge location (intersection file)  ------------------
execGRASS("r.stream.snap", flags=c("overwrite"), # 20 -> 20*20m = 0.4 km
          parameters=list(input="q_grass", output = "q_grass_snap", stream_rast="stream_r", radius = 20))

execGRASS("r.stream.basins", flags=c("overwrite", "l"),
          parameters=list(direction="fdir", points = "q_grass_snap", basins="basins"))

rast_basins_int              <- read_RAST("basins")
rast_basins_int              <- as.polygons(rast_basins_int)
rast_basins_int$ID           <- seq(1, length(rast_basins_int))
rast_basins_int$Name         <- q_location$Name
rast_basins_int$Institution  <- q_location$Institution
rast_basins_int$Area_km2     <- round(expanse(rast_basins_int, unit="km"), 2)
rast_basins_int <- rast_basins_int[,c("ID", "Name", "Institution", "Area_km2")] # Delete file1a9158 etc....
writeVector(rast_basins_int, "GIS South/Basins_Patagonia83_int.shp", overwrite=TRUE)

# 2. Delimitate all basins based of stream gauge location (1 polygon per basin) ------------------
for (i in 1:nrow(q_location)) { # it uses too much space cause of temporal files

  coords <- c(q_location_geom$x[i], q_location_geom$y[i])
  execGRASS("r.water.outlet", flags=c("overwrite"),
            parameters=list(input="fdir", output = "basin_i", coordinates = coords))
  rast_basin_i <- read_RAST("basin_i")
  rast_basin_i <- as.polygons(rast_basin_i)
  writeVector(rast_basin_i, paste0("GIS South/Basins/Basin_",   sprintf("%03d", i),".shp"), overwrite = TRUE)
  print(i)
}

rast_basins <- list.files(path="GIS South/Basins/", pattern = "shp$", full.names = TRUE)
vect_basins   <- vect(rast_basins[1], crs = "epsg:4326")

for(i in 2:length(rast_rbasin83)){
  vect_basins_i <- vect(rast_basins[i], crs = "epsg:4326")
  vect_basins   <- rbind(vect_basins, vect_basins_i)
  print(i)
}

vect_basins$ID           <- seq(1, length(rast_basins))
vect_basins$Name         <- q_location$Name
vect_basins$Institution  <- q_location$Institution
vect_basins$Area_km2     <- round(expanse(vect_basins, unit="km"), 2)
vect_basins <- vect_basins[,c("ID", "Name", "Institution", "Area_km2")] # Delete file1a9158 etc....
writeVector(vect_basins, "GIS South/Basins_Patagonia83.shp", overwrite=TRUE)
