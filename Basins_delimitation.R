# Code for basin delimitation  -------------------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2023)

rm(list=ls())
cat("\014")  

library("rgrass")
library("terra")
setwd("/home/rooda/Dropbox/Patagonia/")

# Data: DEM (Initializate GRASS and files)--------------------------------
dem        <- rast("GIS South/dem_patagonia1.tif")
dem        <- aggregate(dem, fact=3, fun="mean")  # 32Gb of RAM is not enough
dem        <- crop(dem, ext(-74.5, -67, -55.1, -40)) # xmin, xmax, ymin, ymax
depre      <- is.na(dem) # depression DEM -> water/ocean -> use less RAM

initGRASS(gisBase = "/usr/lib/grass82/", home="/home/rooda/Dropbox/Patagonia/", 
                 SG = dem, override = TRUE) 

write_RAST(dem,    vname = "dem_grass",   flags = c("overwrite", "o"))
write_RAST(depre,  vname = "depre_grass", flags = c("overwrite", "o"))

# sink removal using Lindsay et al. (2005) (really necessary?)
#execGRASS("r.hydrodem", flags=c("overwrite"),
#          parameters=list(input="dem_grass", depression = "depre_grass", output = "demf_grass"))

# 1. Preprocessing ------------------------------------------------------------------------------
execGRASS("r.watershed", flags=c("overwrite", "b"), 
          parameters = list(elevation="dem_grass", depression = "depre_grass", threshold=500, 
                            drainage= "fdir", accumulation="accum"))

execGRASS("r.stream.extract", flags=c("overwrite"), 
          parameters = list(elevation="dem_grass", depression = "depre_grass", threshold=10000, 
                            accumulation= "accum", stream_vector="stream_v", stream_raster="stream_r"))

# 2. Delimitate all basins based of stream gauge location (intersection file)  ------------------

# gauge data
q_location <- read.csv("Data/Streamflow/Q_PMETobs_v10_metadata.csv")[,1:7]
q_vect <- vect(q_location, geom=c("gauge_lon", "gauge_lat"), crs="epsg:4326")
writeVector(q_vect, "GIS South/Basins_PMET_v10_points.shp", overwrite=TRUE)
write_VECT(q_vect, vname = "q_grass",     flags = c("overwrite", "o"))

execGRASS("r.stream.snap", flags=c("overwrite"), 
          parameters=list(input="q_grass", output = "q_grass_snap", stream_rast="stream_r", radius = 50))

execGRASS("r.stream.basins", flags=c("overwrite", "l"),
          parameters=list(direction="fdir", points = "q_grass_snap", basins="basins"))

rast_basins_int              <- read_RAST("basins")
rast_basins_int              <- as.polygons(rast_basins_int)
rast_basins_int$gauge_id     <- q_location$gauge_id
rast_basins_int$gauge_name   <- q_location$gauge_name
rast_basins_int$institution  <- q_location$institution
rast_basins_int$int_area     <- round(expanse(rast_basins_int, unit="km"), 2)

# check
sum(rast_basins_int$int_area > 1) == nrow(q_location)

#### Delete columns file1a9158 etc....
rast_basins_int <- rast_basins_int[,c("gauge_id", "gauge_name", "institution", "int_area")]
writeVector(rast_basins_int, "GIS South/Basins_PMET_v10_int.shp", overwrite=TRUE)

# 3. Delimitate all basins based of stream gauge location (1 polygon per basin) ------------------
q_location_snap  <- crds(read_VECT("q_grass_snap"))
q_location_snap  <- as.data.frame(q_location_snap)

for (i in 90:109) { # it uses too much space cause of temporal files

  coords <- c(q_location_snap$x[i], q_location_snap$y[i])
  execGRASS("r.water.outlet", flags=c("overwrite"),
            parameters=list(input="fdir", output = "basin_i", coordinates = coords))
  rast_basin_i <- read_RAST("basin_i")
  rast_basin_i <- as.polygons(rast_basin_i)
  writeVector(rast_basin_i, paste0("GIS South/Basins/Basin_", sprintf("%03d", i),".shp"), overwrite = TRUE)
  print(i)
}

rast_basins <- list.files(path="GIS South/Basins/", pattern = "shp$", full.names = TRUE)
vect_basins   <- vect(rast_basins[1], crs = "epsg:4326")

for(i in 2:length(rast_basins)){
  vect_basins_i <- vect(rast_basins[i], crs = "epsg:4326")
  vect_basins   <- rbind(vect_basins, vect_basins_i)
  print(i)
}

vect_basins$gauge_id     <- q_location$gauge_id
vect_basins$gauge_name   <- q_location$gauge_name
vect_basins$institution  <- q_location$institution
vect_basins$int_area     <- round(expanse(vect_basins, unit="km"), 2)

# Delete file1a9158 etc....
vect_basins <- vect_basins[,c("gauge_id", "gauge_name", "institution", "int_area")] 
writeVector(vect_basins, "GIS South/Basins_PMET_v10.shp", overwrite=TRUE)
